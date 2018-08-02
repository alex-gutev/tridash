;;;; parser.lisp
;;;;
;;;; Metalink Programming Language.
;;;; Copyright (C) 2018  Alexander Gutev
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :metalink.parser)


(defconstant +bind-operator+ '->)
(defconstant +def-operator+ '\:)

(defconstant +macro-operator+ '|macro|)
(defconstant +op-operator+ '|op|)

(defconstant +list-operator+ 'list)

(defparameter *operator-nodes*
  (alist-hash-table
   '((-> 10 right)
     (\: 5 right)))

  "Hash table of the current registered operators. Each key is an
   operator symbol the corresponding value is a list of two elements
   where the first element is the operator's precedence and the second
   element is the operator's associativity: the symbol LEFT or RIGHT
   for left or right associativity.")

(defparameter *list-delimiter* nil
  "The delimiting token type of the node list currently being parsed,
   a terminate token is inserted before this token. Initially this is
   NIL for end of file.")


;;; Adding operators

(defun add-operator (symbol prec assoc)
  "Adds the symbol SYMBOL as an operator with precedence PREC and
   associativity ASSOC."

  (setf (gethash symbol *operator-nodes*) (list prec assoc)))


;;;; Parsing Functions

;;; A single node declaration is parsed per invocation of the parser
;;; function (returned by make-parser) in order to allow new operators
;;; to be registered for use by the remaining declarations in the
;;; source file.

(defun make-parser (stream)
  "Returns a function which, when called parses a single declaration
   from the input stream. Returns nil when EOF is reached."

  (let ((lex (make-lexer stream)))
    (lambda ()
      (when (has-input? lex)
        (parse-delimited-node lex)))))


(defun has-input? (lex)
  "Returns true if there are any, non :TERMINATE, tokens left in the
   lexer's input stream. :TERMINATE tokens are skipped and removed
   from the lexer's input stream."

  (loop
     for type = (next-token lex :peek t)
     while (eq type :terminate)
     do (next-token lex)
     finally (return type)))

(defun parse-delimited-node (lex)
  "Parses a node followed by a :TERMINATE token."

  (let ((*line-term* t))
    (prog1 (parse-node-expression lex)
      (parse-terminator lex))))


(defun parse-node-expression (lex &key (line-term *line-term*))
  "Parses a node expression, which can either be an atom node or
   functor. The value of the :LINE-TERM argument determines whether
   newlines occurring before the node are treated as declaration
   terminators, if no value is provided it defaults to the value of
   *LINE-TERM*."

  (labels ((next-precedence (prec assoc)
             "Returns the minimum operator precedence to use when
              parsing the right operand. If the associativity (ASSOC)
              of the current operator is LEFT, the precedence (PREC)
              is increment by 1 otherwise is returned as is."

             (case assoc
               (left (1+ prec))
               (otherwise prec)))

           (parse-expression (lhs min-prec)
             "If the next token is an infix operator, parses the right
              operand and returns the list containing the operator,
              left operand LHS, and right operand. If the next token
              is not an infix operator, returns LHS."

             (if-let ((op (parse-operator lex min-prec)))
               (destructuring-bind (op prec assoc) op
                 (let ((rhs (parse-expression (parse-node-operand lex :line-term nil)
                                              (next-precedence prec assoc))))
                   (parse-expression (list op lhs rhs) min-prec)))
               lhs)))

    (parse-expression (parse-node-operand lex :line-term line-term) 0)))

(defun parse-node-operand (lex &key (line-term *line-term*))
  "Parses an operand of an infix node expression, this can either be
   an atom or functor node. The value of the :LINE-TERM argument
   determines whether newlines occurring before the node are treated as
   declaration terminators, if no value is provided it defaults to the
   value of *LINE-TERM*."

  (multiple-value-call #'parse-node
    (next-token lex :line-term line-term)
    lex))


(defgeneric parse-node (type lexeme lex)
  (:documentation
   "Parses a node beginning with a token of type TYPE,
    and lexeme LEXEME."))

(defmethod parse-node ((type (eql :integer)) lexeme (lex t))
  "Parses an integer: converts the lexeme string to a CL integer
   value."

  (parse-integer lexeme))

(defmethod parse-node ((type (eql :id)) lexeme lex)
  "Parses either an atom node or a functor node, if the token
   following the identifier is an :OPEN-PAREN, '(', token."

  (aif (parse-prefix-operands lex)
       (list* (id-symbol lexeme) it)
       (id-symbol lexeme)))

(defmethod parse-node ((type (eql :open-paren)) (lexeme t) lex)
  "Parses a node expression, enclosed within parenthesis."

  (let ((*line-term* nil))
    (prog1 (parse-node-expression lex)
      (parse-close-paren lex))))

(defmethod parse-node ((type (eql :open-brace)) (lexeme t) lex)
  "Parses and accumulates nodes into a list until a :CLOSE-BRACE, '}',
   token is read. The first element of the list returned is 'LIST
   followed by the parsed nodes. This function is expected to be
   called after consuming an :OPEN-BRACE, '{', token."

  (parse-node-list :close-brace lex))

(defmethod parse-node (type lexeme (lex t))
  "Method for invalid tokens, signals an error."

  (error 'metalink-parse-error
         :expected '(or :id :integer :open-paren :open-brace)
         :token (cons type lexeme)
         :rule 'parse-node-operand))


(defun id-symbol (name)
  "Interns a symbol with name NAME into the :METALINK.SYMBOLS package."

  (intern name :metalink.symbols))

(defun parse-prefix-operands (lex)
  "Parses the operands of a prefix node. Returns NIL if the next token
   is not :OPEN-PAREN, thus the previous node is not a prefix functor
   node."

  (labels ((parse-separator ()
             (multiple-value-bind (type lxm) (next-token lex :peek t)
               (case type
                 ((:comma :close-paren)
                  (next-token lex)
                  (eq type :comma))

                 (otherwise
                  (error 'metalink-parse-error
                         :expected '(or :comma :close-paren)
                         :token (cons type lxm)
                         :rule 'parse-prefix-operands)))))

           (parse-operands ()
             (cons (parse-node-expression lex)
                   (and (parse-separator) (parse-operands)))))

    (when (eq (next-token lex :peek t) :open-paren)
      (next-token lex)
      (let ((*line-term* nil))
        (parse-operands)))))


(defun parse-node-list (delimiter lex)
  "Parses and accumulates nodes into a list until a token with type
   DELIMITER is read. The first element of the list returned is 'LIST
   followed by the parsed nodes."

  (let ((*list-delimiter* delimiter))
    (cons
     'list

     (iter
       (for type = (has-input? lex))
       (until (eq type delimiter))
       (when (null type)
         (error 'metalink-parse-error
                :expected delimiter
                :token nil
                :rule 'parse-node-list
                :message (format nil "Unexpected end of file, missing ~s delimiter." delimiter)))
       (collect (parse-delimited-node lex))
       (finally (next-token lex))))))


(defun parse-close-paren (lex)
  "Consumes the closing parenthesis."

  (multiple-value-bind (type lxm) (next-token lex)
    (unless (eq type :close-paren)
      (error 'metalink-parse-error
             :expected :close-paren
             :token (cons type lxm)
             :rule 'parse-sub-node))))

(defun parse-operator (lex min-prec)
  "Checks whether the next token is a binary infix operator. If the
   token is an :ID token, is found in the *OPERATOR-NODES* table, and
   has precedence greater than or equal to MIN-PREC a list containing
   the operator's symbol, precedence and associativity is returned. If
   the token is an :ID token but is not found in *OPERATOR-NODES*, an
   error condition is signaled otherwise NIL is returned."

  (multiple-value-bind (type lexeme) (next-token lex :peek t)
    (when (eq type :id)
      (let* ((op (id-symbol lexeme))
             (info (gethash op *operator-nodes*)))

        (unless info
          (error 'metalink-parse-error
                 :message (format nil "Parse error: Unknown infix operator: ~S" lexeme)))

        (when (>= (first info) min-prec)
          (next-token lex)
          (cons op info))))))

(defun parse-terminator (lex)
  "Consumes the declaration terminator. If the end of file is reached,
   it is considered as a declaration terminator."

  (multiple-value-bind (type lxm) (next-token lex :peek t)
    (cond
      ((eq type :terminate)
       (next-token lex))

      ((not (eq type *list-delimiter*))
       (error 'metalink-parse-error
              :expected :terminate
              :token (cons type lxm)
              :rule 'parse-terminator)))))



;;;; Parse Error Conditions

(define-condition metalink-parse-error (error)
  ((rule :initarg :rule
         :reader rule)
   (message :initarg :message
            :initform nil
            :reader message)
   (token-read :initarg :token
               :reader token-read)
   (token-expected :initarg :expected
                   :reader token-expected)))

(defmethod print-object ((err metalink-parse-error) stream)
  (aif (message err)
       (format stream "Parse Error: ~a" it)
       (format stream "Parse Error (~A): Expected ~A, found ~A instead"
          (rule err)
          (token-expected err)
          (token-read err))))
