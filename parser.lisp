;;;; parser.lisp
;;;;
;;;; Tridash Programming Language.
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

(in-package :tridash.parser)


(defconstant +list-operator+ 'list
  "List PSEUDO operator. Used to return a list of node declarations
   aggregated together.")

(defparameter *operator-nodes* nil
  "Hash table of the current registered operators. Each key is an
   operator symbol the corresponding value is a list of two elements
   where the first element is the operator's precedence and the second
   element is the operator's associativity: the symbol :LEFT or :RIGHT
   for left or right associativity.")

(defparameter *list-delimiter* nil
  "The delimiting token type of the node list currently being parsed,
   a terminate token is inserted before this token. Initially this is
   NIL for end of file.")


;;;; Adding operators

(defun add-operator (symbol prec assoc operators)
  "Adds the symbol SYMBOL as an operator with precedence PREC and
   associativity ASSOC."

  (setf (gethash symbol operators) (list prec assoc)))


;;;; Parsing Functions

;;; A single node declaration is parsed per invocation of the parser
;;; function (returned by make-parser) in order to allow new operators
;;; to be registered for use by the remaining declarations in the
;;; source file.

(defun make-parser (stream)
  "Returns a function of one argument, the operator nodes table. When
   the function is called, a single declaration is parsed from the
   input stream. Returns nil when EOF is reached."

  (let ((lex (make-lexer stream)))
    (lambda (*operator-nodes*)
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
  "Parses a node expression, which can either be an atom node or a
   functor. The value of the :LINE-TERM argument determines whether
   newlines occurring before the node are treated as declaration
   terminators. If no value is provided it defaults to the value of
   *LINE-TERM*."

  (labels ((next-precedence (prec assoc)
             "Returns the minimum operator precedence to use when
              parsing the right operand. If the associativity (ASSOC)
              of the current operator is :LEFT, the precedence (PREC)
              is incremented by 1 otherwise PREC is returned as is."

             (case assoc
               (:left (1+ prec))
               (otherwise prec)))

           (parse-expression (lhs min-prec)
             "Attempts to parse a functor expression where LHS is
              either the first operand of a binary expression or a
              functor and MIN-PREC is the minimum operator precedence.

              If the next token is a binary infix operator (OP), the
              right-hand-side operand (RHS) is parsed and the
              expression (OP LHS RHS) is returned.

              If the next token is an open parenthesis :OPEN-PAREN,
              the operands list (OPERANDS) is parsed and the
              expression (LHS . OPERANDS) is returned.

              If the next token is neither a binary operator nor an
              open parenthesis, LHS is returned."

             (acond
               ((parse-operator lex min-prec)
                (destructuring-bind (op prec assoc) it
                  (-<>
                   (parse-node-operand lex :line-term nil)
                   (parse-expression (next-precedence prec assoc))
                   (list op lhs <>)
                   (parse-expression min-prec))))

               ((operand-list? lex min-prec)
                (parse-expression (list* lhs (parse-prefix-operands lex)) min-prec))

               (t lhs)))

           (parse-operand (line-term)
             "Parses an operand node which can either be an atom or a
              functor expression."

             (->
              (parse-node-operand lex :line-term line-term)
              (parse-expression
               (car (gethash :open-paren *operator-nodes*))))))

    (parse-expression (parse-operand line-term) 0)))

(defun parse-node-operand (lex &key (line-term *line-term*))
  "Parses an operand of an infix node expression. The value of
   the :LINE-TERM argument determines whether newlines occurring
   before the node are treated as declaration terminators, if no value
   is provided it defaults to the value of *LINE-TERM*."

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

(defmethod parse-node ((type (eql :real)) lexeme (lex t))
  "Parses a floating point real-number: converts the lexeme string to
   a CL floating-point number."

  (parse-real-number lexeme))

(defmethod parse-node ((type (eql :string)) lexeme (lex t))
  "Parses a string literal."

  (let ((*read-eval* nil))
    (read-from-string lexeme)))

(defmethod parse-node ((type (eql :id)) lexeme (lex t))
  "Parses either an atom node or a functor node, if the token
   following the identifier is an :OPEN-PAREN, '(', token."

  (id-symbol lexeme))

(defmethod parse-node ((type (eql :open-paren)) (lexeme t) lex)
  "Parses a node expression, enclosed within parenthesis."

  (let ((*line-term* nil))
    (prog1 (parse-node-expression lex)
      (parse-close-paren lex))))

(defmethod parse-node ((type (eql :open-brace)) (lexeme t) lex)
  "Parses and accumulates nodes into a list until a :CLOSE-BRACE, '}',
   token is read. The first element of the list returned is
   +LIST-OPERATOR+ followed by the parsed nodes. This function is
   expected to be called after consuming an :OPEN-BRACE, '{', token."

  (parse-node-list :close-brace lex))

(defmethod parse-node (type lexeme (lex t))
  "Method for invalid tokens, signals an error."

  (error 'tridash-parse-error
         :expected '(or :id :integer :real :string :open-paren :open-brace)
         :token (cons type lexeme)
         :rule 'node-operand))



(defun node-path->name (path &optional (prefix "NODE"))
  "Parses the node path PATH, which is expected to be a string which
   names a node. The name may only be an atomic identifier, and the
   name of a functor node. If PATH contains a '.' the part preceding
   the dot is considered as the identifier of the module containing
   the node and the part following the dot as the the node name. If no
   PATH does not contain a '.' it is assumed to be in the :INIT
   module. Returns two values: the module identifier symbol and the
   node name symbol."

  (ematch path
    ((ppcre "^(.*?)\\.(.*?)$" module name)
     (values (id-symbol module) (id-symbol name)))

    ((type string)
     (values :init (id-symbol path)))

    (nil
     (values :init (gensym prefix)))))

(defun id-symbol (name)
  "Interns a symbol with name NAME into the :TRIDASH.SYMBOLS package."

  (intern name :tridash.symbols))

(defun operand-list? (lex precedence)
  "Returns true if the next token is the start of the operand list of
   a functor (:OPEN-PAREN). PRECEDENCE is the current minimum operator
   precedence."

  (and
   (>= (first (gethash :open-paren *operator-nodes*)) precedence)
   (eq (next-token lex :peek t) :open-paren)
   (next-token lex)
   t))

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
                  (error 'tridash-parse-error
                         :expected '(or :comma :close-paren)
                         :token (cons type lxm)
                         :rule 'functor-operands)))))

           (end-list? ()
             (when (eq (next-token lex :peek t) :close-paren)
               (next-token lex)
               t))

           (parse-operands ()
             (cons (parse-node-expression lex)
                   (and (parse-separator) (parse-operands)))))

    (let ((*line-term* nil))
      (unless (end-list?)
        (parse-operands)))))


(defun parse-node-list (delimiter lex)
  "Parses and accumulates nodes into a list until a token with type
   DELIMITER is read. The first element of the list returned is
   +LIST-OPERATOR+ followed by the parsed nodes."

  (let ((*list-delimiter* delimiter))
    (cons
     'list

     (iter
       (for type = (has-input? lex))
       (until (eq type delimiter))

       (when (null type)
         (error 'tridash-parse-error
                :expected delimiter
                :token nil
                :rule 'node-list))

       (collect (parse-delimited-node lex))
       (finally (next-token lex))))))


(defun parse-close-paren (lex)
  "Consumes the closing parenthesis."

  (multiple-value-bind (type lxm) (next-token lex)
    (unless (eq type :close-paren)
      (error 'tridash-parse-error
             :expected :close-paren
             :token (cons type lxm)
             :rule 'bracketed-node))))

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
          (error 'tridash-parse-error
                 :rule 'operator
                 :token (cons type lexeme)
                 :expected '(or infix-operator terminate)))

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
       (error 'tridash-parse-error
              :expected :terminate
              :token (cons type lxm)
              :rule 'terminator)))))



;;;; Parse Error Conditions

(define-condition tridash-parse-error (error)
  ((rule :initarg :rule
         :reader rule
         :documentation
         "The grammar rule being parsed.")

   (token-read :initarg :token
               :reader token-read
               :documentation
               "The token (TYPE . LEXEME) which was actually read.")

   (token-expected :initarg :expected
                   :reader token-expected
                   :documentation
                   "The expected token type."))

  (:documentation
   "Parse error condition."))

(defmethod error-description ((err tridash-parse-error))
  (format nil "Parse Error (~A): Expected ~A, found ~A instead."
          (rule err)
          (token-expected err)
          (token-read err)))

(defmethod print-object ((err tridash-parse-error) stream)
  (print-unreadable-object (err stream :type t :identity nil)
    (princ (error-description err) stream)))
