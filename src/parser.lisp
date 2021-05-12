;;;; parser.lisp
;;;;
;;;; Tridash Programming Language.
;;;; Copyright (C) 2018-2019  Alexander Gutev
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

(defconstant +list-operator+ (id-symbol "/prog")
  "List PSEUDO operator. Used to return a list of node declarations
   aggregated together.")

(defparameter *operator-nodes* nil
  "Map of the current registered operators. Each key is an operator
   symbol and the corresponding value is a list of two elements where
   the first element is the operator's precedence and the second
   element is the operator's associativity: :LEFT for left
   associativity or :RIGHT for right associativity.")

(defparameter *list-delimiter* nil
  "The delimiting token type of the node list currently being parsed,
   a terminate token is inserted before this token. Initially this is
   NIL for end of file.")


;;;; Declaration Types

(defstruct node-declaration
  "Represents a node declaration. Stores the location at which the
   node declaration occurs in the source file."

  location)

(defstruct (atom-node (:include node-declaration)
                      (:constructor atom-node (identifier &key location)))

  "Represents an atom node declaration wit the identifier IDENTIFIER."

  identifier)

(defstruct (literal-node (:include node-declaration)
                         (:constructor literal-node (value &key location)))

  "Represents the literal value VALUE."

  value)

(defstruct (functor-node (:include node-declaration)
                         (:constructor functor-node (operator operands &key location)))

  "Represents a functor node declaration with operator OPERATOR and
   operands OPERANDS."

  operator
  operands)


;;;; Adding operators

(defun add-operator (position symbol prec assoc operators)
  "Add a symbol to a given operator table.

   POSITION identifies the position in which the operator may be
   placed: :INFIX - infix position, :PREFIX - prefix position.

   SYMBOL is the symbol naming the operator.

   PREC is the operator precedence.

   ASSOC is the operator's associativity (:LEFT or :RIGHT).

   OPERATORS is the operator table hash map to which the operator
   should be added."

  (setf (get (list position symbol) operators) (list prec assoc)))


;;;; Parsing Functions

;;; A single node declaration is parsed per invocation of the parser
;;; function (returned by make-parser) in order to allow new operators
;;; to be registered for use by the remaining declarations in the
;;; source file.

(defgeneric make-parser (in)
  (:documentation
   "Creates a parser for the input IN, which is either a `STREAM' or a
    lexer object.

    Returns a function of one argument, the operator nodes map When
    the function is called, a single declaration is parsed from the
    input stream. Returns nil when EOF is reached."))

(defmethod make-parser ((stream stream))
  "Creates a parser with a `LEXER' object created for the input stream
   STREAM."

  (make-parser (make-lexer stream)))

(defmethod make-parser (lexer)
  "Creates a parser for the tokens read from a lexer object. LEXER
   must be an object for which there is a NEXT-TOKEN method."

  (lambda (*operator-nodes*)
    (let ((*lexer* lexer))
      (when (has-input? lexer)
        (parse-delimited-node lexer)))))


(defun has-input? (lex)
  "Returns true if there are any, non :TERMINATE, tokens left in the
   lexer's input stream. :TERMINATE tokens are skipped and removed
   from the lexer's input stream."

  (loop
     for type = (next-token lex :peek t)
     while (= type :terminate)
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

  (labels ((parse-expression (min-prec)
             "Parse expression starting from a LHS operand.

              MIN-PREC is the minimum operator precedence below which
              operators will not be returned."

             (parse-infix-expression (parse-operand line-term min-prec) min-prec))

           (parse-operand (line-term min-prec)
             "Parse an operand of an infix expression.

              Parse infix operators and but does not parse the
              argument list of a functor.

              If LINE-TERM is true a line terminator is treated as a
              TERMINATE token otherwise it is treated as whitespace
              and ignored.

              MIN-PREC is the minimum operator precedence below which
              operators are not parsed into the expression."

             (multiple-value-bind (op prec)
                 (parse-operator :prefix lex min-prec :required nil)

               (if op
                   (parse-prefix-expression op (max min-prec prec))
                   (parse-node-operand lex :line-term line-term))))

           (parse-prefix-expression (operator min-prec)
             "Parse a prefix expression starting from the operand.

              OPERATOR is the prefix operator

              MIN-PREC is the minimum operator precedence below which
              operators are not parsed into the expression."

             (if-let (operand (parse-expression min-prec))
               (functor-node operator (list operand)
                             :location (node-declaration-location operand))
               operator))

           (parse-infix-expression (lhs min-prec)
             "Parse an infix expression starting from the operator.

              LHS is the left hand side operand.

              MIN-PREC is the minimum operator precedence below which
              operators are not parsed into the expression."

             (multiple-value-bind (op prec assoc)
                 (parse-operator :infix lex min-prec)

               (cond
                 (op
                  (-<>
                   (parse-operand nil prec)
                   (parse-infix-expression (next-precedence prec assoc))
                   (list lhs <>)
                   (functor-node op <> :location (node-declaration-location lhs))
                   (parse-infix-expression min-prec)))

                 ((operand-list? lex min-prec)
                  (parse-infix-expression
                   (functor-node lhs (parse-prefix-operands lex)
                                 :location (node-declaration-location lhs))
                   min-prec))

                 (t lhs))))

           (next-precedence (prec assoc)
             "Returns the minimum operator precedence to use when
              parsing the RHS operand. If the associativity (ASSOC)
              of the current operator is :LEFT, the precedence (PREC)
              is incremented by 1 otherwise PREC is returned as is."

             (case assoc
               (:left (1+ prec))
               (otherwise prec))))

    (parse-expression 0)))

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

  (literal-node
   (parse-integer lexeme)
   :location (prior-position lex lexeme)))

(defmethod parse-node ((type (eql :real)) lexeme (lex t))
  "Parses a floating point real-number: converts the lexeme string to
   a CL floating-point number."

  (literal-node
   (parse-real-number lexeme)
   :location (prior-position lex lexeme)))

(defmethod parse-node ((type (eql :string)) lexeme (lex t))
  "Parses a string literal."

  (literal-node
   (parse-string lexeme)
   :location (prior-position lex lexeme)))

(defun parse-string (str)
  "Parse a string literal (enclosed in quotes), possibly with escape
   sequences."

  (with-output-to-string (*standard-output*)
    (with-input-from-string (*standard-input* str)
      (labels
          ((parse-escape (c)
             (write-char
              (case c
                (#\n #\Linefeed)
                (#\r #\Return)
                (#\t #\Tab)
                (#\u
                 (cond
                   ((= (peek-char) #\{)
                    (read-char)
                    (parse-unicode))

                   (t #\u)))
                (otherwise c))))

           (parse-unicode ()
             (let ((code 0))
               (loop
                  for c = (peek-char)
                  for d = (digit-char-p c 16)
                  while d
                  do
                    (setf code (+ (* code 16) d))
                    (read-char)

                  finally
                    (when (= c #\})
                      (read-char)))

               (code-char code))))

        (read-char)                     ; Discard opening quote

        (loop
           for c = (read-char)
           while (/= c #\")
           do
             (case c
               (#\\ (parse-escape (read-char)))
               (otherwise
                (write-char c))))))))

(defmethod parse-node ((type (eql :id)) lexeme (lex t))
  "Parses either an atom node or a functor node, if the token
   following the identifier is an :OPEN-PAREN, '(', token."

  (atom-node
   (id-symbol lexeme)
   :location (prior-position lex lexeme)))

(defun prior-position (lexer lexeme)
  "Returns the lexer position prior to the token with lexeme LEXEME."

  (destructuring-bind (line . column)
      (lexer-position lexer)

    (cons line (- column (length lexeme)))))


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

  (error 'declaration-parse-error
         :expected '(or :id :integer :real :string :open-paren :open-brace)
         :token type
         :rule 'node-operand
         :lexeme lexeme))


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


(defun operand-list? (lex precedence)
  "Returns true if the next token is the start of the operand list of
   a functor (:OPEN-PAREN). PRECEDENCE is the current minimum operator
   precedence."

  (and
   (>= (first (get :open-paren *operator-nodes*)) precedence)
   (= (next-token lex :peek t) :open-paren)
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
                  (= type :comma))

                 (otherwise
                  (error 'declaration-parse-error
                         :expected '(or :comma :close-paren)
                         :token type
                         :rule 'functor-operands
                         :lexeme lxm)))))

           (end-list? ()
             (when (= (next-token lex :peek t) :close-paren)
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

  (let ((*list-delimiter* delimiter)
        (pos (lexer-position lex)))

    (functor-node
     +list-operator+

     (iter
       (for type = (has-input? lex))
       (until (= type delimiter))

       (when (null type)
         (error 'declaration-parse-error
                :expected delimiter
                :token nil
                :rule 'node-list))

       (collect (parse-delimited-node lex))
       (finally (next-token lex)))

     :location pos)))


(defun parse-close-paren (lex)
  "Consumes the closing parenthesis."

  (multiple-value-bind (type lxm) (next-token lex)
    (unless (= type :close-paren)
      (error 'declaration-parse-error
             :expected :close-paren
             :token type
             :rule 'bracketed-node
             :lexeme lxm))))

(defun parse-operator (position lex min-prec &key (required t))
  "Parse an infix/prefix operator token.

   If the token is an :ID token, is found in the *OPERATOR-NODES*
   table for the given POSITION, and has precedence greater than or
   equal to MIN-PREC it is returned.

   If the token is an :ID token but is not found in *OPERATOR-NODES*,
   an error condition is signaled, unless REQUIRED is NIL.

   POSITION is the position of the operator to parse: :INFIX - infix
   operator, :PREFIX - prefix operator.

   LEX is the lexer object.

   MIN-PREC is the minimum precedence. If an operator with a
   precedence lower than this is encountered, it is not returned.

   :REQUIRED is a flag, which if true, an error condition is signalled
   if the next token is an :ID token but is not registered as an
   operator of the given type.

   If an operator was parsed returns three values:

    1. The operator symbol
    2. Operator precedence
    3. Operator associativity

   Otherwise returns NIL."

  (multiple-value-bind (type lexeme) (next-token lex :peek t)
    (when (= type :id)
      (let* ((op (id-symbol lexeme))
             (info (get (list position op) *operator-nodes*)))

        (cond
          (info
           (when (>= (first info) min-prec)
             (next-token lex)
             (values
              (atom-node op :location (prior-position lex lexeme))
              (first info)
              (second info))))

          (required
           (error 'declaration-parse-error
                  :rule 'operator
                  :token type
                  :expected `(or ,(symbolicate position '-operator) :terminate)
                  :lexeme lexeme)))))))

(defun parse-terminator (lex)
  "Consumes the declaration terminator. If the end of file is reached,
   it is considered as a declaration terminator."

  (multiple-value-bind (type lxm) (next-token lex :peek t)
    (cond
      ((= type :terminate)
       (next-token lex))

      ((/= type *list-delimiter*)
       (error 'declaration-parse-error
              :expected :terminate
              :token type
              :rule 'terminator
              :lexeme lxm)))))


;;;; Utility Functions

(defun symbol-mapping (tridash-symbol &optional (cl-symbol (intern (string-upcase tridash-symbol))))
  "Creates a mapping from a Tridash symbol to a Common Lisp
   symbol. Returns a CONS with the CAR being the Tridash symbol with
   name TRIDASH-SYMBOL, created by ID-SYMBOL, and the CDR being
   CL-SYMBOL, which defaults to a symbol with name TRIDASH-SYMBOL
   interned in the current package."

  (cons (id-symbol tridash-symbol) cl-symbol))

(defun symbol-mappings (&rest mappings)
  "Creates a mapping from Tridash symbols to Common Lisp
   symbols. Returns a list with each element of MAPPINGS replaced by
   the result of applying SYMBOL-MAPPING on the element. If an element
   is not a list it is converted to a list of one element."

  (map (compose (curry #'apply #'symbol-mapping) #'ensure-list) mappings))


;;;; Parse Error Conditions

(defconstant +token-names+
  '((:id . "identifier")
    (:integer . "integer")
    (:real . "real")
    (:string . "string")
    (:open-paren . "`(`")
    (:open-brace . "`{`")
    (:comma . "`,`")
    (:close-paren . "`)`")
    (:close-brace . "`}`")
    (:infix-operator . "infix operator")
    (:terminate . "`;` / line break"))

  "The names of the token types, which are displayed.")

(defconstant +rule-names+
  '((node-operand . "Operand Node")
    (functor-operands . "Argument List")
    (node-list . "Node List")
    (bracketed-node . "Node")
    (operator . "Infix Operator")
    (terminator . "Declaration Terminator"))

  "The names of the rules which are displayed.")

(define-condition declaration-parse-error (tridash-parse-error)
  ((rule :initarg :rule
         :reader rule
         :documentation
         "The grammar rule being parsed.")

   (token-read :initarg :token
               :reader token-read
               :documentation
               "The type of the token that was actually read.")

   (token-expected :initarg :expected
                   :reader token-expected
                   :documentation
                   "The expected token type."))

  (:documentation
   "Parse error condition."))

(defmethod print-object ((e declaration-parse-error) stream)
  (labels ((expected-string (expected)
             (match expected
               ((list* 'or tokens)
                (format nil "~{~a~@{~#[,~; or~:;,~] ~a~}~}"
                        (map #'token-name tokens)))

               (_ (token-name expected))))

           (token-name (type)
             (or (cdr (assoc type +token-names+))
                 type))

           (rule-name (rule)
             (or (cdr (assoc rule +rule-names+))
                 rule))

           (found-token (type lexeme)
             (case type
               ((:id :integer :real :string)
                (format nil "~a `~a`"
                        (cdr (assoc type +token-names+))
                        lexeme))

               (:terminate
                (format nil "declaration terminator"))

               (otherwise
                (token-name type)))))

    (with-accessors ((rule rule)
                     (expected token-expected)
                     (read token-read)
                     (lexeme lexeme))
        e

      (if read

          (format stream "Error parsing ~a: Expected ~a, found ~a."
                  (rule-name rule)
                  (expected-string expected)
                  (found-token read lexeme))

          (format stream "Error parsing ~a: Unexpected end of file, expected ~a."
                  (rule-name rule)
                  (expected-string expected))))))
