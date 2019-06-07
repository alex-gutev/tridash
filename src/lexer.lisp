;;;; lexer.lisp
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

(defparameter *line-term* nil
  "Flag for whether newlines should be treated as :TERMINATE tokens
   or :SKIP tokens.")

(defvar *lexer* nil
  "Lexer for the current source file.")

(defvar *current-source-file* nil
  "Path to the current source file.")


(defstruct
    (lexer
      (:constructor make-lexer (stream)))

  "Stores the lexer state. STREAM is the lexer's input
   stream. POSITION is the position (LINE . COLUMN) in the input
   stream. TOKEN is the last token read, in the case that the last
   token was peeked."

  stream
  (position (cons 0 0))

  token)

(define-condition tridash-parse-error (error)
  ((source-path
    :initarg :source-path
    :initform *current-source-file*
    :reader source-path
    :documentation
    "Path to the source file.")

   (location
    :initarg :location
    :initform (lexer-position *lexer*)
    :reader location
    :documentation
    "Location (LINE . COLUMN) within the source file at which the
     error occurred."))

  (:documentation
   "Base condition class representing parse errors."))

(define-condition invalid-token (tridash-parse-error)
  ((lexeme :initarg :lexeme
          :reader lexeme
          :documentation
          "The invalid token string"))

  (:documentation
   "Invalid token error condition."))


(defmacro! define-tokenizer (name &body states)
  "Generates a string tokenizer function from a DFSA (Deterministic
   Finite State Automaton) description. NAME is the name of the
   function.

   Each element of STATES represents a state of the DFSA, of the
   form (STATE . TRANSITIONS).

   STATE is a CONS with the CAR being the state's identifier symbol
   and the CDR being the token type identifier, that is returned by
   the function when the DFSA terminates at the state. If STATE is a
   single symbol, it is interpreted as both the state and token
   identifier.

   TRANSITIONS is a list describing the transitions out of the
   state, where each element is of the form: (TEST . TO-STATE).

   TEST is either a symbol naming a predicate function which is
   applied on the current character, a character or list with the CAR
   being the symbol NOT, AND or OR. NIL indicates the end of the
   stream. If the current character satisfies the test, the DFSA
   transitions to the state with identifier TO-STATE. If TO-STATE does
   not name any state, the value TO-STATE is interpreted as a token
   type identifier that is returned by the function.

   The initial state is the state is the first state in the list. At
   each state transition a character is appended to the lexeme
   string. The return value of the function is a CONS with the CAR
   being the token type identifier and the CDR being the lexeme
   string."

  (let ((state-names (map (compose #'ensure-car #'car) states)))
    (labels ((make-state-fn (state)
               "Generates the function for the state STATE."

               (ematch state
                 ((list* (or (cons name token) name)
                         deltas)

                  (list
                   name
                   (make-state-fn-body (or token name) deltas)))))

             (switch-state (state)
               "Generates the code which transitions to state
                STATE. If STATE is not an identifier of any state,
                code which returns it directly is generated."

               (if (memberp state state-names)
                   `(go ,state)
                   `(return ,state)))

             (make-state-fn-body (token deltas)
               "Generates the body of a state function. TOKEN is the
                token type identifier to return and DELTAS is the list
                of state transitions."

               `(let ((,g!c (next-char ,g!lex t)))
                  (declare (ignorable ,g!c))
                  (cond
                    ,@(map #'make-delta deltas)
                    (t (return ,token)))))

             (make-delta (delta)
               "Generates the code, which forms a branch in a COND
                form, for the state transition DELTA."

               (destructuring-bind (test . state) delta
                 (list (make-test test)
                       `(when ,g!c (vector-push-extend (next-char ,g!lex) ,g!buffer))
                       (switch-state state))))

             (make-test (test)
               "Generates the code for the test."

               (ematch test
                 ((type character)
                  `(= ,g!c ,test))

                 (nil
                  `(null ,g!c))

                 ('_
                  t)

                 ((type symbol)
                  `(,test ,g!c))

                 ((list 'not test)
                  `(not ,(make-test test)))

                 ((list* 'and tests)
                  `(and ,@(map #'make-test tests)))

                 ((list* 'or tests)
                  `(or ,@(map #'make-test tests))))))

      `(defun ,name (,g!lex)
         (let ((,g!buffer (make-array 0 :element-type 'character :adjustable t :fill-pointer t)))
           (cons
            (block nil
              (tagbody
                 ,@ (mappend #'make-state-fn states)
                 ))
            ,g!buffer))))))


(define-tokenizer tokenize
  ((:start . :invalid)
   (linebreakp . :newline)
   (space-char-p . :space)
   (#\# . :comment)

   (#\; . :terminate)
   (#\, . :comma)
   (#\( . :open-paren)
   (#\) . :close-paren)
   (#\{ . :open-brace)
   (#\} . :close-brace)

   ((or #\+ #\-) . :sign)
   (#\" . :in-string)
   (#\. . :dot)
   (digitp . :integer)
   (id-char-p . :id)

   (nil))


  ;; Whitespace, Newlines and Comments

  (:newline
   (linebreakp . :newline))

  ((:space . :skip)
   (space-char-p . :space))

  ((:comment . :skip)
   (nil)
   ((not linebreakp) . :comment))

  ;; Strings

  ((:in-string . :invalid)
   (#\\ . :string-escape)
   (#\" . :string)
   (nil . :invalid)
   (_ . :in-string))

  ((:string-escape . :invalid)
   (nil . :invalid)
   (_ . :in-string))

  ;; Integers

  ((:sign . :id)
   (digitp . :integer)
   (id-char-p . :id))

  (:integer
   (digitp . :integer)
   ((or #\e #\f #\d #\l) . :integer-exponent-start)
   (id-char-p . :id)
   (#\. . :real))

  ((:integer-exponent-start . :id)
   ((or digitp #\+ #\-) . :integer-exponent)
   (id-char-p . :id))

  ((:integer-exponent . :real)
   (digitp . :integer-exponent)
   (id-char-p . :id))

  ;; Reals

  (:real
   (digitp . :real)
   ((or #\e #\f #\d #\l) . :real-exponent-start)
   ((or id-char-p #\.) . :invalid))

  ((:real-exponent-start . :real)
   ((or digitp #\+ #\-) . :real-exponent)
   ((or id-char-p #\.) . :invalid))

  ((:real-exponent . :real)
   (digitp . :real-exponent)
   ((or id-char-p #\.) . :invalid))

  ;; Identifiers

  ((:dot . :id)
   (#\. . :dot))

  (:id
   (id-char-p . :id)))


(defgeneric next-token (lexer &key &allow-other-keys)
  (:documentation
   "Returns the next token as two values: the first value is a keyword
    symbol identifying the token, the second value is the lexeme
    making up the token. If the end of stream is reached NIL is
    returned. If PEEK is true the next token is returned however is
    not removed from the lexer's buffer. If a :LINE-TERM argument is
    supplied and is true a :TERNIMATE token is returned when a newline
    is read, if it is false newlines are treated as skip tokens and
    hence not returned. If a :LINE-TERM argument is not provided it
    defaults to the value of *LINE-TERM*."))

(defmethod next-token ((lex lexer) &key peek (line-term *line-term*))
  "NEXT-TOKEN method for the LEXER class."

  (labels ((next-token ()
             "Returns the next token and updates the value of the
              TOKEN slot of the lexer."

             (aprog1 (get-next-token)
               (if peek
                   (setf (lexer-token lex) it)
                   (setf (lexer-token lex) nil))

               (when (eq (car it) :invalid)
                 (error 'invalid-token
                        :lexeme (cdr it)
                        :location (lexer-position lex)))))

           (get-next-token ()
             "Returns either the value of the TOKEN slot or reads a
              new token from the input stream."

             (or (lexer-token lex) (tokenize lex))))

    (loop
       for (type . lxm) = (next-token)
       for tok-type = (convert-token type line-term)
       while (skip? tok-type lex peek)
       finally (return (values tok-type lxm)))))

(defun convert-token (type line-term)
  "Converts :NEWLINE tokens to either :TERMINATE, if LINE-TERM is
   true, or :SKIP tokens, if LINE-TERM is false. Returns the new token
   type, if TYPE is not :NEWLINE it is simply returned."

  (case type
    (:newline
     (if line-term
         :terminate
         :skip))
    (otherwise type)))

(defun skip? (type lex peek)
  "Returns true if TYPE is :SKIP, i.e. the token is a skip token. If
   PEEK is true and the token is a skip token it is removed from the
   lexer's buffer."

  (when (= type :skip)
    (if peek (setf (lexer-token lex) nil))
    t))


;;; Reading Input Stream

(defun next-char (lex &optional peek)
  "Returns the next character in the lexer's input stream. If PEEK is
   true the character is not removed. Updates the lexer's position."

  (with-accessors ((stream lexer-stream) (position lexer-position)) lex
    (aprog1
        (if peek
            (peek-char nil stream nil nil)
            (read-char stream nil nil))

      (unless peek
        (destructuring-bind (line . column) position
          (if (linebreakp it)
              (setf position (cons (1+ line) 0))
              (setf position (cons line (1+ column)))))))))


;;; Character Predicates

(defun id-char-p (c)
  "Returns true if C is a character which may appear in an
   identifier."

  (and c
       (not (space-char-p c))
       (not (linebreakp c))
       (not (memberp c '(#\; #\. #\, #\( #\) #\{ #\} #\")))))

(defun space-char-p (c)
  "Returns true if C is a whitespace character (excluding line
   breaks)."

  (memberp c '(#\Space #\Tab)))

(defun linebreakp (c)
  "Returns true if C is a line break character."

  (memberp c '(#\Linefeed #\Return)))

(defun digitp (c)
  "Returns true if C is a digit character."

  (and c (digit-char-p c)))


;;; Print Object Methods

(defmethod print-object :around ((e tridash-parse-error) stream)
  (with-slots (source-path location) e
    (destructuring-bind (line . column) location
      (format stream "Parse error in ~a at ~a:~a: "
              source-path
              line
              column)
      (call-next-method))))

(defmethod print-object ((e invalid-token) stream)
  (with-slots (lexeme) e
    (format stream "Invalid Token: `~a`." lexeme)))
