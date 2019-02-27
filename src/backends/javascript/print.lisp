;;;; print.lisp
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

;;;; Printing JavaScript code represent by AST nodes.

(in-package :tridash.backend.js)


(define-constant +js-binary-operators+
  '(+ - * / % > < >= <=
    == === != !==
    >> << >>> <<<
    & && \| \|\| ^
    = += -= *= /= %= &= \|= ^=)

  :test #'equal
  :documentation "List of JavaScript binary operators.")

(define-constant +js-unary-operators+
  '(- ! ~)

  :test #'equal
  :documentation "List of JavaScript unary operators.")


(defvar *print-indented* nil
  "If true pretty prints the code with line breaks between statements,
   and indentation.")

(defvar *indent-offset* 4
  "Number of spaces to insert for each indentation level.")


;;;; Pretty-printing state variables

(defvar *print-newline* nil
  "If true precedes the next token with a new line and indentation.")

(defvar *print-space* nil
  "If true precedes the next token with a space.")

(defvar *indent-level* 0
  "Indentation level.")


;;;; Print functions

(defun output-code (ast-nodes)
  "Prints the JavaScript code represented by each AST node, in
   AST-NODES, to *STANDARD-OUTPUT*."

  (print-ast ast-nodes :semicolon t)
  nil)


;;; Printing AST nodes

(defgeneric print-ast (ast-node &key &allow-other-keys)
  (:documentation
   "Prints the JavaScript code represent by AST-NODE. If :SEMICOLON is
    true, the code is followed by a semicolon. If :BRACKETS is true
    the code is enclosed in brackets (... code ...)."))

(defmethod print-ast ((ast-nodes list) &key semicolon brackets)
  "Prints each AST node in AST-NODES."

  (mapc (rcurry #'print-ast :semicolon semicolon :brackets brackets) ast-nodes))

(defmethod print-ast ((ast-nodes array) &key semicolon brackets)
  "Prints each AST node in AST-NODES if AST-NODES is not a string."

  (if (stringp ast-nodes)
      (call-next-method ast-nodes)
      (map nil (rcurry #'print-ast :semicolon semicolon :brackets brackets) ast-nodes)))


;;; Expressions

(defmethod print-ast ((expression js-new) &key)
  (print-token "new" :space t)
  (print-expression (js-new-operator expression)
                    (js-new-operands expression)))

(defmethod print-ast ((expression js-element) &key)
  (with-accessors ((object js-element-object)
                   (element js-element-element))
      expression

    (print-ast object)
    (print-token "[" :lead-space nil :trail-space nil)
    (print-ast element)
    (print-token "]" :lead-space nil)))

(defmethod print-ast ((expression js-member) &key)
  (with-accessors ((object js-member-object)
                   (field js-member-field))
      expression

    (print-ast object)
    (print-token "." :lead-space nil :trail-space nil)
    (print-ast field)))

(defmethod print-ast ((array js-array) &key)
  (print-token "[" :trail-space nil)
  (print-ast-list (js-array-elements array))
  (print-token "]" :lead-space nil))


(defun print-ast-list (expressions &optional brackets)
  "Prints a list of comma-separated JS expressions. If BRACKETS is
   true the entire list of expressions is surrounded in brackets."

  (when brackets
    (print-token "(" :lead-space nil :trail-space nil))

  (flet ((print-expression (expression)
           (print-token "," :lead-space nil)
           (print-ast expression)))

    (awhen (first expressions)
      (print-ast it)
      (map nil #'print-expression (rest expressions))))

  (when brackets
    (print-token ")" :lead-space nil)))

(defmethod print-ast ((object js-object) &key)
  (print-token "{")
  (print-newline)

  (let ((*indent-level* (1+ *indent-level*)))
    (labels ((print-field (field)
               (destructuring-bind (field value) field
                 (print-ast field)
                 (print-token ":")
                 (print-ast value)))

             (print-field-line (field)
               (print-token "," :lead-space nil)
               (print-newline)
               (print-field field)))

      (with-accessors ((fields js-object-fields)) object
        (awhen (first fields)
          (print-field it)
          (map nil #'print-field-line (rest fields))))))

  (print-token "}"))


;;; Operator/Function call expressions

(defmethod print-ast ((expression js-call) &key)
  (print-expression (js-call-operator expression)
                    (js-call-operands expression)))

(defun print-expression (operator operands)
  "Print an expression consisting of OPERATOR applied to OPERANDS."

  (let ((num-args (length operands)))
    (cond
      ((and (= num-args 2)
            (member operator +js-binary-operators+))
       (print-binary-expression operator operands))

      ((and (= num-args 1)
            (member operator +js-unary-operators+))
       (print-unary-expression operator operands))

      (t
       (print-function-call operator operands)))))

(defun print-function-call (function arguments)
  "Print a function call expression."

  (print-ast function
             :semicolon nil
             :brackets (or (js-call-p function)
                           (js-function-p function)))
  (print-ast-list arguments t))

(defun print-binary-expression (operator operands)
  "Print the expression consisting of the binary infix OPERATOR applied
   to OPERANDS."

  (destructuring-bind (left right) operands
    (print-ast left :semicolon nil :brackets (js-call-p left))
    (print-token operator)
    (print-ast right :semicolon nil :brackets (js-call-p right))))

(defun print-unary-expression (operator operands)
  "Print the expression consisting of the unary OPERATOR applied to
   OPERANDS."

  (print-token operator)
  (print-ast (first operands) :semicolon nil :brackets t))


;;;; Blocks

(defmethod print-ast ((block js-if) &key)
  (with-accessors ((condition js-if-condition)
                   (then js-if-then)
                   (else js-if-else))
      block

    (print-token "if")
    (print-ast condition :semicolon nil :brackets t)
    (print-ast then :semicolon else)

    (when else
      (print-newline)
      (print-token "else" :space t)
      (print-ast else))))

(defmethod print-ast ((while js-while) &key)
  (with-accessors ((condition js-while-condition)
                   (body js-while-body))
      while

    (print-token "while")
    (print-ast condition :semicolon nil :brackets t)
    (print-ast body)))

(defmethod print-ast ((func js-function) &key)
  (with-accessors ((name js-function-name)
                   (args js-function-arguments)) func

    (print-token "function" :space name)

    (awhen name
      (print-token name))

    (print-ast-list args t)
    (print-block (js-function-statements func))))


(defmethod print-ast ((block js-block) &key)
  (print-block (js-block-statements block)))

(defun print-block (statements)
  "Print a block containing the statements STATEMENTS."

  (print-token "{")
  (print-newline)

  (let ((*indent-level* (1+ *indent-level*)))
    (map nil (rcurry #'print-ast :semicolon t) statements))

  (print-newline)
  (print-token "}"))


;;; Statements

(defmethod print-ast ((ret js-return) &key)
  (print-token "return" :space t)
  (print-ast (js-return-value ret)))

(defmethod print-ast ((var js-var) &key)
  (with-accessors ((name js-var-var)
                   (value js-var-value)) var

    (print-token "var" :space t)
    (print-token name)

    (when value
      (print-token "=")
      (print-ast value))))

(defmethod print-ast ((continue js-continue) &key)
  (print-token "continue"))

(defmethod print-ast ((throw js-throw) &key)
  (print-token "throw")
  (print-ast (js-throw-expression throw)))


;;; Other expressions

(defmethod print-ast ((thing null) &key))

(defmethod print-ast (thing &key)
  (print-token thing))

(defmethod print-ast ((thing js-string) &key)
  (print-token (format nil "~s" (js-string-string thing))))


;;; Semicolons and bracketing

(defmethod print-ast :before ((thing t) &key brackets)
  (unless (listp thing)
    (print-leading-space))

  (when brackets
    (print-token "(" :trail-space nil)))

(defmethod print-ast :after ((thing t) &key semicolon brackets)
  (when brackets
    (print-token ")" :lead-space nil))

  (when (and thing semicolon (not (listp thing)))
    (unless (js-block-p thing)
      (print-token ";" :lead-space nil))
    (print-newline)))


;;;; Utility functions

(defun print-token (token &key space (lead-space t) (trail-space t))
  "Prints TOKEN preceded with the necessary whitespace.

   If the keyword argument :SPACE is true, a space is printed after
   the token regardless of the current value of *PRINT-INDENTED* or
   *PRINT-SPACE*.

   If :LEAD-SPACE is NIL no whitespace will be printed before the
   token.

   If :TRAIL-SPACE is NIL the next token printed will not be preceded
   by a space unless a newline was printed after this token."

  (let ((*print-space* (and *print-space* lead-space)))
    (print-leading-space)
    (princ token))

  (cond
    (space
     (princ " ")
     (setf *print-space* nil))

    (t (setf *print-space* trail-space))))

(defun print-leading-space ()
  "Prints the whitespace preceding a token. If *PRINT-NEWLINE* is
   true, a newline is printed followed by the number of spaces for the
   current indentation level. Otherwise if *PRINT-SPACE* is true, a
   single whitespace is printed. Both *PRINT-NEWLINE* and
   *PRINT-SPACE* are set to NIL after invoking this function."

  (when *print-indented*
    (cond
      (*print-newline*
       (fresh-line)

       (loop repeat (* *indent-level* *indent-offset*)
          do
            (princ " "))

       (setf *print-newline* nil)
       (setf *print-space* nil))

      (*print-space*
       (princ " ")
       (setf *print-space* nil)))))

(defun print-newline ()
  "Sets the *PRINT-NEWLINE* flag in order for a newline to be printed
   before the next token."

  (setf *print-newline* t))
