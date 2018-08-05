;;;; ast.lisp
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

;;;; Printing JavaScript code represent by AST nodes.

(in-package :metalink.backend.js)


(defconstant +js-binary-operators+
  '(+ - * / % > < >= <=
    == === != !==
    >> << >>> <<<
    & && \| \|\| ^
    = += -= *= /= %= &= \|= ^=)

  "List of JavaScript binary operators.")

(defconstant +js-unary-operators+
  '(- ! ~)

  "List of JavaScript unary operators.")


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

  (print-ast ast-nodes t)
  nil)


(defgeneric print-ast (ast-node &optional semicolon brackets)
  (:documentation
   "Prints the JavaScript code represent by AST-NODE. If SEMICOLON is
    true, the code is followed by a semicolon. If BRACKETS is true the
    code is enclosed in brackets (... code ...)."))

(defmethod print-ast ((ast-nodes list) &optional semicolon brackets)
  "Prints each AST node in AST-NODES."
  (mapc (rcurry #'print-ast semicolon brackets) ast-nodes))


;;; Expressions

(defmethod print-ast ((expression js-new) &optional semicolon brackets)
  (declare (ignore semicolon brackets))

  (print-token "new" :space t)
  (call-next-method))

(defmethod print-ast ((expression js-element) &optional semicolon brackets)
  (declare (ignore semicolon brackets))

  (with-accessors ((object js-element-object)
                   (element js-element-element))
      expression
    (print-ast object)
    (print-token "[" :lead-space nil :trail-space nil)
    (print-ast element)
    (print-token "]" :lead-space nil)))

(defmethod print-ast ((expression js-member) &optional semicolon brackets)
  (declare (ignore semicolon brackets))

  (with-accessors ((object js-member-object)
                   (field js-member-field))
      expression
    (print-ast object)
    (print-token "." :lead-space nil :trail-space nil)
    (print-ast field)))

(defmethod print-ast ((array js-array) &optional semicolon brackets)
  (declare (ignore semicolon brackets))

  (print-token "[" :trail-space nil)
  (print-ast-list (js-array-elements array))
  (print-token "]" :lead-space nil))


(defun print-ast-list (expressions &optional brackets)
  (when brackets
    (print-token "(" :lead-space nil :trail-space nil))

  (awhen (first expressions)
    (print-ast it)
    (dolist (expression (rest expressions))
      (print-token "," :lead-space nil)
      (print-ast expression)))

  (when brackets
    (print-token ")" :lead-space nil)))

(defmethod print-ast ((object js-object) &optional semicolon brackets)
  (declare (ignore semicolon brackets))

  (print-token "{")
  (print-newline)

  (let ((*indent-level* (1+ *indent-level*)))
    (flet ((print-field (field)
             (destructuring-bind (field value) field
               (print-token field)
               (print-token ":")
               (print-token value))))

      (with-accessors ((fields js-object-fields)) object
        (awhen (first fields)
          (print-field it)
          (dolist (field (rest fields))
            (print-token "," :lead-space nil)
            (print-newline)
            (print-field field))))))

  (print-token "}"))


;;; Operator/Function call expressions

(defmethod print-ast ((expression js-call) &optional semicolon brackets)
  (declare (ignore semicolon brackets))

  (with-accessors ((operator js-call-operator)
                   (operands js-call-operands))
      expression

    (let ((num-args (length operands)))
      (cond
        ((and (= num-args 2)
              (member operator +js-binary-operators+))
         (print-binary-expression operator operands))
        ((and (= num-args 1)
              (member operator +js-unary-operators+))
         (print-unary-expression operator operands))
        (t
         (print-function-call operator operands))))))

(defun print-function-call (function arguments)
  (print-ast function nil (typep function 'js-function))
  (print-ast-list arguments t))

(defun print-binary-expression (operator operands)
  (destructuring-bind (left right) operands
    (print-ast left nil t)
    (print-token operator)
    (print-ast right nil t)))

(defun print-unary-expression (operator operands)
  (print-token operator)
  (print-ast (first operands) nil t))


;;;; Blocks

(defmethod print-ast ((block js-if) &optional semicolon brackets)
  (declare (ignore semicolon brackets))

  (with-accessors ((condition js-if-condition)
                   (then js-if-then)
                   (else js-if-else))
      block
    (print-token "if")
    (print-ast condition nil t)
    (print-ast then (not else))

    (when else
      (print-newline)
      (print-token "else" :space t)
      (print-ast else))))

(defmethod print-ast ((block js-block) &optional semicolon brackets)
  (declare (ignore semicolon brackets))

  (print-token "{")
  (print-newline)

  (let ((*indent-level* (1+ *indent-level*)))
    (mapc (rcurry #'print-ast t) (js-block-statements block)))

  (print-newline)
  (print-token "}"))


(defmethod print-ast ((func js-function) &optional semicolon brackets)
  (declare (ignore semicolon brackets))

  (with-accessors ((name js-function-name)
                   (args js-function-arguments)) func

    (print-token "function" :space name)

    (awhen name
      (print-token name))

    (print-ast-list args t)
    (call-next-method)))



;;; Statements

(defmethod print-ast ((ret js-return) &optional semicolon brackets)
  (declare (ignore semicolon brackets))
  (print-token "return" :space t)
  (print-ast (js-return-value ret)))

(defmethod print-ast ((var js-var) &optional semicolon brackets)
  (declare (ignore semicolon brackets))
  (with-accessors ((name js-var-var)
                   (value js-var-value)) var
    (print-token "var" :space t)
    (print-token name)

    (when value
      (print-token "=")
      (print-ast value))))


;;; Other expressions

(defmethod print-ast ((thing null) &optional semicolon brackets)
  (declare (ignore semicolon brackets)))

(defmethod print-ast (thing &optional semicolon brackets)
  (declare (ignore semicolon brackets))
  (print-token thing))

(defmethod print-ast ((thing js-string) &optional semicolon brackets)
  (declare (ignore semicolon brackets))
  (print-token (format nil "~s" (js-string-string thing))))


;;; Semicolons and bracketing

(defmethod print-ast :before ((thing t) &optional semicolon brackets)
  (declare (ignore semicolon))

  (unless (listp thing)
    (print-leading-space))

  (when brackets
    (print-token "(" :trail-space nil)))

(defmethod print-ast :after ((thing t) &optional semicolon brackets)
  (when brackets
    (print-token ")" :lead-space nil))

  (when (and thing semicolon (not (listp thing)))
    (print-token ";" :lead-space nil)
    (print-newline)))


;;;; Utility functions

(defun print-token (token &key space (lead-space t) (trail-space t))
  "Prints TOKEN preceded with the necessary whitespace. If the keyword
   argument :SPACE is true, a space is printed after the token
   regardless of the current value of *PRINT-INDENTED* or
   *PRINT-SPACE*, by default is is NIL. If :LEAD-SPACE is NIL, no
   whitespace will be printed before the token if :TRAIL-SPACE is NIL,
   *PRINT-SPACE* is NIL the next token printed will not be preceded by
   a space unless a newline was printed after this token."

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
