;;;; ast.lisp
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

;;;; AST node structures for JavaScript syntactic elements

(in-package :tridash.backend.js.ast)


;;;; AST nodes

;;; Expressions

(defstruct (js-call
             (:constructor make-js-call (operator &optional operands)))

  "Operator/function call expression."
  operator operands)

(defun js-call (operator &rest operands)
  (make-js-call operator operands))


(defstruct (js-new
             (:constructor js-new (operator &optional operands)))

  "New object expression."
  operator operands)

(defstruct (js-element
             (:constructor js-element (object element)))

  "Array/object element [] access expression."
  object element)

(defstruct (js-member
             (:constructor js-member (object field)))

  "Object member . access expression."
  object field)

(defun js-members (object &rest fields)
  "Creates a nested member expression for each field in FIELDS."

  (reduce #'js-member fields :initial-value object))


;;; Literals

(defstruct (js-string
             (:constructor js-string (thing)))

  "String literal."

  (string (if (stringp thing) thing (mkstr thing))))

(defstruct (js-array
             (:constructor js-array (&optional elements)))

  "Array literal expression."
  elements)

(defstruct (js-object
             (:constructor js-object (&optional fields)))

  "Object literal expression."
  fields)


;;; Blocks

(defstruct (js-if
             (:constructor js-if (condition then &optional else)))

  "If block."
  condition then else)

(defstruct (js-block
             (:constructor make-js-block (statements)))

  "Block of statements delimited by { ... }."
  statements)

(defun js-block (&rest statements)
  (make-js-block statements))

(defstruct (js-while
             (:constructor js-while (condition body)))

  "While loop."
  condition
  body)

(defstruct (js-function
             (:constructor js-function (name arguments statements)))

  "Function block or expression (in the case of anonymous functions)."

  name
  arguments
  statements)

(defun js-lambda (arguments statements)
  "Creates an anonymous function."

  (js-function nil arguments statements))

(defun lexical-block (&rest statements)
  "Creates a lexically scoped block. All variables introduced in the
   block, using var statements are only visible within the block and
   shadow variables introduced in outer blocks."

  (make-lexical-block statements))

(defun make-lexical-block (statements)
  "Creates a lexically scoped block. All variables introduced in the
   block, using var statements are only visible within the block and
   shadow variables introduced in outer blocks."

  (js-call (js-lambda nil statements)))


;;; Try-Catch

(defstruct (js-catch
             (:constructor js-catch (try var catch)))
  "Try-Catch Statement. TRY is the list of statements making up the
   try block. VAR is the variable to which the exception is bound and
   CATCH is the list of statements making up the catch block."

  try
  var
  catch)


;;; Statements

(defstruct (js-return
             (:constructor js-return (value)))

  "Return statement."
  value)

(defstruct (js-var
             (:constructor js-var (var &optional value)))

  "Variable, var x = <value>, declaration statement."
  var value)

(defstruct (js-continue
             (:constructor js-continue))
  "Loop continue statement.")

(defstruct (js-throw
             (:constructor js-throw (expression)))

  "Throw statement."
  expression)

;;; Expression Types

(defun function-expression? (node)
  "Returns true if the ast-node NODE is a `JS-FUNCTION' with the NAME
   slot set to NIL."

  (and (js-function-p node)
       (null (js-function-name node))))

(deftype expression ()
  "JavaScript expression derived type."

  `(or js-call
       js-new
       js-member
       js-element
       js-array
       js-object
       js-string
       (and js-function (satisfies function-expression?))

       string
       symbol
       number))

(defun expressionp (thing)
  "Returns true if THING is an ast-node which refers to a JavaScript
   expression."

  (typep thing 'expression))


;;; Mapping

(defgeneric map-js-node (fn node)
  (:documentation
   "Applies a function (FN) on each subnode of an AST node (NODE)
    which is either an expression or statement.")

  (:method (fn (call js-call))
    (make-js-call
     (funcall fn (js-call-operator call))
     (map fn (js-call-operands call))))

  (:method (fn (new js-new))
    (js-new
     (funcall fn (js-new-operator new))
     (map fn (js-new-operands new))))

  (:method (fn (element js-element))
    (js-element
     (funcall fn (js-element-object element))
     (funcall fn (js-element-element element))))

  (:method (fn (member js-member))
    (js-member
     (funcall fn (js-member-object member))
     (js-member-field member)))

  (:method (fn (string js-string))
    (declare (ignore fn))
    string)

  (:method (fn (array js-array))
    (js-array
     (map fn (js-array-elements array))))

  (:method (fn (object js-object))
    (js-object
     (map
      (lambda (field)
        (list (first field) (funcall fn (second field))))
      (js-object-fields object))))

  (:method (fn (if-block js-if))
    (js-if (funcall fn (js-if-condition if-block))
           (funcall fn (js-if-then if-block))
           (aand (js-if-else if-block) (funcall fn it))))

  (:method (fn (block js-block))
    (make-js-block (map fn (js-block-statements block))))

  (:method (fn (while js-while))
    (js-while
     (funcall fn (js-while-condition while))
     (funcall fn (js-while-body while))))

  (:method (fn (function js-function))
    (js-function (js-function-name function)
                 (map fn (js-function-arguments function))
                 (map fn (js-function-statements function))))

  (:method (fn (try js-catch))
    (js-catch (map fn (js-catch-try try))
              (js-catch-var try)
              (map fn (js-catch-catch try))))

  (:method (fn (return js-return))
    (js-return (funcall fn (js-return-value return))))

  (:method (fn (throw js-throw))
    (js-throw (funcall fn (js-throw-expression throw))))

  (:method (fn (nodes list))
    (map fn nodes))

  (:method (fn thing)
    (declare (ignore fn))
    thing))
