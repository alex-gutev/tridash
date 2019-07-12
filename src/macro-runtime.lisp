;;;; macro-runtime.lisp
;;;;
;;;; Tridash Programming Language.
;;;; Copyright (C) 2019  Alexander Gutev
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

;;;; Tridash runtime for macro system.

(in-package :tridash.frontend)


;;;; Thunks

(defstruct thunk
  "A thunks stores a function which evaluates to a value. The function
   FN is called to compute the value when it is actually required."

  fn)

(defmacro! thunk (expression)
  "Creates a `THUNK' with a COMPUTE function that evaluates
   EXPRESSION."

  `(let (,g!result ,g!computed?)
     (make-thunk
      :fn
      (lambda ()
        (if ,g!computed?
            ,g!result
            (prog1 (setf ,g!result ,expression)
              (setf ,g!computed? t)))))))

(defun resolve (thing)
  "Returns the value of THING. If THING has not yet been evaluated, it
   is evaluated. If THING evaluates to a list, each element of the
   list is resolved by RESOLVE."

  (atypecase (resolve% thing)
    (cons
     (resolve-list it))

    (otherwise it)))

(defun resolve% (thing)
  "If THING is a `THUNK' calls it's COMPUTE function. If the function
   returns another `THUNK' repeats the procedure on it.

   If THING is not a `THUNK', returns it."

  (nlet-tail resolve ((thing thing))
    (typecase thing
      (thunk (resolve (funcall (thunk-fn thing))))
      (otherwise thing))))

(defun resolve-list (list)
  "Returns a list where each element of LIST is resolved by RESOLVE."

  (loop
     for items = list then (resolve% (rest items))
     while items
     collect (resolve (car items))))


;;;; Failures

(define-condition tridash-fail ()
  ()

  (:documentation
   "Condition raised when a Tridash fail expression is evaluated."))

(defmacro! catch-failures (&body body)
  "Wraps BODY in a HANDLER-CASE that handles `TRIDASH-FAIL'
   conditions, by returning a thunk that resignals the condition."

  `(handler-case
       (progn ,@body)

     ;; Catch value failures triggered by the evaluation of the
     ;; function and return them as a thunk.
     (tridash-fail (,g!c) (thunk (error ,g!c)))))


;;;; Definition Macros

(defmacro define-tridash-function% (name (&rest lambda-list) &body body)
  "Defines an externally defined Tridash function, with name NAME,
   lambda-list LAMBDA-LIST and body BODY.

   NAME is converted to a string and interned in the TRIDASH.SYMBOLS
   package. An entry which maps NAME to the the function is added to
   *TRIDASH-CL-FUNCTIONS*."

  (let ((name (id-symbol (string name))))
    `(progn
       (defun ,name ,lambda-list ,@body)

       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',name *tridash-cl-functions*) ',name)))))

(defmacro! define-tridash-function (name (&rest lambda-list) &body body)
  "Defines an externally defined Tridash function by
   DEFINE-TRIDASH-FUNCTION%. If BODY consists of a single symbol a
   function, which applies the function named by the symbol on the
   resolved (by RESOLVE%) arguments. Otherwise the macro is identical
   to DEFINE-TRIDASH-FUNCTION%.

   This macro always wraps BODY in a HANDLER-CASE form handling the
   `TRIDASH-FAIL' condition. If the forms in BODY signal a
   `TRIDASH-FAIL' condition a thunk which re-signals the condition is
   returned."

  (match body
    ((list (and (type symbol) fn))
     `(define-tridash-function ,name (&rest ,g!args)
        (apply #',fn (map #'resolve% ,g!args))))

    (_
     `(define-tridash-function% ,name ,lambda-list
        (catch-failures ,@body)))))


;;;; Core Functions

;;; Conditionals and Boolean Expressions

(define-tridash-function |if| (cond then else)
  (if (bool-value (resolve% cond)) then else))

(define-tridash-function |member| (object key)
  (get (resolve% key) (resolve% object)))

(define-tridash-function% |fail| ()
  (thunk (error 'tridash-fail)))

(define-tridash-function |catch| (try catch)
  (handler-case (resolve% try)
    (tridash-fail () catch)))

(defun call-node (operator args)
  "Applies the function OPERATOR on ARGS."

  (catch-failures
    (apply (resolve% operator) args)))


;;; Boolean Expressions

(define-tridash-function |and| (a b)
  (if (bool-value (resolve% a)) b 0))

(define-tridash-function |or| (a b)
  (or (bool-value (resolve% a)) b))

(define-tridash-function |not| (a)
  (if (bool-value (resolve% a)) 0 1))


;;; Arithmetic

(define-tridash-function + (a b) +)
(define-tridash-function - (a &optional b)
  (if b
      (- (resolve% a) (resolve% b))
      (- (resolve% a))))

(define-tridash-function * (a b) *)
(define-tridash-function / (a b) /)

;;; Comparison

(define-tridash-function < (a b) <)
(define-tridash-function <= (a b) <=)
(define-tridash-function > (a b) >)
(define-tridash-function >= (a b) >=)
(define-tridash-function = (a b) =)
(define-tridash-function != (a b) /=)

;;; Type Conversions

(define-tridash-function |string| (x) mkstr)

;;; Type Predicates

(define-tridash-function |int?| (x) integerp)
(define-tridash-function |real?| (x) numberp)
(define-tridash-function |string?| (x) stringp)

;;; Lists

(define-tridash-function |cons| (a b)
  (cons a b))

(define-tridash-function |list| (xs)
  (progn
    xs))

(define-tridash-function |head| (list) car)
(define-tridash-function |tail| (list) cdr)
