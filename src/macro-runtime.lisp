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


(defconstant +empty-list+ (get :empty-list *core-meta-nodes*)
  "Meta-Node representing the empty list failure type.")


;;;; Thunks

(defstruct thunk
  "A thunks stores a function which evaluates to a value. The function
   COMPUTE is called to compute the value when it is actually required."

  compute)

(defstruct (catch-thunk (:include thunk))
  "A thunk which resolves to the value of CATCH if its COMPUTE
   function fails."

  catch)


(defmacro! thunk (expression)
  "Creates a `THUNK' with a COMPUTE function that evaluates
   EXPRESSION."

  `(let (,g!result ,g!computed?)
     (make-thunk
      :compute
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

(defun resolve-list (list)
  "Returns a list where each element of LIST is resolved by RESOLVE."

  (loop
     for items = list then
       (handler-case
           (resolve% (rest items))
         (tridash-fail (c)
           (if (= (fail-type c) +empty-list+)
               nil
               (error c))))

     while items
     collect
       (resolve (car items))))


(defun resolve% (thing)
  "If THING is a `THUNK' calls it's COMPUTE function. If the function
   returns another `THUNK' repeats the procedure on it.

   If THING is not a `THUNK', returns it."

  (nlet-tail resolve ((thing thing))
    (typecase thing
      (catch-thunk
       (resolve
        (resolve-catch
         (catch-thunk-compute thing)
         (catch-thunk-catch thing))))

      (thunk
       (resolve% (funcall (thunk-compute thing))))

      (otherwise thing))))

(defun resolve-catch (try catch)
  "Resolves TRY fully. If TRY fails returns CATCH or the CATCH of the
   last `CATCH-THUNK' to which TRY resolves."

  (handler-case
      (nlet-tail resolve ((try (funcall try)))

        (typecase try
          (catch-thunk
           (setf catch (combine-catch-thunk (catch-thunk-catch try)
                                            catch))

           (resolve (funcall (catch-thunk-compute try))))

          (thunk
           (resolve (funcall (thunk-compute try))))

          (otherwise
           try)))

    (tridash-fail ()
      catch)))

(defun combine-catch-thunk (try catch)
  "Returns a `CATCH-THUNK' with a COMPUTE function that evaluates to
   TRY and CATCH as the CATCH value.

   If TRY is a `CATCH-THUNK' returns a new `CATCH-THUNK', in which the
   CATCH value is the result of combining (by COMBINE-CATCH-THUNK) the
   CATCH value of TRY and CATCH.

   If TRY is a `THUNK', returns a new `CATCH-THUNK' with COMPUTE
   function set to the COMPUTE function of TRY and CATCH value CATCH.

   If TRY is not a `THUNK', simply returns its value."

  (match try
    ((catch-thunk- compute (catch new-catch))
     (make-catch-thunk
      :compute compute
      :catch (combine-catch-thunk new-catch catch)))

    ((thunk- compute)
     (make-catch-thunk
      :compute compute
      :catch catch))

    (_ try)))


;;;; Failures

(define-condition tridash-fail ()
  ((fail-type
    :initarg :fail-type
    :initform nil
    :accessor fail-type
    :documentation
    "Value indicating the type of failure."))

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

(defun fail-thunk (&optional type)
  "Returns a new thunk which signals a `TRIDASH-FAIL' condition, with
   failure type TYPE."

  (thunk (error 'tridash-fail :fail-type type)))

(defun get-fail-type (thing)
  (handler-case
      (progn
        (resolve% thing)
        (fail-thunk))
    (tridash-fail (c) (fail-type c))))


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

   If the elements of LAMBDA-LIST are lists, where the first element
   is the variable named and the second element is a type specifier,
   then the body of the function is wrapped in a CHECK-TRIDASH-TYPES
   form, with checks the types of the arguments in LAMBDA-LIST.

   This macro always wraps BODY in a HANDLER-CASE form handling the
   `TRIDASH-FAIL' condition. If the forms in BODY signal a
   `TRIDASH-FAIL' condition a thunk which re-signals the condition is
   returned."

  (match body
    ((list (and (type symbol) fn))
     `(define-tridash-function ,name ,(map #'ensure-car lambda-list)
        ,(if (some #'consp lambda-list)

             `(check-tridash-types ,lambda-list
                (,fn ,@(map #'first lambda-list)))

             `(,fn ,@(map (curry #'list 'resolve%) lambda-list)))))

    (_
     `(define-tridash-function% ,name ,lambda-list
        (catch-failures ,@body)))))


;;;; Core Functions

(deftype tridash-value ()
  `(or string symbol number))

(defmacro check-tridash-types ((&rest vars) &body body)
  "Performs type checking and signals a `TRIDASH-FAIL' condition if
   type checking fails. Each element of VARS is a list where the first
   element is a symbol naming a variable and the second element is a
   type specifier to which the value of the variable is
   checked. Additionally each value is resolved by RESOLVE% and bound
   to a variable with the same name. These bindings are visible to the
   forms in BODY."

  (flet ((make-check-type (var)
           `(typep ,(first var) ',(second var)))

         (make-binding (var)
           `(,(first var) (resolve% ,(first var)))))

    `(let ,(map #'make-binding vars)
       (if (and ,@(map #'make-check-type vars))
           (progn ,@body)
           (error 'tridash-fail)))))


;;; Conditionals

(define-tridash-function |if| (cond then &optional (else (fail-thunk)))
  (if (bool-value (resolve% cond)) then else))

(define-tridash-function |member| (object key)
  (check-tridash-types ((object hash-map))
    (multiple-value-bind (value in-map?)
        (get (resolve% key) object)

      (unless in-map?
        (error 'tridash-fail))

      value)))

(define-tridash-function |catch| (try catch &optional (test nil testp))
  (->> (if testp (thunk (test-fail-type try catch test)) catch)
       (combine-catch-thunk try)))

(defun test-fail-type (try catch test)
  "Applies the function/meta-node TEST on the failure type of TRY. If
   the result is true, returns CATCH otherwise returns a thunk which
   fails with the same type as TRY."

  (let ((type (get-fail-type try)))
    (if (bool-value (call-node test (list type)))
        catch
        (fail-thunk type))))


(define-tridash-function% |fail| (&optional type)
  (fail-thunk type))

(define-tridash-function% |fail-type| (thing)
  (get-fail-type thing))


(defun call-node (operator args)
  "Applies the function OPERATOR on ARGS."

  (catch-failures
    (check-tridash-types ((operator (or function meta-node)))
      (typecase operator
        (function
         (apply operator args))

        (meta-node
         (call-meta-node operator args))))))


;;; Boolean Expressions

(define-tridash-function |and| (a b)
  (if (bool-value (resolve% a)) b 0))

(define-tridash-function |or| (a b)
  (or (bool-value (resolve% a)) b))

(define-tridash-function |not| (a)
  (if (bool-value (resolve% a)) 0 1))


;;; Arithmetic

(define-tridash-function + ((a number) (b number)) +)

(define-tridash-function - (&rest args)
  (ematch args
    ((list a b)
     (check-tridash-types ((a number) (b number))
       (- a b)))

    ((list a)
     (check-tridash-types ((a number))
       (- a)))))

(define-tridash-function * ((a number) (b number)) *)
(define-tridash-function / ((a number) (b number)) /)


;;; Comparison

(define-tridash-function < ((a number) (b number)) <)
(define-tridash-function <= ((a number) (b number)) <=)
(define-tridash-function > ((a number) (b number)) >)
(define-tridash-function >= ((a number) (b number)) >=)

(define-tridash-function = (a b)
  (= (resolve% a) (resolve% b)))

(define-tridash-function != (a b)
  (/= (resolve% a) (resolve% b)))


;;; Type Conversions

(define-tridash-function |string| (x) mkstr)

(define-tridash-function |int| (x)
  (typecase (resolve% x)
    (integer x)
    (real (truncate x))
    (string
     (handler-case
         (parse-integer x)
       (error () (error 'tridash-fail))))))

(define-tridash-function |real| (x)
  (typecase (resolve% x)
    ((or integer real) x)
    (string
     (handler-case
         (parse-number:parse-real-number x)
       (error () (error 'tridash-fail))))))


;;; Type Predicates

(define-tridash-function |int?| (x) integerp)
(define-tridash-function |real?| (x) realp)
(define-tridash-function |string?| (x) stringp)


;;; Lists

(define-tridash-function |cons| (a b)
  (cons a b))

(define-tridash-function |head| (list)
  (atypecase (resolve% list)
    (cons (car it))
    (null (empty-list))
    (otherwise (fail-thunk))))

(define-tridash-function |tail| (list)
  (atypecase (resolve% list)
    (cons
     (or (cdr it) (empty-list)))
    (null (empty-list))
    (otherwise (fail-thunk))))

(define-tridash-function |cons?| (thing)
  (consp (resolve% thing)))

(define-tridash-function |Empty| ()
  (empty-list))

(defun empty-list ()
  "Returns a `THUNK' which signals a `TRIDASH-FAIL' condition with the
   type representing an empty list."

  (fail-thunk +empty-list+))


;;; Introspection Utilities

(define-tridash-function |node?| (thing)
  (node? (resolve% thing)))

(define-tridash-function |find-node| (node &optional module)
  (let ((module (or (resolve% module) *current-module*)))

    (unless (typep module 'module)
      (error 'tridash-fail))

    (handler-case
        (let ((*create-nodes* nil))
          (at-source
            (process-declaration (resolve node) module :top-level t)))

      (non-existent-node-error ()
        (error 'tridash-fail)))))

(define-tridash-function |get-attribute| (node attribute)
  (check-tridash-types ((node node))
    (get (mkstr (resolve% attribute)) (attributes node) (fail-thunk))))
