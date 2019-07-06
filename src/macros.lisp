;;;; macros.lisp
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

;;;; Implementation of user-defined macros.

(in-package :tridash.frontend)

(in-readtable lol-syntax)


(defvar *operand-vars* nil
  "The operands list of the meta-node currently being compiled to
   CL.")

(defvar *current-meta-node* nil
  "The meta-node currently being compiled to CL.")


;;;; Macro Attributes

(defmethod process-attribute (node (attribute (eql (id-symbol "MACRO"))) value (module t))
  "Sets the internal :MACRO-FUNCTION attribute to a function that
   compiles the META-NODE to a CL function, calls it and calls
   PROCESS-DECLARATION on the result."

  (when (bool-value value)
    (setf (node-macro-function node)
          (make-macro-function node))
    value))


;;;; Macro Function

(defun make-macro-function (meta-node)
  "Creates the macro-node function, which compiles META-NODE to a CL
   function, calls the function and processes the result."

  (lambda (operator operands module)
    (declare (ignore operator))
    (-> (call-tridash-meta-node meta-node operands)
        resolve
        (process-declaration module :level *level*))))


;;;; Compiling CL function from Meta-Node

(defun call-tridash-meta-node (meta-node args)
  "Calls the meta-node META-NODE with arguments ARGS. If the meta-node
   has been compiled to a CL function, the function is called
   otherwise it is compiled to a CL function by
   COMPILE-META-NODE-FUNCTION."

  (apply
   (or (meta-node-cl-function meta-node)
       (compile-meta-node-function meta-node))
   args))

(defun compile-meta-node-function (meta-node)
  "Compiles the META-NODE meta-node to a CL function. Stores the
   compiled CL function in the :CL-FUNCTION attribute."

  (build-meta-node meta-node)
  (finish-build-meta-node meta-node)

  (setf (meta-node-cl-function meta-node)
        (compile nil (tridash->cl-function meta-node))))


(defun meta-node-cl-function (meta-node)
  "Returns the compiled CL function of the meta-node."

  (get :cl-function (attributes meta-node)))

(defun (setf meta-node-cl-function) (fn meta-node)
  "Sets the compiled CL function of META-NODE to FN."

  (setf (get :cl-function (attributes meta-node)) fn))


;;;; Error Conditions

(define-condition unsupported-meta-node-error (semantic-error)
  ((node-name :initarg :node-name
              :reader node-name
              :documentation "Name of the unsupported meta-node"))

  (:documentation
   "Error condition: `EXTERNAL-META-NODE' is not supported in
    macros."))

(defmethod print-object ((e unsupported-meta-node-error) stream)
  (format stream "External Meta-Node `~a` is not supported in macros."
          (node-name e)))


(define-condition macro-outer-node-error (semantic-error)
  ()

  (:documentation
   "Error condition: Outer nodes referenced from a macro-node or a
    meta-node used by a macro-node."))

(defmethod print-object ((e macro-outer-node-error) stream)
  (format stream "Cannot reference outer-nodes from macro-node or meta-node used by macro-node."))



;;;; Compiling Tridash Expressions to CL

(defvar *tridash-cl-functions* (make-hash-map)
  "Map mapping Tridash meta-node identifiers to the corresponding CL
   function identifiers.")


(defun tridash->cl-function (meta-node)
  "Returns a CL LAMBDA expression which is compiled from META-NODE."

  (let ((*current-meta-node* meta-node)
        (*operand-vars* (make-hash-map)))

    (flet ((add-operand (operand)
             (ensure-get operand *operand-vars* (gensym (mkstr operand)))))

      `(lambda ,(map #'add-operand (operands meta-node))
         ,(->
           (contexts meta-node)
           map-values
           first
           value-function
           (tridash->cl :thunk t))))))


(defgeneric tridash->cl (expression &key &allow-other-keys)
  (:documentation
   "Compiles the Tridash expression EXPR to a CL expression. If the
    :THUNK argument is true (the default), then an expression which
    creates a `THUNK' is generated, otherwise an expression which
    computes the value directly is returned."))

(defmethod tridash->cl :around ((expression t) &key (thunk t))
  "If THUNK is true and EXPRESSION is compiled to a CL function
   application expression, then the CL expression is wrapped in a
   THUNK."

  (let ((cl-expression (call-next-method)))
    (if (and thunk (not (constant? cl-expression)))
        `(thunk ,cl-expression)
        cl-expression)))

(defun constant? (thing)
  "Returns true if THING is a constant which should not be wrapped in
   a `THUNK'."

  (or (atom thing)
      (match thing
        ((list (or 'quote 'function) _) t))))


(defmethod tridash->cl ((link node-link) &key)
  "Returns the variable corresponding to the linked node by looking up
   the node's identifier in *OPERAND-VARS*"

  (with-slots (name) (node-link-node link)
    (ensure-get name *operand-vars* (gensym (mkstr name)))))


(defmethod tridash->cl ((functor functor-expression) &key)
  "If the operator of the functor is an `EXTERNAL-META-NODE',
   generates a CL expression which calls the corresponding CL
   function, found by looking up the name of the meta-node in
   +TRIDASH-CL-FUNCTIONS+. If there is no corresponding CL function an
   error condition is signalled.

   If the operator is a `META-NODE', generates a
   CALL-TRIDASH-META-NODE expression."

  (with-struct-slots functor-expression- (meta-node arguments)
      functor

    (match meta-node
      ((external-meta-node name)
       (->> (make-meta-node-arguments meta-node arguments)
            (list* (external-meta-node-cl-function name))))

      ((type meta-node)
       (->> (make-meta-node-arguments meta-node arguments)
            (list* 'list)
            (list 'call-tridash-meta-node meta-node)))

      (_
       `(funcall (resolve ,(tridash->cl meta-node :thunk nil)) ,@(map #'tridash->cl arguments))))))

(defun make-meta-node-arguments (meta-node arguments)
  "Returns expressions, wrapped in `THUNK's if necessary, for the
   arguments ARGUMENTS of the `FUNCTOR-EXPRESSION' with operator
   META-NODE."

  (loop
     for (strict? . strict-args) = (strict-arguments meta-node) then strict-args
     for arg in arguments
     collect (tridash->cl arg :thunk (not strict?))))

(defun strict-arguments (meta-node)
  "Returns a list indicating whether each operand of META-NODE is
   strict or lazy, by
   TRIDASH.FRONTEND.STRICTNESS::STRICT-ARGUMENTS. This function
   ensures that the definition of META-NODE is fully built."

  ;; Ensure that META-NODE is built
  (build-meta-node meta-node)
  (finish-build-meta-node meta-node)

  (tridash.frontend.strictness::strict-arguments meta-node))

(defmethod tridash->cl ((object object-expression) &key)
  "Generates a CL expression that creates a `HASH-MAP'."

  (flet ((make-entry (pair)
           (destructuring-bind (key value) pair
             `(cons ',key ,(tridash->cl value :thunk nil)))))

    `(alist-hash-map (list ,@(map #'make-entry (object-expression-entries object))))))

(defmethod tridash->cl ((block expression-block) &key)
  (tridash->cl (expression-block-expression block)
               :thunk nil))


(defmethod tridash->cl ((ref meta-node-ref) &key)
  (with-struct-slots meta-node-ref- (node outer-nodes)
      ref

    (when outer-nodes
      (error 'macro-outer-node-error))

    (match node
      ((external-meta-node name)
       `#',(external-meta-node-cl-function name))

      ((type meta-node)
       (with-gensyms (args)
         `#'(lambda (&rest ,args)
            (call-tridash-meta-node ,node ,args)))))))

(defmethod tridash->cl (literal &key)
  (match literal
    ((or (type number) (type string))
     literal)

    (_ `',literal)))

(defun external-meta-node-cl-function (name)
  "Returns the name of the CL function implementing the
   `EXTERNAL-META-NODE' with name NAME."

  (or (get name *tridash-cl-functions*)
      (error 'unsupported-meta-node-error :node-name name)))

;;;; Macro-Writing

(defmethod process-functor ((operator (eql +quote-operator+)) args module)
  "Returns the raw argument unprocessed."

  (declare (ignore module))

  (match-syntax (operator any) args
    ((list thing)
     thing)))


;;;; Tridash CL Runtime

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
  "If THING is a `THUNK' calls it's COMPUTE function. If the function
   returns another `THUNK' repeats the procedure on it.

   If THING is not a `THUNK', returns it."

  (nlet-tail resolve ((thing thing))
    (typecase thing
      (thunk (resolve (funcall (thunk-fn thing))))
      (otherwise thing))))

(define-condition tridash-fail ()
  ()

  (:documentation
   "Condition raised when a Tridash fail expression is evaluated."))


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
   resolved (by RESOLVE) arguments. Otherwise the macro is identical
   to DEFINE-TRIDASH-FUNCTION%."

  (match body
    ((list (and (type symbol) fn))
     `(define-tridash-function% ,name (&rest ,g!args)
        (apply #',fn (map #'resolve ,g!args))))

    (_
     `(define-tridash-function% ,name ,lambda-list ,@body))))


;;;; Core Functions

;;; Conditionals and Boolean Expressions

(define-tridash-function |if| (cond then else)
  (if (bool-value (resolve cond)) then else))

(define-tridash-function |member| (object key)
  (get (resolve key) (resolve object)))

(define-tridash-function |fail| ()
  (thunk (error 'tridash-fail)))

(define-tridash-function |catch| (try catch)
  (handler-case (resolve try)
    (tridash-fail () catch)))


;;; Boolean Expressions

(define-tridash-function |and| (a b)
  (if (bool-value (resolve a)) b 0))

(define-tridash-function |or| (a b)
  (or (bool-value (resolve a)) b))

(define-tridash-function |not| (a)
  (if (bool-value (resolve a)) 0 1))


;;; Arithmetic

(define-tridash-function + (a b) +)
(define-tridash-function - (a b) -)
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

(define-tridash-function |cons| (a b) cons)
(define-tridash-function |list| (&rest xs) list)
