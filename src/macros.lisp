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


;;;; Compiling Tridash Expressions to CL

(defconstant +tridash-recur-tag+ 'recur
  "TAGBODY tag for tail-recursive self calls.")

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
         (block nil
           (tagbody
              ,+tridash-recur-tag+
              (return
                ,(->
                  (contexts meta-node)
                  map-values
                  first
                  value-function
                  (tridash->cl :tail-position-p t)))))))))


(defgeneric tridash->cl (expression &key &allow-other-keys)
  (:documentation
   "Compiles the Tridash expression EXPR to a CL expression. If the
    :TAIL-POSITION-P argument is provided and is true, then EXPRESSION
    appears in tail position."))

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
       (aif (get name *tridash-cl-functions*)
            `(thunk (,it ,@(map #'tridash->cl arguments)))
            (error "External meta-node ~a not supported." name)))

      ((type meta-node)
       `(thunk (call-tridash-meta-node ,meta-node (list ,@(map #'tridash->cl arguments)))))

      (_
       `(thunk (funcall (resolve ,(tridash->cl meta-node)) ,@(map #'tridash->cl arguments)))))))

(defun make-tail-call (arguments)
  "Generates a tail self call with arguments ARGUMENTS."

  `(progn
     (psetf ,@(mappend #2`(,(get a1 *operand-vars*) ,(tridash->cl a2))
                       (operands *current-meta-node*) arguments))
     (go ,+tridash-recur-tag+)))


(defmethod tridash->cl ((if if-expression) &key)
  (with-struct-slots if-expression- (condition then else)
      if

    `(thunk
      (,(id-symbol "if")
        ,(tridash->cl condition)
        ,(tridash->cl then)
        ,(tridash->cl else)))))

(defmethod tridash->cl ((object object-expression) &key)
  "Generates a CL expression that creates a `HASH-MAP'."

  (flet ((make-entry (pair)
           (destructuring-bind (key value) pair
             `(cons ',key ,(tridash->cl value)))))

    `(thunk (alist-hash-map (list ,@(map #'make-entry (object-expression-entries object)))))))

(defmethod tridash->cl ((member member-expression) &key)
  "Generates a GET CL function expression."

  (with-struct-slots member-expression- (object key)
      member

    `(thunk (get ',key (resolve ,(tridash->cl object))))))

(defmethod tridash->cl ((expr catch-expression) &key)
  "Generates a CL CATCH expression with the tag symbol given by
   +FAIL-CATCH-TAG+. If the CATCH expression returns the catch tag
   identifier, the expression in the `CATCH' slot is evaluate."

  (with-struct-slots catch-expression- (main catch)
      expr

    `(thunk
      (handler-case (resolve ,(tridash->cl main))
        (tridash-fail () ,(tridash->cl catch))))))

(defmethod tridash->cl ((fail fail-expression) &key)
  "Generates a CL THROW expression which throws the value of
   +FAIL-CATCH-TAG+."

  '(thunk (error 'tridash-fail)))

(defmethod tridash->cl ((group expression-group) &key tail-position-p)
  (tridash->cl (expression-group-expression group) :tail-position-p tail-position-p))


(defmethod tridash->cl ((ref meta-node-ref) &key)
  (with-struct-slots meta-node-ref- (node outer-nodes)
      ref

    (when outer-nodes
      (error "Cannot reference outer-nodes in CL backend."))

    (match node
      ((external-meta-node name)
       (aif (get name *tridash-cl-functions*)
            `#',it
            (error "External meta-node ~a not supported." name)))

      ((type meta-node)
       (with-gensyms (args)
         `(lambda (&rest ,args)
            (call-tridash-meta-node ,node ,args)))))))

(defmethod tridash->cl (literal &key)
  (match literal
    ((or (type number) (type string))
     literal)

    (_ `',literal)))


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

;;; Lists

(define-tridash-function |cons| (a b) cons)
(define-tridash-function |list| (&rest xs) list)
