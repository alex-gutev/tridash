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

(defvar *return-nil* nil
  "Flag: If true NIL expressions are compiled to NIL values, otherwise
   they are compiled to a fail thunk.")



;;; Error Conditions

(define-condition unsupported-meta-node-error (undefined-external-meta-node-error)
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
  ((meta-node :initarg :meta-node
              :reader meta-node
              :documentation "Name of the meta-node."))

  (:documentation
   "Error condition: Outer nodes referenced from a macro meta-node."))

(defmethod print-object ((e macro-outer-node-error) stream)
  (format stream "Macro meta-node ~a references nodes in the global scope."
          (meta-node e)))


(define-condition compile-meta-node-loop-error (semantic-error)
  ((meta-node :initarg :meta-node
              :reader meta-node
              :documentation "The meta-node."))

  (:documentation
   "Error condition: A meta-node is used in a macro used within
    itself, leading to a compilation loop."))

(defmethod print-object ((e compile-meta-node-loop-error) stream)
  (format stream "Meta-node ~a invoked during the macro expansion of itself."
          (meta-node e)))



;;; Calling Tridash Meta-Nodes from CL

(defun make-macro-function (meta-node)
  "Creates the macro-node function, which compiles META-NODE to a CL
   function, calls the function and processes the result."

  (lambda (operator operands module)
    (declare (ignore operator))

    (let ((*tridash-call-reason* :macro)
          (operands (map #'unwrap-declaration operands)))
      (check-arity meta-node operands)

      (-<> (make-rest-args meta-node operands)
           (call-tridash-meta-node meta-node <>)
           resolve
           (process-macro-expansion module)))))


(defgeneric call-meta-node (meta-node args &key &allow-other-keys)
  (:documentation
   "Calls the `META-NODE' META-NODE with arguments ARGS. Arity checks
    are performed and the rest arguments are grouped into a single
    list. Can be used to call both `META-NODE's and
    `EXTERNAL-META-NODE's."))

(defmethod call-meta-node :around ((meta-node meta-node) args &key (resolve t))
  "Performs arity checking and calls groups the rest arguments into a
   single list, before calling the next method with the new argument
   list."

  (check-arity meta-node args)

  (let ((value (call-next-method meta-node (make-rest-args meta-node args))))
    (if resolve
        (resolve value)
        value)))

(defmethod call-meta-node ((meta-node meta-node) args &key)
  (call-tridash-meta-node meta-node args))

(defmethod call-meta-node ((meta-node external-meta-node) args &key)
  (apply (external-meta-node-cl-function (name meta-node)) args))


(defun call-tridash-meta-node (meta-node args)
  "Calls the meta-node META-NODE with arguments ARGS. If the meta-node
   has been compiled to a CL function, the function is called
   otherwise it is compiled to a CL function by
   COMPILE-META-NODE-FUNCTION."

  (apply (compile-meta-node-function meta-node) args))



;;; Compiling Meta-Nodes to CL Functions

(defun compile-meta-node-function (meta-node)
  "Compiles META-NODE to a CL function."

  (or (meta-node-cl-function meta-node)
      (setf (meta-node-cl-function meta-node)
            (compile nil (tridash->cl-function meta-node)))))

(defun meta-node-cl-function (meta-node)
  "Returns the compiled CL function of the meta-node."

  (get :cl-function (attributes meta-node)))

(defun (setf meta-node-cl-function) (fn meta-node)
  "Sets the compiled CL function of META-NODE to FN."

  (setf (get :cl-function (attributes meta-node)) fn))


(defun build-used-meta-nodes (meta-node &optional (visited (make-hash-set)))
  "Builds each meta-node which is used by META-NODE"

  (unless (memberp meta-node visited)
    (nadjoin meta-node visited)

    (unless (external-meta-node? meta-node)
      (foreach
       (lambda (meta-node)
         (build-meta-node meta-node)
         (build-used-meta-nodes meta-node visited))

       (->> meta-node
            definition
            used-meta-nodes-in-definition)))))

(defun finish-build-used-meta-nodes (meta-node &optional (visited (make-hash-set)))
  "Peforms the final built steps for META-NODE and each meta-node used
   by META-NODE."

  (unless (memberp meta-node visited)
    (nadjoin meta-node visited)

    (unless (external-meta-node? meta-node)
      (finish-build-meta-node meta-node)

      (->> meta-node
           definition
           used-meta-nodes-in-definition
           (foreach (rcurry #'finish-build-used-meta-nodes visited))))))

(defgeneric used-meta-nodes-in-definition (definition)
  (:documentation
   "Returns the meta-nodes used in the meta-node definition DEFINITION.")

  (:method ((definition module))
    (->> definition
         nodes
         map-values
         used-meta-nodes))

  (:method ((definition flat-node-table))
    (->> definition
         nodes
         used-meta-nodes)))



;;; Compiling Tridash Expressions to CL

(defvar *tridash-cl-functions* (make-hash-map)
  "Map mapping Tridash meta-node identifiers to the corresponding CL
   function identifiers.")

(defvar *expression-blocks* (make-hash-map)
  "Map where each key is an `EXPRESSION-BLOCK' and each corresponding
   value is a list of the form (VAR EXPRESSION) where VAR is the name
   of the variable with which the value of the expression block is
   referenced and EXPRESSION is the actual compiled expression which
   computes the block's value.")


;;; Function Creation

(defun tridash->cl-function (meta-node)
  "Returns a CL LAMBDA expression which is compiled from META-NODE."

  (build-meta-node meta-node)
  (build-used-meta-nodes meta-node)

  ;; Finish build will not perform any final build steps unless there
  ;; are still some meta-nodes, used by META-NODE, which have not been
  ;; built yet.
  (finish-build-used-meta-nodes meta-node)

  (let ((*current-meta-node* meta-node)
        (*operand-vars* (make-hash-map))
        (*expression-blocks* (make-hash-map))
        (*return-nil* nil))

    (flet ((add-operand (operand)
             (ensure-get operand *operand-vars* (gensym (mkstr operand)))))

      (foreach #'add-operand (operand-node-names meta-node))
      (foreach #'add-operand (outer-node-operand-names meta-node))

      (let ((main (->
                   (contexts meta-node)
                   map-values
                   first
                   value-function
                   (tridash->cl :thunk nil))))

        `(lambda ,(make-meta-node-lambda-list meta-node)
           (declare (ignorable ,@(map-values *operand-vars*)))
           (let ,(map #'first (map-values *expression-blocks*))
             ,@(map #`(setf ,(first a1) ,(second a1)) (map-values *expression-blocks*))
             ,main))))))

(defun get-operand-var (operand)
  "Returns the name of the variable in which the value of the operand
   OPERAND is stored."

  (get operand *operand-vars*))

(defun make-meta-node-lambda-list (meta-node)
  "Creates the lambda-list of the CL function of META-NODE."

  (match-state (operands meta-node)
    :start 'required

    (required
     (cons (and (type node) arg) args)
     :from required

     (cons (get-operand-var (name arg)) (next args)))

    (optional
     (and (cons (list* (eql +optional-argument+) _) _)
          args)
     :from required

     (cons '&optional (next args)))

    (optional
     (cons (list* (eql +optional-argument+) arg (or (list value) nil))
           args)
     :from optional

     (when (has-nodes? value)
       (error 'macro-outer-node-error))

     (cons (list (get-operand-var (name arg))
                 (if value
                     (tridash->cl value)
                     (fail-thunk +fail-type-no-value+)))
           (next args)))

    (rest
     (and (list* (list (eql +rest-argument+) _) _) args)
     :from required

     (cons '&optional (next args)))

    (rest
     (cons (list (eql +rest-argument+) arg) args)

     (cons
      (list (get-operand-var (name arg)) '(empty-list))
      (next args)))

    (outer
     nil
     :from required

     (awhen (next nil)
       (cons '&optional (next nil))))

    (outer
     nil

     (map
      (lambda (node)
        (list
         (->> meta-node
              outer-nodes
              (get node)
              name
              get-operand-var)

         (handler-case
             ;; There is no issue if the outer node is coalesced as
             ;; that can only happen if the outer-node is defined in
             ;; an enclosing, non-global, scope of META-NODE. In that
             ;; case, however, a value will be provided for the
             ;; outer-node as an argument anyway.
             (tridash->cl (constant-node-value node))

           (not-constant-context ()
             `(error 'macro-outer-node-error :meta-node ,meta-node)))))

      (outer-node-references meta-node)))))


(define-condition not-constant-context ()
  ()

  (:documentation
   "Condition which indicates that a node or context does not have a
    constant value."))

(defun constant-node-value (node)
  "Returns the value expression of NODE if it has a constant value. If
   it does not have a constant value the condition
   `not-constant-context' is signalled.

   A node is considered to have a constant value if it is not an input
   node one of the following is satisfied:

   - It has a single context with no operands, or at least one of the
     operands of the remaining contexts does not have a constant
     value.

   - One of its contexts has operands which all have constant
     values. The rest of the contexts must have at least one operand
     which does not have a constant value."

  (labels ((context-value (context)
             (if (emptyp (operands context))
                 (value-function context)

                 (replace-dependencies
                  (value-function context)
                  (map
                   (lambda (operand)
                     (cons (car operand) (constant-node-value (car operand))))

                   (operands context)))))

           (replace-dependencies (expression operands)
             (typecase expression
               (node-link
                (get (node-link-node expression) operands))

               (otherwise
                (map-expression! (rcurry #'replace-dependencies operands) expression)))))

    (with-slots (contexts) node
      (when (input-node? node)
        (error 'not-constant-context))

      (nlet-tail get-value
          ((contexts (map-values contexts))
           (value nil)
           (found 0))

        (when (> found 1)
          (error 'not-constant-context))

        (cond
          (contexts
           (handler-case
               (get-value
                (rest contexts)
                (context-value (first contexts))
                (incf found))

             (not-constant-context ()
               (get-value (rest contexts) value found))))

          ((= found 1)
           value)

          (t
           (error 'not-constant-context)))))))


;;; Thunks

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


;;; Argument and Node Referneces

(defmethod tridash->cl ((link node-link) &key)
  "Returns the variable corresponding to the linked node by looking up
   the node's identifier in *OPERAND-VARS*"

  (with-slots (name) (node-link-node link)
    (ensure-get name *operand-vars* (gensym (mkstr name)))))

(defmethod tridash->cl ((ref node-ref) &key)
  (node-ref-node ref))


;;; Object Expressions

(defmethod tridash->cl ((object object-expression) &key)
  "Generates a CL expression that creates a `HASH-MAP'."

  (flet ((make-entry (pair)
           (let ((*return-nil* nil))
             (destructuring-bind (key value) pair
               `(cons ',key ,(tridash->cl value :thunk t))))))

    `(alist-hash-map (list ,@(map #'make-entry (object-expression-entries object))))))

;;; Expression Blocks

(defmethod tridash->cl ((block expression-block) &key)
  (with-struct-slots expression-block- (expression count) block
    (acond
      ((= count 1)
       (tridash->cl expression :thunk nil))

      ((get block *expression-blocks*)
       (first it))

      (t
       (let ((var (gensym))
             (expr (tridash->cl expression :thunk t)))

         (setf (get block *expression-blocks*)
               (list var expr))

         var)))))


;;; Functor Expressions and Meta-Node References

(defmethod tridash->cl ((functor functor-expression) &key)
  "If the operator of the functor is an `EXTERNAL-META-NODE',
   generates a CL expression which calls the corresponding CL
   function, found by looking up the name of the meta-node in
   +TRIDASH-CL-FUNCTIONS+. If there is no corresponding CL function an
   error condition is signalled.

   If the operator is a `META-NODE', generates a
   CALL-TRIDASH-META-NODE expression."

  (with-struct-slots functor-expression- (meta-node arguments outer-nodes)
      functor

    (match meta-node
      ((external-meta-node name)
       (let ((*return-nil* t))
         (->> (make-meta-node-arguments meta-node arguments)
              (list* (external-meta-node-cl-function name)))))

      ((type meta-node)
       (let ((*return-nil* nil))
         (-<>> (make-meta-node-arguments meta-node arguments)
               (append <> (make-outer-operands meta-node outer-nodes))
               (list* 'list)
               (list 'call-tridash-meta-node meta-node))))

      (_
       `(call-node ,(tridash->cl meta-node :thunk nil)
                   (list ,@(map #'tridash->cl arguments)))))))

(defun make-meta-node-arguments (meta-node arguments)
  "Returns expressions, wrapped in `THUNK's if necessary, for the
   arguments ARGUMENTS of the `FUNCTOR-EXPRESSION' with operator
   META-NODE."

  (loop
     for (strict? . strict-args) = (strict-arguments meta-node) then strict-args
     for arg in arguments
     while (or arg (not *return-nil*))
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

(defun make-outer-operands (meta-node outer-nodes)
  (let ((strict (tridash.frontend.strictness:strict-outer-operands meta-node)))
    (map
     (lambda (node)
       (tridash->cl (get node outer-nodes) :thunk (not (get node strict))))
     (outer-node-references meta-node))))

(defun external-meta-node-cl-function (name)
  "Returns the name of the CL function implementing the
   `EXTERNAL-META-NODE' with name NAME."

  (or (get name *tridash-cl-functions*)
      (error 'unsupported-meta-node-error :node-name name)))


(defmethod tridash->cl ((ref meta-node-ref) &key)
  (with-struct-slots meta-node-ref- (node optional outer-nodes)
      ref

    (with-gensyms (args)
      (let ((apply-args (make-meta-node-ref-arguments node args)))
        `#'(lambda (&rest ,args)
             (check-arity ,node ,args)

             ,(ematch node
                ((external-meta-node name)
                 `(apply #',(external-meta-node-cl-function name) ,apply-args))

                ((type meta-node)
                 (let ((*return-nil* nil))
                   (multiple-value-bind (lambda-list vars)
                       (destructure-meta-node-args (operands node) optional)

                     `(destructuring-bind ,lambda-list ,args
                       (call-tridash-meta-node
                        ,node

                        (list ,@vars ,@(make-outer-operands node outer-nodes)))))))))))))

(defun destructure-meta-node-args (operands optional)
  "Creates the destructuring lambda-list for the meta-node argument
   list OPERANDS. OPTIONAL is the list containing the default values
   of the optional arguments."

  (let ((lambda-list (make-collector nil))
        (vars (make-collector nil)))

    (labels ((process-required (args)
               (match args
                 ((list* (list* (eql +optional-argument+) _) _)
                  (accumulate lambda-list '&optional)
                  (process-optional args optional))

                 ((list* (list* (eql +rest-argument+) _) _)
                  (make-rest))

                 ((list* (not (type cons)) args)
                  (let ((var (gensym)))
                    (accumulate lambda-list var)
                    (accumulate vars var))

                  (process-required args))))

             (process-optional (args optional)
               (match args
                 ((list* (list* (eql +optional-argument+) _) args)
                  (let ((var (gensym)))
                    (accumulate lambda-list (list var (tridash->cl (first optional))))
                    (accumulate vars var))

                  (process-optional args (rest optional)))

                 ((list* (list* (eql +rest-argument+) _) _)
                  (make-rest))))

             (make-rest ()
               (accumulate lambda-list '&rest)

               (let ((var (gensym))
                     (rest-var (gensym)))

                 (accumulate lambda-list var)
                 (accumulate lambda-list '&aux)
                 (accumulate lambda-list `(,rest-var (or ,var (empty-list))))

                 (accumulate vars rest-var))))

      (process-required operands)
      (values (collector-sequence lambda-list)
              (collector-sequence vars)))))

(defun has-nodes? (expression)
  "Checks whether EXPRESSION references any nodes."

  (walk-expression
   (lambda (x)
     (if (typep x 'node-link)
         (return-from has-nodes? t)
         t))
   expression))

(defun make-meta-node-ref-arguments (meta-node args)
  "Makes the argument list to be passed to the `META-NODE', inside a
   `META-NODE-REF'. ARGS is the name of the variable storing the
   meta-node's argument list."

  (if (null (cdr (meta-node-arity meta-node)))
      `(make-rest-args ',meta-node ,args)
      args))

(defun make-rest-args (meta-node args)
  "Group the arguments in ARGS, which correspond to the rest
   arguments, in a single list."

  (aif (position +rest-argument+ (operands meta-node) :key #'ensure-car)
       (group-rest-args args it)
       args))

(defun group-rest-args (args n)
  "Returns a list in which the elements after the N'th element of ARGS
   are grouped into a single list element. If ARGS has less than N
   elements it is returned as is."

  (let ((c (make-collector nil)))
    (loop
       for rest on args
       for (arg) = rest
       for i = 0 then (1+ i)
       do
         (cond
           ((< i n)
            (accumulate c arg))

           (t
            (accumulate c rest)
            (loop-finish))))

    (collector-sequence c)))


;;; Argument List

(defmethod tridash->cl ((list argument-list) &key)
  (with-struct-slots argument-list- (arguments) list
    (if arguments
        (list* 'list (map #'tridash->cl (argument-list-arguments list)))
        '(empty-list))))


;;; Literals

(defmethod tridash->cl ((literal null) &key)
  (unless *return-nil*
    (fail-thunk +fail-type-no-value+)))

(defmethod tridash->cl (literal &key)
  (match literal
    ((or (type number) (type string))
     literal)

    (_ `',literal)))



;;; Macro-Writing

(defmethod process-functor ((operator (eql +quote-operator+)) args module)
  "Returns the raw argument unprocessed."

  (declare (ignore module))

  (match-syntax +quote-operator+
      ((any thing))
      args

    thing))


;;; Macro Attributes

(defmethod process-attribute (node (attribute (eql (id-symbol "MACRO"))) value (module t))
  "Sets the internal :MACRO-FUNCTION attribute to a function that
   compiles the META-NODE to a CL function, calls it and calls
   PROCESS-DECLARATION on the result."

  (when (bool-value value)
    (setf (node-macro-function node)
          (make-macro-function node))
    value))

(defmethod process-attribute ((node t) (attribute (eql (id-symbol "TARGET-TRANSFORM"))) value module)
  (process-operator-node value module))
