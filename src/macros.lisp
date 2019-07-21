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


;;;; Macro Attributes

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


;;;; Macro Function

(defun make-macro-function (meta-node)
  "Creates the macro-node function, which compiles META-NODE to a CL
   function, calls the function and processes the result."

  (let ((num-args (1- (length (operands meta-node))))
        (max-args (cdr (meta-node-arity meta-node))))

    (lambda (operator operands module)
      (declare (ignore operator))
      (check-arity meta-node operands)

      (-<> (group-rest-args operands num-args)
           (if (null max-args) <> operands)
           (call-tridash-meta-node meta-node <>)
           resolve
           (process-macro-expansion module)))))


;;;; Calling Tridash Meta-Nodes from CL

(defgeneric call-meta-node (meta-node args)
  (:documentation
   "Calls the `META-NODE' META-NODE with arguments ARGS. Arity checks
    are performed and the rest arguments are grouped into a single
    list. Can be used to call both `META-NODE's and
    `EXTERNAL-META-NODE's."))

(defmethod call-meta-node :around ((meta-node meta-node) args)
  "Performs arity checking and calls groups the rest arguments into a
   single list, before calling the next method with the new argument
   list."

  (check-arity meta-node args)

  (let ((max-args (cdr (meta-node-arity meta-node))))
    (-<>> (group-rest-args args (1- (length (operands meta-node))))
          (if (null max-args) <> args)
          (call-next-method meta-node)
          resolve)))

(defmethod call-meta-node ((meta-node meta-node) args)
  (call-tridash-meta-node meta-node args))

(defmethod call-meta-node ((meta-node external-meta-node) args)
  (apply (external-meta-node-cl-function (name meta-node)) args))


(defun call-tridash-meta-node (meta-node args)
  "Calls the meta-node META-NODE with arguments ARGS. If the meta-node
   has been compiled to a CL function, the function is called
   otherwise it is compiled to a CL function by
   COMPILE-META-NODE-FUNCTION."

  (let ((f (or (meta-node-cl-function meta-node)
               (compile-meta-node-function meta-node))))

    (with-slots (outer-nodes operands) meta-node
      ;; Check that all values are provided for all the outer-nodes
      ;; referenced by the meta-node.
      (when (and (not (emptyp outer-nodes))
                 (< (length args) (length operands)))
        (error 'macro-outer-node-error :meta-node meta-node)))

    (apply f args)))


;;;; Compiling CL function from Meta-Node

(defun compile-meta-node-function (meta-node)
  "Compiles the META-NODE meta-node to a CL function. Stores the
   compiled CL function in the :CL-FUNCTION attribute."

  (unless (external-meta-node? meta-node)
    (unless (typep (definition meta-node) 'flat-node-table)
      (build-meta-node meta-node)

      ;; Determine the meta-node's referenced outer-nodes
      (outer-node-references meta-node)

      (finish-build-meta-node meta-node)

      (build-referenced-meta-nodes meta-node))

    (or (meta-node-cl-function meta-node)
        (setf (meta-node-cl-function meta-node)
              (compile nil (tridash->cl-function meta-node))))))

(defun build-referenced-meta-nodes (meta-node)
  "Compile each meta-node used by META-NODE to a CL Function."

  (flet ((build-meta-nodes (expression)
           (match expression
             ((and (type meta-node) meta-node)
              (compile-meta-node-function meta-node))

             ((meta-node-ref- node)
              (compile-meta-node-function node)))
           t))

    (unless (external-meta-node? meta-node)
      (->> meta-node
           contexts
           first
           cdr
           value-function
           (walk-expression #'build-meta-nodes)))))

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
  ((meta-node :initarg :meta-node
              :reader meta-node
              :documentation "Name of the meta-node."))

  (:documentation
   "Error condition: Outer nodes referenced from a macro meta-node."))

(defmethod print-object ((e macro-outer-node-error) stream)
  (format stream "Macro meta-node ~a references nodes in the global scope."
          (meta-node e)))


;;;; Compiling Tridash Expressions to CL

(defvar *tridash-cl-functions* (make-hash-map)
  "Map mapping Tridash meta-node identifiers to the corresponding CL
   function identifiers.")


(defun tridash->cl-function (meta-node)
  "Returns a CL LAMBDA expression which is compiled from META-NODE."

  (let ((*current-meta-node* meta-node)
        (*operand-vars* (make-hash-map))
        (*return-nil* nil))

    (flet ((add-operand (operand)
             (ensure-get operand *operand-vars* (gensym (mkstr operand)))))

      (foreach #'add-operand (operand-node-names meta-node))

      `(lambda ,(make-meta-node-lambda-list meta-node)
         ,(->
           (contexts meta-node)
           map-values
           first
           value-function
           (tridash->cl :thunk nil))))))

(defun get-operand-var (operand)
  "Returns the name of the variable in which the value of the operand
   OPERAND is stored."

  (get operand *operand-vars*))

(defun make-meta-node-lambda-list (meta-node)
  "Creates the lambda-list of the CL function of META-NODE."

  (match-state (operands meta-node)
    :start 'required

    (required
     (cons (and (type symbol) arg) args)
     :from required

     (cons (get-operand-var arg) (next args)))

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

     (cons (list (get-operand-var arg) (tridash->cl value))
           (next args)))

    (rest
     (and (list (list (eql +rest-argument+) _)) args)
     :from required

     (cons '&optional (next args)))

    (rest
     (list (list (eql +rest-argument+) arg))

     (list (get-operand-var arg)))

    (outer
     (and (cons (cons (eql +outer-node-argument+) _) _) args)
     :from required

     (cons '&optional (next args)))

    (outer
     (cons (and (cons (eql +outer-node-argument+) _) arg) rest)
     :from (optional outer rest)

     (cons (get-operand-var arg) (next rest)))))


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

(defmethod tridash->cl ((ref node-ref) &key)
  (node-ref-node ref))


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
       (let ((*return-nil* t))
         (->> (make-meta-node-arguments meta-node arguments)
              (list* (external-meta-node-cl-function name)))))

      ((type meta-node)
       (let ((*return-nil* nil))
         (->> (make-meta-node-arguments meta-node arguments)
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
           (let ((*return-nil* nil))
             (destructuring-bind (key value) pair
               `(cons ',key ,(tridash->cl value :thunk t))))))

    `(alist-hash-map (list ,@(map #'make-entry (object-expression-entries object))))))

(defmethod tridash->cl ((block expression-block) &key)
  (tridash->cl (expression-block-expression block)
               :thunk nil))


(defmethod tridash->cl ((ref meta-node-ref) &key)
  (with-struct-slots meta-node-ref- (node optional outer-nodes)
      ref

    (with-gensyms (args)
      (let ((apply-args (make-meta-node-arg-list node args)))
        `#'(lambda (&rest ,args)
             (check-arity ,node ,args)
             ,(ematch node
                ((external-meta-node name)
                 `(apply #',(external-meta-node-cl-function name) ,apply-args))

                ((type meta-node)
                 (let ((*return-nil* nil))
                   `(call-tridash-meta-node
                     ,node
                     ,(if outer-nodes
                          `(append ,apply-args (list ,@(map #'tridash->cl outer-nodes)))
                          apply-args))))))))))

(defun has-nodes? (expression)
  "Checks whether EXPRESSION references any nodes."

  (walk-expression
   (lambda (x)
     (if (typep x 'node-link)
         (return-from has-nodes? t)
         t))
   expression))

(defun make-meta-node-arg-list (meta-node args)
  "Makes the argument list to be passed to the `META-NODE', inside a
   `META-NODE-REF'. ARGS is the name of the variable storing the
   meta-node's argument list."

  (if (null (cdr (meta-node-arity meta-node)))
      `(group-rest-args ,args ,(1- (length (operands meta-node))))
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


(defmethod tridash->cl ((list argument-list) &key)
  (list* 'list (map #'tridash->cl (argument-list-arguments list))))

(defmethod tridash->cl ((literal null) &key)
  (unless *return-nil*
    (fail-thunk)))

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
