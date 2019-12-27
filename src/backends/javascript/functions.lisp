;;;; functions.lisp
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

;;;; Value Function Expressions to JavaScript Compiler Functions

(in-package :tridash.backend.js)

(in-readtable cut-syntax)


(defconstant +js-function-type-checks+
  (alist-hash-map
   '(("+" :real :real)
     ("-" :real :real)
     ("*" :real :real)
     ("/" :real :real)
     ("%" :real :real)
     ("<" :real :real)
     (">" :real :real)
     ("<=" :real :real)
     (">=" :real :real)

     ("!" . t)))

  "Map mapping JS function names to a list indicating the type checks
   which should be performed for the arguments passed to that
   function, where the type is one of the keys in
   +TYPE-CHECK-FUNCTIONS+. A mapped value of T, as opposed to a list,
   indicates that no type checking should be performed however the
   function call should be surround in a try-catch block to catch any
   failures occurring from resolving the arguments.")

(defconstant +type-check-functions+
  (alist-hash-map
   '((:real . "Tridash.check_number")
     (:value . "Tridash.check_value")))

  "Map mapping type keywords to JS type checking function names.")

(defvar *protect* t
  "Flag indicating whether the expression, currently being compiled,
   should be wrapped in a try-catch block, which returns a 'failing'
   thunk in case of an error.")


;;; Meta-Node Functions

(defun create-function-meta-node (meta-node)
  "Generates the value-function of the `meta-node' META-NODE. This can
   only be used if META-NODE has no subnodes, other than its operand
   nodes."

  (with-slots (contexts) meta-node
    (let* ((operands (append (operand-node-names meta-node)
                             (outer-node-operand-names meta-node)))
           (context (cdr (first contexts)))
           (op-vars (make-operand-blocks operands)))

      (with-slots (value-function) context
        (labels ((get-input (link)
                   (cdr (assoc (name (node-link-node link)) op-vars :test #'equal))))

          (let ((body (compile-function value-function #'get-input)))

            (list
             (js-function
              (meta-node-id meta-node)
              (map (compose #'value-block-expression #'cdr) op-vars)

              body)

             (store-in-public-nodes meta-node (meta-node-id meta-node)))))))))

(defun make-operand-blocks (operands)
  "Creates a `value-block' object for each each operand. Returns an
   association list where each key is the operand and the associated
   value is the `value-block' object."

  (loop
     for operand in operands
     for i = 0 then (1+ i)
     collect
       (cons operand
             (make-value-block :expression (mkstr "a" i)))))

(defun store-in-public-nodes (node expr)
  "If NODE has a :PUBLIC-NAME attribute returns an assignment
   expression which stores EXPR in 'Tridash.nodes' under the key which
   value of the :PUBLIC-NAME attribute"

  (awhen (attribute :public-name node)
    (-<> (js-member +tridash-namespace+ "nodes")
         (js-element (js-string it))
         (js-call "=" <> expr))))


;;; Node Context Functions

(defun create-compute-function (context)
  "Generates the value computation function of CONTEXT. The anonymous
   function expression is returned in the first value. The second
   return value is a MAP mapping `node' objects, of which the context
   references the previous value, to indices within the
   'previous_values' array."

  (symbol-macrolet ((values-var "values"))
    (let ((operands (make-hash-map))
          (uses-old-value?)
          (references-this?)
          (previous-nodes (make-hash-map)))

      (flet ((get-operand (operand)
               (ematch operand
                 ((type node-link)
                  (ensure-get operand operands
                    (make-link-expression context operand values-var)))

                 ((list :previous-value (node-ref node))
                  (setf references-this? t)

                  (ensure-get node operands
                    (make-value-block
                     :expression
                     (->> (ensure-get node previous-nodes
                            (length previous-nodes))

                          (js-element
                           (js-member "self" "previous_values")))))))))

        (with-slots (value-function) context
          (let* ((*protect* nil)
                 (body (compile-function value-function #'get-operand)))

            (when value-function
              (values
               (js-lambda
                (list values-var)
                (append
                 (when references-this?
                   (list
                    (js-var "self" "this")))
                 body))

               previous-nodes))))))))

(defun make-link-expression (context link &optional (values-var "values"))
  "Creates an expression which references the node with `NODE-LINK'
   LINK linked to the context CONTEXT."

  (make-value-block
   :expression

   (js-element
    values-var

    (->> link
         node-link-node
         (dependency-index context)))))



;;; Compiling Functions

(defclass function-block-state ()
  ((input
    :initarg :input
    :accessor input
    :documentation
    "Function which should return an expression that references the
     value of the dependency corresponding to the `NODE-LINK' object
     passed as an argument.")

   (var-counter
    :initform 0
    :accessor var-counter
    :documentation
    "Variable identifier counter. Appended as a suffix to variables
     introduced in a function.")

   (blocks
    :initform (make-hash-map)
    :accessor blocks
    :documentation
    "Map from `expression-block' objects to the `value-block' objects
     containing the instructions for computing their values.")

   (strict-blocks
    :accessor strict-blocks
    :initarg :strict-blocks
    :documentation
    "Strict expression of the `expression-block' objects in the
     function's expression."))

  (:documentation
   "Compilation state for a single function."))

(defstruct value-block
  "Represents a block of instructions for computing the value of an
   expression.

   VARIABLE is the name of the variable in which the result is
   stored. If NIL then the result is not stored in a variable but is
   computed directly by EXPRESSION.

   EXPRESSION is a JS expression which references the result of the
   expression.

   STATEMENTS is the list of JS statements which compute the
   expression's value.

   OPERANDS is the list of `value-block' objects, of which the results
   are referenced in STATEMENTS.

   BLOCKS is a map from `expression-block' objects to a list storing
   the corresponding `value-block' object and the number of references
   to the block's value.

   COMMON-P is a flag indicating whether the value computed by this
   block is referenced by more than one block, i.e. is a compiled
   `expression-block'.

   PROTECT-P is a flag indicating whether the statements of this block
   should be surrounded in a try/catch to capture failures."

  operands

  variable
  expression

  statements

  (blocks (make-hash-map))

  common-p
  protect-p)

(defvar *function-block-state* nil
  "Compilation state for the function currently being compiled.")


(defun compile-function (expression get-input &key (return-value t) ((:state *function-block-state*) (make-instance 'function-block-state)))
  "Generates the body of the value computation function
   FUNCTION. GET-INPUT is a function which should return an expression
   that references the value of the dependency corresponding to the
   `node-link' object passed as an argument.

   If :RETURN-VALUE is T the last statement in the body is a return
   statement that returns the value of EXPRESSION. If :RETURN-VALUE is
   NIL the body does not contain a return statement and the expression
   which references EXPRESSION's value is returned in the second value.

   The `function-block-state' can be specified using the :STATE
   argument."

  (setf (input *function-block-state*) get-input)
  (setf (strict-blocks *function-block-state*)
        (let ((tridash.frontend.strictness:*analyze-nodes* nil)
              (tridash.frontend.strictness:*return-blocks* t))
          (analyze-expression expression)))

  (make-function-body
   (compile-expression expression :protect *protect*)
   :return-value return-value))

(defun make-function-body (block &key (replace-blocks t) (return-value t))
  "Returns the list of statements making up the body of a function
   with return value given by the `value-block' object BLOCK.

   If REPLACE-BLOCKS is true (default) any references to
   `value-block's, within the AST nodes, are replaced with their
   corresponding expressions.

   If :RETURN-VALUE is T the last statement in the body is a return
   statement that returns the value of EXPRESSION. If :RETURN-VALUE is
   NIL the body does not contain a return statement and the expression
   which references EXPRESSION's value is returned in the second value."

  (merge-protect-blocks block)
  (inline-blocks block)
  (move-return-in-if block)

  (awhen (and return-value (value-block-expression block))
    (appendf
     (value-block-statements block)
     (list
      (js-return it))))

  (values
   (-> block
       flatten-block
       (extract-statements replace-blocks))
   (replace-value-blocks (value-block-expression block))))

(defun flatten-block (block)
  "Returns a list of all the blocks which are required for computing
   the value of BLOCK. The last element of the list is BLOCK, itself.

   Compiled `expression-block's of which the count does not equal the
   reference within BLOCK, are not included in the list."

  ;; 1. Remove from OPERANDS any expression blocks in which the
  ;; reference count within the expression does not equal the
  ;; expression block count.
  ;;
  ;; 2. Prepend all `expression-blocks' of which the reference count
  ;; equals the expression block's count.

  (flet ((prepend-block? (block)
           (destructuring-bind (expression-block count block) block
             (declare (ignore block))
             (= (expression-block-count expression-block) count))))

    (append
     (-<>> block
           value-block-blocks
           (remove-if-not #'prepend-block?)
           map-values
           (map-extend (compose #'flatten-block #'second)))

     (->> block
          value-block-operands
          (remove-if #'value-block-common-p)
          (map-extend #'flatten-block))

     (list block))))

(defun extract-statements (blocks &optional (replace-blocks t))
  "Returns the list of statements of each block in BLOCKS.

   If REPLACE-BLOCKS is true (default) any references to
   `value-block's, within the AST nodes, are replaced with their
   corresponding expressions."

  (labels ((extract-statements (block)
             "Returns BLOCK's statements, with references to
              `value-block's replaced and the statements wrapped in a
              try/catch if necessary."

             (with-struct-slots value-block- (protect-p statements)
                 block

               (let-if ((statements (replace-value-blocks statements) statements))
                   replace-blocks

                 (if protect-p
                     (protect-block block statements)
                     statements))))

           (block-statements (block)
             (let ((statements (extract-statements block)))
               (aif (value-block-variable block)
                    (list* (js-var it) statements)
                    statements)))

           (protect-block (block statements)
             (with-struct-slots value-block- (variable)
                 block

               (when statements
                 (let ((thunk (thunk (list (js-throw "e")))))
                   (list
                    (js-catch
                     statements
                     "e"

                     (list
                      (if variable
                          (js-call "=" variable thunk)
                          (js-return thunk))))))))))

    (map-extend #'block-statements blocks)))

(defun replace-value-blocks (node)
  "Replaces references to `value-block' objects, with their
   corresponding expressions in NODE."

  (match node
    ((value-block- expression)
     (replace-value-blocks expression))

    (_
     (map-js-node #'replace-value-blocks node))))


(defgeneric compile-expression (expression &key &allow-other-keys)
  (:documentation
   "Compiles a single expression.

    Returns a `value-block' structure containing the statements which
    compute the expression's value."))


(defun next-var (&optional (state *function-block-state*))
  (with-slots (var-counter) state
    (prog1 (mkstr "v" var-counter)
      (incf var-counter))))


;;; Thunks

(defmethod compile-expression :around (expression &key (thunk nil) ((:protect *protect*) t))
  "If :THUNK is true, compiles EXPRESSION to a thunk function."

  (let* ((*protect* (and *protect* (not thunk)))
         (block (call-next-method)))
    (with-struct-slots value-block- (statements expression common-p)
        block

      (if (and thunk
               (not common-p)
               (or statements
                   (not (non-computing? expression))))
          (make-thunk block)
          block))))

(defun make-thunk (block)
  (let* ((ref-blocks
          (remove-if
           (lambda (pair)
             (= (expression-block-count (first pair))
                (second pair)))
           (value-block-blocks block))))

    (make-value-block
     :expression
     (thunk
      (make-function-body block :replace-blocks nil))

     :blocks ref-blocks
     :operands (map #'second (map-values ref-blocks)))))

(defun thunk (statements)
  (->> statements
       (js-lambda nil)
       list
       (js-new +thunk-class+)))


(defun non-computing? (expression)
  "Returns true if the JS expression EXPRESSION does not compute a new
   value but simply references an existing value.

   Variable references, anonymous functions and literal values are
   always considered non-computing. Member and element expressions are
   considered non-computing if the object and element expressions are
   non-computing. Function calls, new expressions, object and array
   literals are not considered non-computing."

  (match expression
    ((js-new- (operator (equal +thunk-class+)))
     t)

    ((type (or js-call js-new js-object js-array))
     nil)

    ((js-member- object field)
     (and (non-computing? object)
          field))

    ((js-element- object element)
     (and (non-computing? object)
          (non-computing? element)))

    ((type expression) t)))


;;; Operand References

(defmethod compile-expression ((link node-link) &key)
  (with-slots (input) *function-block-state*
    (funcall input link)))

(defmethod compile-expression ((self (eql :self)) &key)
  (with-slots (input) *function-block-state*
    (funcall input :self)))


;;; Expression Blocks

(defmethod compile-expression ((block expression-block) &rest args &key)
  (with-struct-slots expression-block- (count expression) block
    (if (> count 1)
        (compile-expression-block block)
        (apply #'compile-expression expression args))))

(defun compile-expression-block (block)
  (with-slots (blocks strict-blocks) *function-block-state*
    (with-struct-slots expression-block- (count expression) block
      (or (get block blocks)

          (let* ((result (next-var))
                 (vblock
                  (make-value-block
                   :variable result
                   :expression result
                   :common-p t)))

            (setf (get block blocks) vblock)

            (let ((value
                   (compile-expression
                    expression
                    :thunk (not (strict? strict-blocks block)))))

              (setf (value-block-operands vblock) (list value))

              (setf (value-block-blocks vblock)
                    (copy (value-block-blocks value)))
              (setf (get block (value-block-blocks vblock))
                    (list 1 vblock))

              (setf (value-block-statements vblock)
                    (list
                     (js-call "=" result value)))

              vblock))))))

(defmethod compile-expression ((cycle cyclic-reference) &key)
  "Handles cyclic references. Returns the `value-block' of the
   referenced expression."

  (with-struct-slots cyclic-reference- (expression) cycle
    (check-type expression expression-block)

    (let ((block (get expression (blocks *function-block-state*))))
      (assert block)
      block)))


;;; Functor Expressions

(defmethod compile-expression ((expression functor-expression) &key)
  (with-struct-slots functor-expression- (meta-node arguments outer-nodes)
      expression

    (compile-functor-expression meta-node arguments outer-nodes)))

(defgeneric compile-functor-expression (operator arguments outer-nodes)
  (:documentation
   "Generates code for a functor expression consisting of OPERATOR
    applied to ARGUMENTS. OUTER-NODES is a map mapping the outer nodes
    to the corresponding Tridash expressions which compute their
    values."))


;;;; External Meta-Node Operator

(defmethod compile-functor-expression ((meta-node external-meta-node) arguments outer-nodes)
  (declare (ignore outer-nodes))

  (match meta-node
    ((eq (get 'if *core-meta-nodes*))
     (compile-if-expression arguments))

    ((eq (get :member *core-meta-nodes*))
     (destructuring-bind (object key) arguments
       (call-next-method
        meta-node
        (list object (if (symbolp key) (symbol-name key) key)))))

    ((eq (get :previous-value *core-meta-nodes*))
     (funcall (input *function-block-state*)
              (list :previous-value (first arguments))))

    (_
     (call-next-method meta-node (remove-nil-arguments arguments)))))

(defun remove-nil-arguments (arguments)
  "Removes NIL's from the end of the list ARGUMENTS."

  (flet ((null-arg (arg)
           (or (null arg)
               (and (argument-list-p arg)
                    (null (argument-list-arguments arg))))))

    (let ((first-nil (position nil arguments)))
      (if (and first-nil (every #'null-arg (subseq arguments first-nil)))
          (subseq arguments 0 first-nil)
          arguments))))


;;;; Meta-Node Operator

(defmethod compile-functor-expression ((meta-node meta-node) arguments outer-nodes)
  (let ((strict-outer-nodes (strict-outer-operands meta-node))
        (protected (protected-call? meta-node)))

    (multiple-value-bind (operands blocks)
        (compile-operands
         (append arguments (outer-node-operands meta-node outer-nodes))

         (append
          (strict-arguments meta-node)
          (map (rcurry #'get strict-outer-nodes) (outer-node-references meta-node)))

         :protect
         (if protected *protect* t))

      (let ((result (next-var)))
        (make-value-block
         :variable result
         :expression result
         :operands operands
         :statements
         (list (make-meta-node-call result meta-node operands))

         :blocks blocks
         :protect-p (and protected *protect*))))))


;;;; Node Operator

(defmethod compile-functor-expression (node arguments outer-nodes)
  (declare (ignore outer-nodes))

  (multiple-value-bind (operands blocks)
      (compile-operands
       (cons node arguments)
       (cons t (make-list (length arguments) :initial-element nil)))

    (let ((result (next-var)))
      (make-value-block
       :variable result
       :expression result
       :operands operands
       :statements
       (list
        (destructuring-bind (fn &rest args) operands
          (->> (make-js-call (resolve fn) args)
               (js-call "=" result))))

       :blocks blocks
       :protect-p *protect*))))


;;;; Operands

(defun compile-operands (operands strict-operands &key (protect t))
  "Compiles the operands of an expression.

   Returns two values: a list of `value-block' objects containing the
   statements for each operand and a map (`expression-block' => count)
   containing the total number of references for each
   `expression-block' referenced with OPERANDS."

  (let ((blocks (make-hash-map)))
    (labels ((compile-operand (operand strict?)
               (aprog1
                   (compile-expression operand
                                       :thunk (not strict?)
                                       :protect protect)
                 (merge-blocks (value-block-blocks it))))

             (merge-blocks (blocks)
               (foreach #'add-block blocks))

             (add-block (block)
               (destructuring-bind (expression-block count block) block
                 (unless (= count (expression-block-count expression-block))
                   (setf (get expression-block blocks)
                         (list (+ (first (get expression-block blocks '(0))) count)
                               block))))))

      (values (map #'compile-operand operands strict-operands) blocks))))

(defun resolve (expression)
  (js-call +resolve-function+ expression))


;;;; Rest Argument Lists

(defmethod compile-expression ((arg-list argument-list) &key)
  (with-struct-slots argument-list- (arguments) arg-list
    (if arguments
        (compile-argument-list arguments)
        (make-value-block :expression (empty-list)))))

(defun compile-argument-list (arguments)
  (multiple-value-bind (operands blocks)
      (compile-operands arguments (make-list (length arguments) :initial-element nil))

    (make-value-block
     :operands operands
     :expression (js-array operands)

     :blocks blocks)))

(defun empty-list ()
  "Returns an expression which creates a failure that indicates an
   empty list."

  (js-call "Tridash.Empty"))


;;; Meta-Node Call Expressions

(defun meta-node-id (meta-node)
  "Returns the global meta-node function/operator identifier of
   META-NODE. If META-NODE is a `META-NODE' object either a new
   identifier is created or the existing identifier is returned. If
   META-NODE is an `EXTERNAL-META-NODE' the JavaScript
   function/operator name corresponding to the name of the meta-node,
   in +JS-PRIMITIVE-OPERATORS+, the node's :PUBLIC-NAME attribute, or
   the name of the meta-node itself is returned."

  (etypecase meta-node
    (external-meta-node
     (if-let (js-name (attribute :js-name meta-node))
       (aif (get js-name +js-function-type-checks+)
            (cons js-name it)
            js-name)

       (error 'undefined-external-meta-node-error
              :backend "JavaScript"
              :meta-node meta-node)))

    (meta-node
     (with-slots (meta-node-ids) *backend-state*
       (ensure-get meta-node meta-node-ids
         (mkstr "metanode" (length meta-node-ids)))))))


(defun meta-node-call (var meta-node operands)
  "Generates a JS expression or block which invokes META-NODE with
   operands OPERANDS (where each operand is a JS expression) and
   stores the result in the variable VAR."

  (match (meta-node-id meta-node)
    ((cons name t)
     (->> (map #'resolve operands)
          (make-js-call name)
          (js-call "=" var)))

    ((cons name types)
     (->> (map #'make-type-check operands types)
          (make-js-call name)
          (js-call "=" var)))

    ((or (cons name nil) name)
     (->> (make-js-call name operands)
          (js-call "=" var)))))

(defun make-type-check (expression type)
  "Makes an expression which validates that the result of EXPRESSION
   is of type TYPE."

  (let ((check-fn (get type +type-check-functions+)))
    (assert check-fn (check-fn))

    (js-call check-fn (resolve expression))))


(defparameter *meta-node-call* #'meta-node-call
  "Function of three arguments (VAR, META-NODE, OPERANDS), which
   should return a JS expression or block that invokes the meta-node
   META-NODE on operands OPERANDS and stores the result in VAR.")

(defun make-meta-node-call (var meta-node operands)
  "Returns a JS expression or block which calls META-NODE with
   OPERANDS, storing the result in VAR, using the function bound to
   *META-NODE-CALL*."

  (funcall *meta-node-call* var meta-node operands))

(defun protected-call? (meta-node)
  "Returns true if the call to META-NODE should be wrapped in a
   try/catch."

  (and (external-meta-node? meta-node)
       (consp (meta-node-id meta-node))))


;;; If Expressions

(defun compile-if-expression (arguments)
  "Generates an if-block for an IF functor expression with arguments
   ARGUMENTS."

  (multiple-value-bind (operands blocks)
      (compile-operands arguments (list t nil nil) :protect *protect*)

    (let ((result (next-var)))
      (make-value-block
       :variable result
       :expression result
       :operands operands

       :statements
       (destructuring-bind (cond then &optional else) operands

         (list
          (js-if (resolve cond)
                 (js-call "=" result then)
                 (when else
                   (js-call "=" result else)))))

       :blocks blocks
       :protect-p *protect*))))


;;; Object Expressions

(defmethod compile-expression ((object object-expression) &key)
  "Generates an expression which returns an object containing each
   field-value pair in OPERANDS."

  (with-accessors ((entries object-expression-entries)) object
    (let ((keys (map #'first entries))
          (values (map #'second entries)))

      (multiple-value-bind (values blocks)
          (compile-operands values (make-list (length values) :initial-element nil))

        (make-value-block
         :operands values

         :expression
         (js-object
          (map #'list (map #'js-string keys) values))

         ;; (->> values
         ;;      (map #'list (map #'js-string keys))
         ;;      js-object)

         :blocks blocks)))))


;;; Meta-Node References

(defmethod compile-expression ((ref meta-node-ref) &key)
  "Generates an expression which returns a function that invokes the
   meta-node referenced by REF."

  (with-struct-slots meta-node-ref- (node optional outer-nodes)
      ref

    (let ((js-name (meta-node-id node)))
      (if (= (ensure-car js-name) "-")
          (make-value-block :expression "Tridash.sub_neg")
          (make-ref-function ref)))))

(defun make-ref-function (ref)
  "Creates a JavaScript anonymous function which executes the
   meta-node referenced by the `META-NODE-REF' REF."

  (with-struct-slots meta-node-ref- (node optional outer-nodes)
      ref

    (let ((operands (append optional (outer-node-operands node outer-nodes))))
      (multiple-value-bind (operands blocks)
          (compile-operands operands (make-list (length operands) :initial-element nil))

        (make-value-block
         :operands operands

         :expression

         (let ((op-values operands)
               (fn-args (make-collector nil))
               (call-args (make-collector nil))
               (rest-arg nil))

           (nlet-tail make-args
               ((operands (operands node))
                (op-values op-values))

             (ematch operands
               ((list* (list (eql +optional-argument+) _ _) rest)
                (let ((var (next-var)))
                  (accumulate call-args var)
                  (accumulate fn-args (js-call "=" var (first op-values))))

                (make-args rest (rest op-values)))

               ((list* (list (eql +rest-argument+) _) rest)
                (let ((var (next-var)))
                  (setf rest-arg var)

                  (accumulate call-args var)
                  (accumulate fn-args (js-call "..." var)))

                (make-args rest op-values))

               ((list* (type (or symbol node)) rest)
                (let ((var (next-var)))
                  (accumulate call-args var)
                  (accumulate fn-args var))

                (make-args rest op-values))

               (nil
                (extend call-args op-values))))

           (js-lambda
            (collector-sequence fn-args)

            (list
             (make-arity-check node)

             (when rest-arg
               (js-if
                (js-call "===" (js-member rest-arg "length") 0)
                (js-call "=" rest-arg (empty-list))))

             (return-call-result
              (make-meta-node-call "result" node (collector-sequence call-args))))))

         :blocks blocks)))))

(defun make-arity-check (meta-node)
  "Generates code which checks that the correct number of arguments
   are supplied to META-NODE. This assumes that the arguments are
   stored in the `length' property of the `arguments' object. The
   generated code returns a failure of type `Arity-Error' if an
   incorrect number of arguments are supplied."

  (destructuring-bind (min . max) (meta-node-arity meta-node)
    (js-if
     (cond
       ((= min max)
        (js-call "!==" (js-member "arguments" "length") min))

       ((null max)
        (js-call "<" (js-member "arguments" "length") min))

       (t
        (js-call "||"
                 (js-call "<" (js-member "arguments" "length") min)
                 (js-call ">" (js-member "arguments" "length") max))))

     (js-return (js-call "Tridash.ArityError")))))

(defun outer-node-operands (meta-node outer-nodes)
  "Generates the JS expressions which compute the values of the outer
   node operands OUTER-NODES of the meta-node META-NODE. Returns a
   list of JS expressions which should be appended to the main
   argument list."

  (map (rcurry #'get outer-nodes) (outer-node-references meta-node)))

(defun return-call-result (statement &optional (var "result"))
  "Generates a statement(s) which returns the result of STATEMENT. If
   STATEMENT is simply an assignment to VAR, the assigned expression
   is returned."

  (match statement
    ((js-call- (operator (equal "="))
               (operands
                (list (equal var) expression)))
     (js-return expression))

    (_
     (list
      (js-var var)
      statement
      (js-return var)))))


;;; Raw Node References

(defconstant +type-node-table+ "Tridash.type_nodes"
  "Expression referencing JS object storing nodes which serve as
   types.")

(defmethod compile-expression ((ref node-ref) &key)
  "Compiles raw node references. Currently generates an expression
   which references the raw Node object or meta-node function."

  (with-struct-slots node-ref- (node) ref
    (->>
     (etypecase node
       (meta-node
        (meta-node-id node))

       (node
        (type-node-path node)))

     (make-value-block :expression))))

(defun type-node-path (node)
  "Return an expression which references the type node NODE."

  (with-slots (node-ids type-node-ids) *backend-state*
    (if (memberp node node-ids)
        (node-path node)
        (js-element +type-node-table+ (ensure-get node type-node-ids (length type-node-ids))))))

(defun create-type-nodes (state)
  "Generate the node creation code for the type nodes, stored in
   TYPE-NODE-IDS slot of the backend state STATE."

  (with-slots (type-node-ids) state
    (let ((*node-path* #'type-node-path))
      (foreach #'create-node (map-keys type-node-ids)))))


;;; Literal Values

(defmethod compile-expression ((string string) &key)
  (make-value-block :expression (js-string string)))

(defmethod compile-expression ((sym symbol) &key)
  (->> (js-string sym)
       (js-call "Tridash.get_symbol")
       (make-value-block :expression)))

(defmethod compile-expression ((chr character) &key)
  (->> (js-string chr)
       list
       (js-new "Tridash.Char")
       (make-value-block :expression)))

(defmethod compile-expression ((null null) &key)
  (make-value-block :expression (js-call "Tridash.fail" "Tridash.NoValue")))

(defmethod compile-expression ((literal number) &key)
  (make-value-block :expression literal))


;;; Optimizations

(defun inline-blocks (block)
  "For each block, referenced by BLOCK (either indirectly or through
   one of its operands), of which the statements comprise just an
   assignment to the blocks variable, the block expression is replaced
   with the result of the assignment and the block's variable and
   statements are set to NIL."

  (inline-block block)
  (inline-operands block))

(defun inline-operands (block)
  (with-struct-slots value-block- (operands protect-p) block
    (unless (value-block-common-p block)
      (->> (remove-if #'value-block-protect-p operands)
           (if protect-p operands)
           (foreach #'inline-block)))

    (foreach #'inline-operands operands)))

(defun inline-block (block)
  (with-struct-slots value-block- (variable expression statements common-p)
      block

    (match (strip-redundant statements :strip-block t)
      ((list
        (js-call- (operator (equal "="))
                  (operands (list (equal variable) result))))

       (setf expression result)
       (setf statements nil)
       (setf variable nil)))))

(defun move-return-in-if (block)
  "If BLOCK consists of a single statement which is an if block, in
   which the if and else branches comprise only an assignment of a
   value to the block's variable, the assignments within the branches
   are replaced with return statements and the block's variable is set
   to NIL."

  (with-struct-slots value-block- (variable expression statements)
      block

    (labels ((assignment-result (statement)
               (match statement
                 ((js-call- (operator (equal "="))
                            (operands
                             (list (equal variable)
                                   result)))

                  result)

                 ((js-block- (statements (list statement)))
                  (assignment-result statement)))))

      (match (strip-redundant statements :strip-block t)
        ((list
          (js-if- condition then else))

         (when-let ((then (assignment-result then))
                    (else (assignment-result else)))
           (setf statements
                 (list
                  (js-if condition
                         (js-return then)
                         (js-return else))))

           (setf variable nil)
           (setf expression nil)))

        ((list
          (js-catch- (try (list try))
                     var
                     (catch (list catch))))

         (when-let ((try (assignment-result try))
                    (catch (assignment-result catch)))

           (setf statements
                 (list
                  (js-catch
                   (list
                    (js-return try))
                   var
                   (list
                    (js-return catch)))))

           (setf variable nil)
           (setf expression nil)))))))

(defun merge-protect-blocks (block)
  "Merges the protected operands (where PROTECT-P is true) of each
   block referenced by BLOCK (either directly or indirectly through
   its operands) into the statements of the block itself, if it is
   also protected."

  (labels ((merge-operands (block)
             (with-struct-slots value-block- (operands statements protect-p)
                 block

               (foreach #'merge-operands operands)

               (when protect-p
                 (let ((protected (remove-if-not #'value-block-protect-p operands)))
                   (foreach #'inline-block protected)

                   (setf statements
                         (append
                          (map-extend #'operand-statements protected)
                          statements))

                   (setf operands
                         (remove-if #'value-block-protect-p operands))))))

           (operand-statements (operand)
             (with-struct-slots value-block- (variable statements)
                 operand

               (if variable
                   (list* (js-var variable) statements)
                   statements))))

    (merge-operands block)))
