;;;; expressions.lisp
;;;;
;;;; Tridash Programming Language.
;;;; Copyright (C) 2019-2020  Alexander Gutev
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

;;;; Functions for compiling internal Tridash function expressions to
;;;; WebAssembly

(in-package :tridash.backend.wasm)


;;; Type Constants

(defconstant +type-thunk+ 0)

(defconstant +type-i32+ 2)
(defconstant +type-f32+ 3)

(defconstant +type-string+ 4)

(defconstant +type-fail+ 5)

(defconstant +type-funcref+ 6)
(defconstant +type-funcref-args+ 7
  "Function reference with optional/outer node arguments.")

(defconstant +type-array+ 8)

(defconstant +type-symbol+ 9)
(defconstant +type-charecter+ 10)

;;;; Type Tags

(defconstant +tag-mask+ 3)
(defconstant +tag-int+ 1)
(defconstant +tag-funcref+ 2)
(defconstant +tag-fail+ 3)

(defconstant +max-immediate-int+ (1- (expt 2 29)))
(defconstant +min-immediate-int+ (- (expt 2 29)))

(defconstant +word-alignment+ 4
  "Byte boundary to which objects should be aligned.")


;;; Backend State

(defclass backend-state ()
  ((meta-node-functions
    :initform (make-hash-map)
    :accessor meta-node-functions
    :documentation
    "Map from meta-node objects to meta-node function identifiers.")

   (thunk-functions
    :initform (make-hash-map)
    :accessor thunk-functions
    :documentation
    "Map from thunk function indices to the instructions comprising
     the thunk functions.")

   (meta-node-ref-functions
    :initform (make-hash-map)
    :accessor meta-node-ref-functions
    :documentation
    "Map from meta-node's to a list containing the index and
     instructions of the corresponding meta-node reference
     function.")

   (string-constants
    :initform (make-hash-map)
    :accessor string-constants
    :documentation
    "Map from string constants to their corresponding locations in
     memory.")

   (constant-offset
    :initform 0
    :accessor constant-offset
    :documentation
    "Offset, within linear memory, to the next location where a
     constant value may be stored.")

   (data-section
    :initform (make-array 0 :element-type '(unsigned-byte 8) :fill-pointer t :adjustable t)
    :accessor data-section
    :documentation
    "Contents of the data section as an array of bytes.")

   (symbol-table
    :initform (make-hash-map)
    :accessor symbol-table
    :documentation
    "Map mapping symbols/strings to their offsets within the data
     section.")

   (object-descriptors
    :initform nil
    :accessor object-descriptors
    :documentation
    "List of the object descriptors. Each element is a CONS of the
     form (OFFSET . FIELDS) where OFFSET is the offset of the
     descriptor within the data section and FIELDS is the list of the
     offsets, within the data section, of the symbols naming the
     fields.")))

(defvar *backend-state*)

(defun meta-node-id (meta-node)
  "Returns the label for the function implementing META-NODE."

  (etypecase meta-node
    (external-meta-node
     (list
      "import"
      (or (attribute :wasm-name meta-node)
          (error 'undefined-external-meta-node-error
                 :backend "Wasm32"
                 :meta-node meta-node))))

    (meta-node
     (with-slots (meta-node-functions) *backend-state*
       (ensure-get meta-node meta-node-functions
         (symb '$m (length meta-node-functions)))))))

(defun add-thunk-function (function &optional (state *backend-state*))
  "Adds a thunk function to the global backend state's list of thunk
   functions. Returns the function label."

  (with-slots (thunk-functions) state
    (let ((index (length thunk-functions)))
      (setf (get index thunk-functions)
            function)

      (list 'thunk index))))


;;; Compiling Functions

(defstruct value-block
  "Represents a block of instructions for computing the value of an
   expression.

   LABEL is the local variable label in which the result is stored.

   OPERANDS is the list of `VALUE-BLOCK's of which the values are
   referenced in in INSTRUCTIONS.

   INSTRUCTIONS is the list of instructions.

   STRICT-P is a flag for whether the value of the expression is
   strictly computed.

   IMMEDIATE-P is a flag for whether the value of the expression is
   available as an immediate unboxed value, that is it is computed
   locally by INSTRUCTIONS prior to being boxed.

   COUNT is the total number of references to the block's value. NIL
   if the block is only referenced in one place.

   COMMON-P is a flag indicating whether the value computed by this
   block is referenced by more than one block, i.e. is a compiled
   `expression-block'."

  label
  operands
  instructions

  strict-p
  immediate-p

  count
  common-p)

(defclass function-block-state ()
  ((argument-locals
    :initform (make-hash-map)
    :accessor argument-locals
    :documentation
    "Local variables in which the values of the function arguments are
     stored.")

   (local-counter
    :initform 0
    :accessor local-counter
    :documentation
    "Local variable counter for generating local variable
     identifiers.")

   (expression-blocks
    :initform (make-hash-map)
    :accessor expression-blocks
    :documentation
    "Map from `expression-block' objects to the `value-block' objects
     containing the instructions for computing their values.")

   (strict-blocks
    :accessor strict-blocks
    :documentation
    "Strictness expression of the `EXPRESSION-BLOCK' objects in the
     function's expression."))

  (:documentation
   "Stores the compilation state for a function."))

(defvar *function-block-state* nil
  "The compilation state for the function currently being compiled.")

(defun compile-function (expression operands)
  "Compiles a function comprising the expression EXPRESSION with
   operand nodes OPERANDS."

  (let ((*function-block-state* (make-instance 'function-block-state)))
    (foreach #'add-operand operands)

    (let ((tridash.frontend.strictness:*analyze-nodes* nil)
          (tridash.frontend.strictness:*return-blocks* t))
      (setf (strict-blocks *function-block-state*)
            (analyze-expression expression)))

    (let* ((block (compile-expression expression))
           (blocks (flatten-block block))
           (locals (locals-map blocks)))

      `(func ,@(make-list (length operands) :initial-element '(param i32))
             ,@(-> (map-extend #'value-block-instructions blocks)
                   (append `((local.get (ref ,(value-block-label block)))))
                   remove-redundant-boxes

                   (make-function-body
                    (make-operand-map (length operands))
                    locals))))))

(defun add-operand (node)
  "Adds a local variable, to *FUNCTION-BLOCK-STATE*, and a
   `VALUE-BLOCK', for the operand node NODE."

  (with-slots (argument-locals) *function-block-state*
    (setf (get node argument-locals) (make-value-block :label (next-local)))))

(defun next-local (&optional (state *function-block-state*))
  "Returns a unique identifier for a new local variable in the
   function with compilation state STATE."

  (with-slots (local-counter) state
    (prog1 local-counter
      (incf local-counter))))

(defun flatten-block (block)
  "Returns a list of all the blocks which are required for computing
   the value of BLOCK. The last element of the list is BLOCK, itself.

   Compiled `expression-block's of which the count does not equal the
   number of references within BLOCK, are not included in the
   list. The second return value contains a map of all such
   `VALUE-BLOCK's and the number of times they are referenced within
   BLOCK."

  (let ((flat-block nil)
        (block-map (make-hash-map)))

    (labels ((block-operands (block)
               "Returns the operands of BLOCK which can be inserted
                into the flat list."

               (with-struct-slots value-block- (operands) block
                 (remove-if-not #'count= operands)))

             (count= (block)
               "Returns true if the number of references to BLOCK
                within the function is equal to its total number of
                references."

               (or (not (value-block-common-p block))
                   (= (value-block-count block)
                      (get block block-map))))

             (add-to-block-map (operands)
               "Increments the reference count, within BLOCK-MAP, for
                each `VALUE-BLOCK' in OPERANDS."

               (foreach
                (lambda (block)
                  (when (value-block-common-p block)
                    (incf (get block block-map 0))))
                operands)))

      (nlet-tail add-blocks ((blocks (list block)))
        (setf flat-block (append blocks flat-block))

        (let ((next-blocks (map-extend #'block-operands blocks)))
          (foreach (compose #'add-to-block-map #'value-block-operands) blocks)

          (doseq ((block . count) block-map)
            (when (= count (value-block-count block))
              (push block next-blocks)
              (erase block-map block)))

          (when next-blocks
            (add-blocks next-blocks)))))

    (values flat-block block-map)))


(defun make-operand-map (num-operands)
  "Creates a map mapping the labels [0, num-operands] to themselves."

  (map-to 'hash-map #'cons (range 0 num-operands) (range 0 num-operands)))

(defun locals-map (blocks)
  "Returns the map mapping local variable labels to the corresponding
   `value-block' objects."

  (map-to
   'hash-map

   (lambda (block)
     (cons (value-block-label block) block))

   blocks))


(defun make-function-body (instructions operands locals)
  "Generates the full series of instructions for a function.

   Performs the following:

   1. Expands macro instructions into their wasm equivalents
   2. Inserts the necessary stack load and stores for references
   3. Maps local variable labels to indices
   4. Generates (local ...) declarations
   5. Optimizations

   INSTRUCTIONS are the instructions for computing the result of then
   function.

   OPERANDS is the map, mapping parameter local variable labels to
   indices.

   LOCALS is the map mapping local variable labels to the
   corresponding VALUE-BLOCK objects."

  (-> instructions
      remove-redundant-boxes
      (expand-wasm-macros locals)

      ;; Making Reference Load and Stores
      optimize-reference-loads
      make-load-references
      patch-stack-offsets

      ;; Renumber locals and generate locals declarations
      (make-locals operands)))

(defun make-locals (instructions &optional (local-map (make-hash-map)))
  "Adds local variable declarations, for all local variables used in
   INSTRUCTIONS. Returns INSTRUCTIONS with all local variables labels
   converted to indices and prepended with the local variable
   declarations.

   LOCAL-MAP is the initial map from labels to indices to use. New
   labels are added to the map with the index being the size of the
   map."

  (let ((decls (make-collector nil)))
    (labels ((map-instruction (instruction)
               (list
                (match instruction
                  ((list (and (or 'local.get 'local.set 'local.tee) op) local)
                   `(,op ,(map-local local)))

                  (_ instruction))))

             (map-local (local)
               (ensure-get local local-map
                 (prog1 (length local-map)
                   (accumulate decls '(local i32))))))

      (let ((body (map-wasm #'map-instruction instructions)))
        (append (collector-sequence decls) body)))))


(defgeneric compile-expression (expression &key &allow-other-keys)
  (:documentation
   "Compiles a single intermediate expression.

    Returns a `VALUE-BLOCK' structure containing the instructions for
    computing the expression's value."))


;;;; Thunks

(defmethod compile-expression :around (expression &key (thunk nil))
  "If :THUNK is true, compiles EXPRESSION to a thunk function,
   which is added to the THUNK-FUNCTIONS list of *BACKEND-STATE*, and
   returns a `value-block' which creates the thunk object."

  (let ((block (call-next-method)))
    (with-struct-slots value-block- (instructions common-p)
        block

      (if (and thunk instructions (not common-p))
          (multiple-value-bind (fn closure)
              (make-thunk-function block)

            (make-thunk (add-thunk-function fn) closure))

          block))))

(defun make-thunk-function (block)
  "Creates a thunk function which computes the value of the
   `value-block' BLOCK.

   Returns two values: the thunk function and the list of locals
   comprising the thunk's closure."

  (flet ((load-local (local index)
           `((local.get (ref $c))
             (i32.load offset ,(* 4 index))
             (local.set (ref ,(value-block-label local))))))

    (with-struct-slots value-block- (label instructions locals) block
      (multiple-value-bind (blocks closure)
          (flatten-block block)

        (decrement-block-count closure)

        (let ((closure-locals (sort (map-keys closure) :key #'value-block-label)))
          (values
           `(func (param i32) (result i32)
                  ,@(->
                     (append
                      (map-extend #'load-local closure-locals (range 1))
                      (map-extend #'value-block-instructions blocks)

                      `((local.get (ref ,label))))

                     (make-function-body
                      (make-locals (alist-hash-map '(($c . 0))))
                      (locals-map blocks))))
           closure-locals))))))

(defun decrement-block-count (block-map)
  "Decrement block count of each `VALUE-BLOCK' in BLOCK-MAP by the
   number of references to the block."

  (doseq ((block . count) block-map)
    (decf (value-block-count block)
          (1- count))))

(defun make-thunk (funcref closure)
  "Returns a `value-block' which creates a thunk object that invokes
   the thunk function with index FUNCREF with the locals in CLOSURE in
   the thunk's closure."

  (let* ((result (next-local))
         (num-locals (length closure))
         (size (* 4 (+ 3 num-locals))))

    (flet ((store-local (local index)
             `((local.get (ref ,result))
               (local.get (ref ,(value-block-label local)))
               (i32.store offset ,(+ 12 (* 4 index))))))

      (make-value-block
       :label result
       :operands closure

       :instructions
       `((i32.const ,size)
         (call (import "alloc"))
         (local.tee (ref ,result))

         (i32.const ,+type-thunk+)
         i32.store

         (local.get (ref ,result))
         (i32.const ,funcref)
         (i32.store offset 4)

         (local.get (ref ,result))
         (i32.const ,num-locals)
         (i32.store offset 8)

         ,@(map-extend #'store-local closure (range 0)))))))


;;; Expression Blocks

(defmethod compile-expression ((block expression-block) &rest args &key)
  (with-struct-slots expression-block- (count expression) block
    (if (> count 1)
        (compile-expression-block block)
        (apply #'compile-expression expression args))))

(defun compile-expression-block (block)
  (with-slots (expression-blocks strict-blocks) *function-block-state*
    (with-struct-slots expression-block- (count expression) block
      (or (get block expression-blocks)

          (let* ((result (next-local))
                 (vblock
                  (make-value-block
                   :label result
                   :count count
                   :common-p t)))

            ;; Add to blocks in case it contains a cyclic reference to
            ;; itself
            (setf (get block expression-blocks) vblock)

            (let ((value
                   (compile-expression
                    expression
                    :thunk (not (strict? strict-blocks block)))))

              (setf (value-block-operands vblock)
                    (list value))

              (setf (value-block-instructions vblock)
                    `((local.get (ref ,(value-block-label value)))
                      (local.set (ref ,result)))))

            vblock)))))

(defmethod compile-expression ((cycle cyclic-reference) &key)
  "Handles cyclic references. Returns the `value-block' of the
   referenced expression."

  (with-struct-slots cyclic-reference- (expression) cycle
    (check-type expression expression-block)

    (let ((block (get expression (expression-blocks *function-block-state*))))
      (assert block)

      ;; Decrement reference count by 1 and return raw reference to
      ;; the `VALUE-BLOCK' object.
      ;;
      ;; NOTE: The `VALUE-BLOCK' object is not added to the operands
      ;; of the return block to prevent an infinite loop.

      (decf (value-block-count block))

      (let ((result (next-local)))
        (make-value-block
         :label result
         :instructions
         `((local.get (ref ,(value-block-label block)))
           (local.set (ref ,result))))))))


;;; Functor Expressions

(defconstant +arithmetic-operators+
  (alist-hash-map
   `(("+" . add)
     ("-" . sub)
     ("*" . mul)
     ("/" . div_s)
     ("%" . rem_s))))

(defmethod compile-expression ((expression functor-expression) &key)
  (with-struct-slots functor-expression- (meta-node arguments outer-nodes)
      expression

    (compile-functor-expression meta-node arguments outer-nodes)))

(defgeneric compile-functor-expression (operator arguments outer-nodes)
  (:documentation
   "Compiles a functor expression consisting of OPERATOR applied to
    ARGUMENTS. OUTER-NODES is a map mapping the outer nodes to the
    corresponding Tridash expressions which compute their values."))

(defmethod compile-functor-expression ((meta-node meta-node) arguments outer-nodes)
  "Compiles a functor expression in which the operator is a
   `meta-node'."

  (let* ((result (next-local))
         (strict-outer-nodes (strict-outer-operands meta-node))
         (operands
          (compile-operands
           (append arguments (outer-node-operands meta-node outer-nodes))

           (append
            (strict-arguments meta-node)
            (map (rcurry #'get strict-outer-nodes)
                 (outer-node-references meta-node))))))

    (make-value-block
     :label result
     :operands operands

     :instructions
     `(,@(map (curry #'list 'local.get)
              (map (compose (curry #'list 'ref) #'value-block-label) operands))
         (call ,(meta-node-id meta-node))
         (local.set (ref ,result))))))

(defun outer-node-operands (meta-node outer-nodes)
  "Returns the list of intermediate expressions for computing the
   outer-nodes of META-NODE, in the correct order."

  (map (rcurry #'get outer-nodes) (outer-node-references meta-node)))


(defmethod compile-functor-expression ((meta-node external-meta-node) arguments outer-nodes)
  "Compiles a functor expression in which the operator is an
   `external-meta-node'."

  (declare (ignore outer-nodes))

  (let* ((name (attribute :wasm-name meta-node)))

    (acond
      ((get name +arithmetic-operators+)
       (let* ((operands (compile-operands (remove-nil-arguments arguments) (strict-arguments meta-node)))
              (operand-labels (map #'value-block-label operands))
              (result (next-local)))

         (->> (compile-arithmetic-expression it operand-labels result)
              (make-value-block :label result
                                :operands operands
                                :strict-p t
                                :immediate-p t
                                :instructions))))

      ((= name "if")
       (compile-if-expression arguments))

      (t
       (call-next-method meta-node (remove-nil-arguments arguments))))))

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


(defun compile-arithmetic-expression (instruction operands result)
  (destructuring-bind (l r) operands
    `((block (result) (for ,result)
        (block (result)
          (resolve ,l)
          (unbox ,l)

          (block (result)
            (resolve ,r)
            (unbox ,r)

            (block (result)
              (block (result)
                (local.get (type ,l))
                (local.get (type ,r))
                i32.or

                (i32.const 2)
                i32.sub

                ;;TODO: Branch to block which returns failure if types
                ;;are neither float nor integer
                (br_table 0 1))

              ;; Integer Arithmetic

              (local.get (value ,l))
              (local.get (value ,r))
              ,(symb 'i32. instruction)

              (box ,result (type i32))
              (br 3))

            ;; Floating-point arithmetic
            (local.get (type ,l))
            (i32.const ,+type-i32+)
            i32.eq
            (if (result f32)
                (then (local.get (value ,l))
                      f32.convert_i32)
                (else (local.get (value ,l))
                      f32.reinterpret_i32))

            (local.get (type ,r))
            (i32.const ,+type-i32+)
            i32.eq
            (if (result f32)
                (then (local.get (value ,r))
                      f32.convert_i32)
                (else (local.get (value ,l))
                      f32.reinterpret_i32))

            ,(symb 'f32. instruction)
            (box ,result (type f32))
            (br 2))

          ;; Handle failure in $b
          (get-fail ,r)
          (box ,result (type fail))
          (br 1))

        ;; Handle failure in $a
        (get-fail ,l)
        (box ,result (type fail))))))

(defun compile-if-expression (operands)
  "Returns a block which computes the result of an if expression with
   arguments OPERANDS."

  (let* ((operands (compile-operands operands (list t nil nil)))
         (operand-labels (map #'value-block-label operands))
         (result (next-local)))

    (make-value-block
     :label result
     :operands operands

     :instructions
     (destructuring-bind (test then else) operand-labels
       `((local.get ,then)
         (local.get ,else)
         (local.get ,test)
         select
         (local.set ,result))))))

(defmethod compile-functor-expression (node arguments outer-nodes)
  "Compiles a functor expression in which the operator is a `node'
   object of which the value is interpreted as a function."

  (declare (ignore outer-nodes))

  (let ((operands
         (compile-operands
          (cons node arguments)
          (cons t (make-list (length arguments) :initial-element nil)))))

    (destructuring-bind (operator-block . operand-blocks) operands
      (let ((result (next-local)))
        (make-value-block
         :label result
         :operands operands

         :instructions

         (let ((operator (value-block-label operator-block))
               (operands (map #'value-block-label operand-blocks))
               (arg-list (next-local))
               (num-args (length arguments)))

           (flet ((push-operand (operand offset)
                    `((local.get (ref ,arg-list))
                      (local.get (ref ,operand))
                      (i32.store offset ,offset))))

             `((block
                   (block
                       (resolve ,operator)
                     (unbox ,operator (type funcref))

                     ;; Allocate storage for arguments list
                     (i32.const ,(+ 4 (* 4 num-args)))
                     (call (import "alloc"))
                     (local.tee (ref ,arg-list))

                     ;; Store number of arguments
                     (i32.const ,num-args)
                     i32.store

                     ;; Store arguments
                     ,@(map-extend-to 'list #'push-operand operands (range 4 (+ 4 (* 4 num-args)) 4))

                     (local.get (type ,operator))
                     (i32.const ,+type-funcref+)
                     i32.eq

                     (if (result i32)
                         (then
                          (local.get (ref ,arg-list))
                          (local.get (value ,operator))
                          (call_indirect (type (func (param i32) (result i32)))))

                         (else
                          (local.get (ref ,arg-list))
                          (local.get (ref ,operator))
                          (i32.load offset 12)
                          (local.get (ref ,operator))
                          i32.load
                          (call_indirect (type (func (param i32) (param i32) (result i32))))))

                     (local.set (ref ,result))
                     (br 1))

                 ;; Handle failure in operator operand
                 (get-fail ,operator)
                 (box ,result (type fail)))))))))))


;;; Operands

(defun compile-operands (operands strict-operands)
  "Compiles the operands of an expression. Returns the list of
   `VALUE-BLOCK' objects for computing the values of the operands."

  (labels ((compile-operand (operand strict?)
             (compile-expression operand :thunk (null strict?))))

    (map #'compile-operand operands strict-operands)))

(defmethod compile-expression ((link node-link) &key)
  (get (node-link-node link)
       (argument-locals *function-block-state*)))

(defmethod compile-expression ((list argument-list) &key)
  "Compile rest argument lists to an expression which creates an array
   object containing the arguments."

  (with-struct-slots argument-list- (arguments) list
    (let* ((result (next-local))
           (num-args (length arguments))
           (operands (compile-operands arguments (repeat nil))))

      (flet ((store-arg (arg index)
               `((local.get (ref ,result))
                 (local.get (ref ,(value-block-label arg)))
                 (i32.store (offset ,(+ 8 (* 4 index)))))))

        (make-value-block
         :label result
         :operands operands

         :instructions

         (if (emptyp arguments)
             `((call (import "empty_list"))
               (local.set (ref ,result)))

             `((i32.const ,(+ 8 (* num-args 4)))
               (call (import "alloc"))
               (local.tee (ref ,result))

               ;; Store object type
               (i32.const ,+type-array+)
               i32.store

               ;; Store list size
               (local.get (ref ,result))
               (i32.const ,num-args)
               (i32.store (offset 4))

               ;; Store Arguments
               ,@(map-extend #'store-arg operands (range 0)))))))))


;;; Meta-Node References

(defmethod compile-expression ((ref meta-node-ref) &key)
  "Creates a block which creates a function reference object to the
   meta-node's function. Generates the meta-node reference function
   and adds it to the map META-NODE-REF-FUNCTIONS (of
   *BACKEND-STATE*) if it has not been added already."

  (with-slots (meta-node-ref-functions) *backend-state*
    (with-struct-slots meta-node-ref- (node optional outer-nodes) ref
      (let* ((result (next-local))

             (arguments
              (append optional (outer-node-operands node outer-nodes)))

             (operands
              (compile-operands arguments (make-list (length arguments) :initial-element nil)))

             (func
              (first
               (ensure-get node meta-node-ref-functions
                 (list (length meta-node-ref-functions)
                       (make-meta-node-ref-function node))))))

        (flet ((store-arg (operand index)
                 "Generate instructions which stores the value of
                  OPERAND at INDEX within the defaults array of the
                  function reference."

                 `((local.get (ref ,result))
                   (local.get (ref ,(value-block-label operand)))
                   (i32.store offset ,(+ 12 (* 4 index))))))

          (make-value-block
           :label result
           :operands operands

           :instructions
           (if (emptyp operands)

               `((i32.const (meta-node-ref ,func))
                 (box ,result (type funcref)))

               `((i32.const ,(+ 12 (* 4 (length operands))))
                 (call (import "alloc"))
                 (local.tee (ref ,result))

                 (i32.const ,+type-funcref-args+)
                 i32.store

                 (local.get (ref ,result))
                 (i32.const (meta-node-ref ,func))
                 (i32.store offset 4)

                 ;; Store number of default argument values
                 (local.get (ref ,result))
                 (i32.const ,(length operands))
                 (i32.store offset 8)

                 ;; Store optional and outer node argument values
                 ,@(map-extend #'store-arg operands (range 0))))))))))

(defun make-meta-node-ref-function (meta-node)
  "Create a meta-node reference function for META-NODE."

  (with-slots (operands) meta-node
    (flet ((make-operand (index)
             (make-value-block :label index)))

      (let* ((labels (coerce (range 0 (length operands)) 'list))
             (operands (map #'make-operand labels))

             (outer-nodes
              (map-to 'hash-map #'cons
                      (outer-node-references meta-node)
                      (map #'make-operand (range (length operands)))))

             (block (compile-functor-expression meta-node operands outer-nodes)))

        (make-ref-function-declaration
         meta-node
         (value-block-label block)
         (flatten-block block))))))

(defun make-ref-function-declaration (meta-node result blocks)
  (with-slots (operands) meta-node
    (labels ((load-operand (index)
               "Generate code which loads an operand from the
                arguments array parameter."

               `((local.get (ref $args))
                 (i32.load offset ,(+ 4 (* index 4)))
                 (local.set (ref ,index))))

             (load-optional (start count)
               "Generate code which loads the values of the optional
                arguments, either from the arguments array or the
                default values array."

               (reduce
                #'make-block
                (map (rcurry #'make-load-optional start count) (range 0 (1+ count)))

                :from-end t
                :key (rcurry #'coerce 'list)
                :initial-value

                `(block
                     (local.get $count)
                   (i32.const ,start)
                   i32.sub
                   (br_table ,@(reverse (range 0 (1+ count)))))))

             (make-load-optional (num-provided start count)
               "Generate code which loads NUM-PROVIDED optional
                arguments from the arguments array, and the rest from
                the default values array."

               (concatenate
                (map-extend
                 (lambda (index)
                   (if (< index num-provided)
                       (load-operand (+ start index))
                       (load-default start index)))
                 (range 0 count))
                `((br ,num-provided))))

             (load-default (start index)
               "Generate code which loads an optional argument from
                the defaults array."

               (load-default-value (+ start index) (* 4 index)))

             (load-default-value (local offset)
               `((local.get (ref $defaults))
                 (i32.load offset ,offset)
                 (local.set (ref ,local))))

             (make-block (loads block)
               "Wrap BLOCK and LOADS into a block."

               `(block
                    ,block
                  ,@loads))

             (num-optional (arity)
               "Determine the number of optional arguments."

               (destructuring-bind (min . max) arity
                 (- (or max (1- (length operands)))
                    min)))

             (make-rest-array (start)
               "Generate code which creates an array containing the
                rest arguments."

               `((block
                     (block
                         ;; Check whether has rest arguments
                         (local.get $count)
                       (i32.const ,start)
                       i32.lt_u
                       (br_if 0)

                       ;; Determine number of rest arguments
                       (local.get $count)
                       (i32.const ,start)
                       i32.sub

                       ;; Determine size of array
                       (i32.const 4)
                       i32.mul
                       (local.tee $rest-size)

                       ;; Allocate memory for array
                       (i32.const 8)
                       i32.add
                       (call (import "alloc"))
                       (local.tee (ref ,start))

                       ;; Store object type
                       (i32.const ,+type-array+)
                       i32.store

                       ;; Store number of arguments
                       (local.get (ref ,start))
                       (local.get $count)
                       (i32.const ,start)
                       i32.sub
                       (i32.store offset 4)

                       ;; Copy arguments using memcopy
                       (local.get (ref ,start))
                       (i32.const 8)
                       i32.add

                       (local.get (ref $args))
                       (i32.const ,(+ 4 (* 4 start)))
                       i32.add

                       (local.get $rest-size)
                       (call (import "memcopy"))
                       (br 1))

                   ;; Set rest argument array to empty
                   (call (import "Empty"))
                   (local.set (ref ,start))))))

      (let* ((num-outer-nodes (length (outer-node-references meta-node)))
             (num-operands (length operands))
             (arity (meta-node-arity meta-node))
             (num-optional (num-optional arity)))

        `(func
          (param i32)
          ,@(when (plusp num-optional)
              '((param i32)))
          (result i32)

          ,@(->
             `((local.get (ref $args))
               i32.load
               (local.set $count)

               ;; Check Arity
               ,(make-arity-check arity)

               ;; Load required arguments
               ,@(map-extend-to 'list #'load-operand (range 0 (car arity)))

               ;; Load optional arguments if any
               ,@(when (plusp num-optional)
                   (list (load-optional (car arity) num-optional)))

               ;; Construct rest argument list
               ,@(unless (cdr arity)
                   (make-rest-array (1- (length operands))))

               ;; Load outer node values
               ,@(when (plusp num-outer-nodes)
                   (map-extend-to 'list #'load-default-value
                                  (range num-operands (+ num-operands num-outer-nodes))
                                  (map (curry #'* 4) (range num-optional))))

               ;; Body Instructions
               ,@(map-extend #'value-block-instructions blocks)
               (local.get (ref ,result)))

             (make-function-body
              (alist-hash-map
               (if (plusp num-optional)
                   '(($args . 0)
                     ($defaults . 1))
                   '(($args . 0))))
              (locals-map blocks))))))))

(defun make-arity-check (arity)
  "Generate code which checks that the number of arguments (stored in
   a $count local variable) is within the ARITY."

  (destructuring-bind (min . max) arity
    `(block
         ,@(cond
            ((= min max)
             `((local.get $count)
               (i32.const ,min)
               i32.eq))

            ((null max)
             `((local.get $count)
               (i32.const ,min)
               i32.ge_u))

            (t
             `((local.get $count)
               (i32.const ,min)
               i32.ge_u

               (local.get $count)
               (i32.const ,max)
               i32.le_u

               i32.and)))

       (br_if 0)

       (call (import "make_arity_error"))
       return)))


(defmethod compile-expression ((block value-block) &key)
  "Method for expressions which have already been compiled. Simply
   returns the block."

  block)


;;; Object Expressions

(defmethod compile-expression ((object object-expression) &key)
  "Generates an expression which returns an object containing each
   field-value pair in OPERANDS."

  (with-accessors ((entries object-expression-entries)) object
    (let ((keys (map #'first entries))
          (values (map #'second entries)))

      (let ((result (next-local))
            (values (compile-operands values (repeat nil)))
            (descriptor (make-object-descriptor keys)))

        (flet ((store-field (value index)
                 `((local.get (ref ,result))
                   (local.get (ref ,(value-block-label value)))
                   (i32.store offset ,(+ 4 (* 4 index))))))

          (make-value-block
           :label result
           :operands values

           :instructions
           `((i32.const ,(+ 4 (* 4 (length values))))
             (call (import "alloc"))
             (local.tee (ref ,result))

             ;; Store object descriptor
             (i32.const (data ,descriptor))
             i32.store

             ;; Store each value
             ,@(map-extend #'store-field values (range 0)))))))))

(defun make-object-descriptor (fields)
  "Reserve space for a new object descriptor with subnode identifiers
   FIELDS. Does not actually create the descriptor, but simply fills
   the reserved space with zeros.

   Returns the offset of the object descriptor, within the data
   section."

  (with-slots (data-section constant-offset object-descriptors)
      *backend-state*

    (let ((table (reserve-hash-table (length fields)))
          (fields
           (-> (compose (curry #'+ 4)
                        (rcurry #'add-string-constant +type-symbol+)
                        #'symbol-name)
               (map fields))))

      ;; Reserve space for table in data section
      (setf data-section

            (concatenate
             data-section

             (byte-encode (length fields))
             table))

      ;; Add object descriptor
      (push (cons constant-offset fields)
            object-descriptors)

      (prog1 constant-offset
        (incf constant-offset (+ 4 (length table)))))))

(defun reserve-hash-table (num-fields &key (load-factor 0.7))
  "Returns an array of bytes, the size of which corresponds to a hash
   table containing NUM-FIELDS fields, with a load factor of
   LOAD-FACTOR.

   The size field of the hash-table is initialized to the correct
   number of buckets however the remaining fields are initialized to
   zero."

  (let ((num-buckets (ceiling num-fields load-factor)))
    (concatenate
     (byte-encode num-buckets)
     (repeat 0 (* 8 num-buckets)))))


;;; Literals

;;;; Numbers

(defmethod compile-expression ((value integer) &key)
  (let ((result (next-local)))
    (make-value-block
     :label result
     :instructions (number-literal value 'i32 result)

     :immediate-p t
     :strict-p t)))

(defmethod compile-expression ((value float) &key)
  (let ((result (next-local)))
    (make-value-block
     :label result
     :instructions (number-literal value 'f32 result)

     :immediate-p t
     :strict-p t)))

(defun number-literal (value type result)
  `((,(symb type '.const) ,value)
    (box ,result (type ,type))))


;;;; Strings

(defmethod compile-expression ((value string) &key)
  "Adds the string constant to the data section, increments the
   constant section offset and returns a `VALUE-BLOCK' which returns a
   reference to the string within the data section."

  (let ((result (next-local)))
    (make-value-block
     :label result
     :instructions
     `((i32.const (data ,(add-string-constant value +type-string+)))
       (local.set (ref ,result)))

     :strict-p t)))

(defun byte-encode (value)
  "Return the list of bytes comprising a 32-bit integer VALUE in
   little endian order."

  (check-type value integer)

  (list
   (ldb (byte 8 0) value)
   (ldb (byte 8 8) value)
   (ldb (byte 8 16) value)
   (ldb (byte 8 24) value)))

(defun add-string-constant (value type)
  "Add a string constant to the data section and increment the
   constant section offset. The constant object is of type TYPE. The
   offset to the object is returned."

  (with-slots (data-section constant-offset symbol-table) *backend-state*
    (ensure-get (cons value type) symbol-table
      (let* ((octet-string
              (string-to-octets
               value
               :encoding :utf-8
               :use-bom nil))

             (size (+ 8 (length octet-string)))
             (padding (mod (- size) +word-alignment+)))

        (setf data-section
              (concatenate
               data-section
               (byte-encode type)
               (byte-encode (length octet-string))
               octet-string
               (make-list padding :initial-element 0)))

        (prog1 constant-offset
          (incf constant-offset (+ size padding)))))))


;;;; Characters

(defmethod compile-expression ((chr character) &key)
  (let ((result (next-local)))
    (make-value-block
     :label result
     :instructions
     `((i32.const ,(char-code chr))
       (box ,result (type character)))

     :immediate-p t
     :strict-p t)))


;;;; Symbols

(defmethod compile-expression ((sym symbol) &key)
  "Adds the symbol constant to the data section, increments the
   constant section offset and returns a `VALUE-BLOCK' which returns a
   reference to the symbol within the data section."

  (let ((result (next-local)))
    (make-value-block
     :label result
     :instructions
     `((i32.const (data ,(add-string-constant (symbol-name sym) +type-symbol+)))
       (local.set (ref ,result)))

     :strict-p t)))


;;;; No Value

(defmethod compile-expression ((null null) &key)
  (let ((result (next-local)))
    (make-value-block
     :label result
     :instructions
     `((call (import "fail_no_value"))
       (local.set (ref ,result))))))


;;; Managing Pointers

(defun make-load-references (instructions)
  "Replaces each LOCAL.GET/SET/TEE instruction, for a REF local, with
   a sequence of instructions that load/store the reference from/onto
   the stack."

  (let ((store-operands (make-hash-map)))
    (labels ((expand (instruction)
               "Expand LOCAL.GET/SET/TEE instructions for REF locals
                to instructions which load/store the object reference
                on the stack."

               (match instruction
                 ((list 'local.get (list 'ref label))
                  (ensure-get label store-operands t)

                  ;; Load label offset from stack
                  `((local.get $stack-base)
                    (i32.load (offset (local-offset ,label)))
                    (local.tee ,label)))

                 ((list 'local.set (list 'ref label))
                  (ensure-get label store-operands nil)

                  `((local.set ,label)

                    ;; Store in stack
                    (local.get $stack-base)
                    (local.get ,label)
                    (i32.store (offset (local-offset ,label)))))

                 ((list 'local.tee (list 'ref label))
                  (concatenate
                   (expand `(local.set (ref ,label)))
                   `((local.get ,label))))

                 (_ (list instruction))))

             (add-store-operands (instructions)
               "Add instructions which store the references to the
                function operands on the stack."

               (concatenate
                (make-store-operands)
                instructions))

             (make-store-operands ()
               "Create instructions which store the references to the
                function operands on the stack."

               (let ((locals (remove-if-not #'cdr store-operands)))
                 (concatenate
                  (->> locals
                       map-keys
                       (map-extend #'store-operand)))))

             (store-operand (label)
               "Create instructions which store operand LABEL on the
                stack."

               `((local.get $stack-base)
                 (local.get ,label)
                 (i32.store (offset (local-offset ,label))))))

      (add-store-operands (map-wasm #'expand instructions)))))

(defun optimize-reference-loads (instructions)
  "Optimize the loading/storing of reference from/onto the stack.

   LOCAL.GET/SET/TEE instructions for REF locals, are replaced with
   instructions for regular locals if it can be determined that there
   is no need to refresh or save the reference at that point."

  (let ((live (make-hash-map))
        (at-start t)
        (store-indices (make-hash-map))
        (load-counts (make-hash-map)))

    (labels ((optimize-load (instruction)
               "Replace REF locals with regular locals in LOCAL.GET
                instructions."

               (match instruction
                 ((list 'local.get (list 'ref label))
                  (list
                   (if (or (get label live) at-start)
                       (list 'local.get label)
                       (prog1 (list 'local.get (list 'ref label))
                         (setf (get label live) t)))))

                 ((list (or 'local.set 'local.tee) (list 'ref label))
                  (setf (get label live) t)
                  (list instruction))

                 ((list 'call _)
                  (clear live)
                  (setf at-start nil)
                  (list instruction))

                 (_
                  (list instruction))))

             (match-load-stores (instruction)
               "If INSTRUCTION is a LOCAL.SET REF, assign it a unique
                index. If INSTRUCTION is a LOCAL.GET REF, increment
                the number of loads for the last store (for that
                label) in load-COUNTS."

               (match instruction
                 ((list (and (or 'local.set 'local.tee) instruction)
                        (list 'ref label))
                  (let ((index (length load-counts)))
                    (setf (get label store-indices) index)
                    (setf (get index load-counts) 0)

                    (list (list instruction (list 'ref label index)))))

                 ((list 'local.get (list 'ref label))
                  (awhen (get label store-indices)
                    (incf (get it load-counts)))

                  (list instruction))

                 (_ (list instruction))))

             (optimize-store (instruction)
               "Replace LOCAL.SET REF instructions, for which the load
                count (in STORE-COUNTS) is 0, with regular LOCAL.SET
                instructions."

               (match instruction
                 ((list (and (or 'local.set 'local.tee) instruction)
                        (list 'ref label index))

                  (list
                   (if (zerop (get index load-counts))
                       (list instruction label)
                       (list instruction (list 'ref label)))))

                 (_ (list instruction)))))

      ;; Optimizing Stores
      ;;
      ;; Stores (local.set (ref $x)) for which there is no following
      ;; (local.get (ref $x)) can be removed

      (->> (map-wasm #'optimize-load instructions)
           (map-wasm #'match-load-stores)
           (map-wasm #'optimize-store)))))

(defun patch-stack-offsets (instructions)
  "Replace symbolic references to the stack offsets at which local
   variables are stored with actual offsets, and add the stack
   initialization and cleanup instructions."

  (let ((offsets (make-hash-map))
        (size 0))

    (labels ((patch-offset (instruction)
               "Patch references to local variable stack offsets with
                the actual offsets."

               (match instruction
                 ((list (and (or 'i32.load 'i32.store) op)
                        (list 'offset
                              (list 'local-offset label)))

                  (unless (get label offsets)
                    (setf (get label offsets) size)
                    (incf size 4))

                  `((,op (offset ,(get label offsets)))))

                 (_ (list instruction))))

             (add-stack-instructions (instructions)
               "Add instructions for initializing the stack base
                pointer, reserving stack space, zeroing stack elements
                and restoring the stack pointer."

               `((i32.const $stack-ptr)
                 (i32.const $stack-ptr)
                 i32.load
                 (i32.const ,size)
                 i32.sub
                 (local.tee $stack-base)
                 i32.store

                 ,@(zero-stack)

                 ,@instructions
                 (i32.const $stack-ptr)
                 (local.get $stack-base)
                 (i32.const ,size)
                 i32.add
                 i32.store))

             (zero-stack ()
               "Create instructions which zero out the reserved stack
                space."

               (let* ((size (/ size 4)))
                 (multiple-value-bind (num rem)
                     (floor size 2)

                   (concatenate-to
                    'list

                    (map-extend (curry #'zero-offset 'i64)
                                (range 0 (* num 8) 8))

                    (map-extend (curry #'zero-offset 'i32)
                                (range (* num 8) (+ (* num 8) (* rem 4)) 4))))))

             (zero-offset (type offset)
               "Create instructions which zero out the stack location
                at OFFSET. Type is the element size, I32 or I64."
               `((local.get $stack-base)
                 (,(symb type '.const) 0)
                 (,(symb type '.store) (offset ,offset)))))

      (let ((patched (map-wasm #'patch-offset instructions)))
        (if (zerop size)
            patched
            (add-stack-instructions patched))))))


;;; Optimization

(defun remove-redundant-boxes (instructions)
  "Removes `BOX' instructions for locals which do not need to be
   boxed."

  (let ((used-boxed (hash-set)))

    (labels ((mark-used (instruction)
               (match instruction
                 ((list 'local.get (list 'ref local))
                  (nadjoin local used-boxed))))

             (replace-box (instruction)
               (match instruction
                 ((list 'box local (list 'type type))
                  (if (memberp local used-boxed)
                      (list instruction)
                      (set-value local type)))

                 (_ (list instruction))))

             (set-value (local type)
               (ecase type
                 (f32 (set-float-value local))
                 (i32 (set-int-value local))
                 (fail (set-fail-value local))))

             (set-float-value (local)
               `(i32.reinterpret_f32
                 (local.set (value ,local))

                 (i32.const ,+type-f32+)
                 (local.set (type ,local))))

             (set-int-value (local)
               `((local.set (value ,local))
                 (i32.const ,+type-i32+)
                 (local.set (type ,local))))

             (set-fail-value (local)
               `((local.set ,local))))

      (walk-wasm #'mark-used instructions)
      (map-wasm #'replace-box instructions))))

(defun expand-wasm-macros (instructions locals)
  (let ((resolved (make-hash-set))
        (unboxed (make-hash-set)))

    (labels ((expand-macro (instruction)
               (match instruction
                 ((list 'resolve local)
                  (unless (or (resolved? local)
                              (value-block-strict-p (get local locals)))

                    (nadjoin local resolved)

                    `((local.get (ref ,local))
                      (call $resolve)
                      (local.set (ref ,local)))))

                 ((list 'unbox local (list 'type type))
                  (cond
                    ((or (value-block-immediate-p (get local locals))
                         (unboxed? local))
                     `((local.get ,local)
                       (i32.const ,+tag-mask+)
                       i32.and
                       (i32.const ,+tag-fail+)
                       i32.eq
                       (br_if 0)))

                    (t
                     (nadjoin local unboxed)
                     (unbox-local local type))))

                 ((list 'box local (list 'type type))
                  (box-local local type))

                 ((list 'get-fail local)
                  `((local.get (ref ,local))))

                 (_ (list instruction))))

             (resolved? (local)
               (memberp local resolved))

             (unboxed? (local)
               (memberp local unboxed)))

      (map-wasm #'expand-macro instructions))))


;;;; Unboxing

(defun unbox-local (local type)
  "Generate code which unboxes the value stored in LOCAL and checks
   that it is of type TYPE."

  (ecase type
    (number (unbox-number local))
    (funcref (unbox-funcref local))))

(defun unbox-number (local)
  "Generate code which unboxes an immediate or boxed numeric value."

  `((block (result)       ;; Type Error
      (block (result)     ;; Unbox immediate integer
        (block (result)   ;; Unbox boxed integer
          (block (result) ;; Untag
            (local.get ,local)
            (i32.const ,+tag-mask+)
            i32.and

            (br_table 0 1 2 4))

          ;; Boxed Value
          (local.get (ref ,local))
          i32.load
          (local.tee (type ,local))

          ;; Check Type
          (i32.const ,+type-i32+)
          i32.sub
          (br_table 2 2 1)

          (local.get (ref ,local))
          (i32.load offset 4)
          (local.set (value ,local))
          (br 2))

        ;; Immediate Integer
        (i32.const ,+type-i32+)
        (local.set (type ,local))

        (local.get ,local)
        (i32.const 2)
        i32.shr_s
        (local.set (value ,local))
        (br 1))

      (call (import "type-error"))
      (local.set (ref ,local))
      (br 1))))

(defun unbox-funcref (local)
  "Generates code which unboxes an function reference."

  `((block (result)       ;; Type Error
      (block (result)     ;; Unbox immediate integer
        (block (result)   ;; Unbox boxed integer
          (block (result) ;; Untag
            (local.get ,local)
            (i32.const ,+tag-mask+)
            i32.and

            (br_table 0 1 2 4))

          ;; Boxed Reference
          (local.get (ref ,local))
          i32.load
          (local.tee (type ,local))

          ;; Check Type
          (i32.const ,+type-funcref-args+)
          i32.ne
          (br_if 1)
          (br 2))

        ;; Immediate Reference
        (i32.const ,+type-funcref+)
        (local.set (type ,local))

        (local.get (ref ,local))
        (i32.const 2)
        i32.shr_s
        (local.set (value ,local))
        (br 1))

      (call (import "type-error"))
      (local.set (ref ,local))
      (br 1))))


;;;; Boxing

(defun box-local (local type)
  (ecase type
    (f32 (box-float local))
    (i32 (box-integer local))
    (character (box-character local))
    (funcref (box-funcref local))
    (fail
     `((local.set (ref ,local))))))

(defun box-float (local)
  `(i32.reinterpret_f32
    (local.set (value ,local))

    (i32.const 8)
    (call (import "alloc"))
    (local.tee (ref ,local))

    (i32.const ,+type-f32+)
    (local.tee (type ,local))
    i32.store

    (local.get (ref ,local))
    (local.get (value ,local))
    (i32.store (offset 4))))

(defun box-integer (local)
  `((local.tee (value ,local))
    (i32.const ,+max-immediate-int+)
    i32.le

    (local.get (value ,local))
    (i32.const ,+min-immediate-int+)
    i32.ge

    i32.and
    (if (then
         (local.get (value ,local))
         (i32.const 2)
         i32.shl
         (i32.const ,+tag-int+)
         i32.or
         (local.set ,local))

        (else
         (i32.const 8)
         (call (import "alloc"))
         (local.tee (ref ,local))

         (i32.const ,+type-i32+)
         i32.store

         (local.get (ref ,local))
         (local.get (value ,local))
         (i32.store offset 4)))))

(defun box-character (local)
  "Generates code which creates a boxed character object, with the
   reference to it stored in LOCAL."

  `((local.set (value ,local))

    (i32.const 8)
    (call (import "alloc"))
    (local.tee (ref ,local))

    (i32.const ,+type-charecter+)
    (local.tee (type ,local))
    i32.store

    (local.get (ref ,local))
    (local.get (value ,local))
    (i32.store (offset 4))))

(defun box-funcref (local)
  "Generates code which boxes the function index, stored in LOCAL into
   an immediate value stored in a tagged pointer."

  `((local.tee (value local))
    (i32.const 2)
    i32.shl
    (i32.const ,+tag-funcref+)
    i32.or
    (local.set ,local)

    (i32.const ,+type-funcref+)
    (local.set (type local))))


;;; Utilities

(defun walk-wasm (fn instructions)
  (labels ((walk (instruction)
             (unless (funcall fn instruction)
               (match instruction
                 ((list* (or 'block 'if 'then 'else 'loop)
                         instructions)
                  (map #'walk instructions))))))
    (map #'walk instructions)))

(defun map-wasm (fn instructions)
  (labels ((map-instruction (instruction)
             (match instruction
               ((list* (and (or 'block 'if 'then 'else 'loop) block)
                       instructions)

                (list (list* block (map-extend #'map-instruction instructions))))

               ((list* 'result _)
                (list instruction))

               (_ (funcall fn instruction)))))

    (map-extend #'map-instruction instructions)))
