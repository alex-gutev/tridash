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
(defconstant +type-resolved-thunk+ 1)
(defconstant +type-catch-thunk+ 15)

(defconstant +type-i32+ 2)
(defconstant +type-f32+ 3)

(defconstant +type-string+ 4)

(defconstant +type-fail+ 5)

(defconstant +type-funcref+ 6)
(defconstant +type-funcref-args+ 7
  "Function reference with optional/outer node arguments.")

(defconstant +type-array+ 8)
(defconstant +type-int-array+ 12)

(defconstant +type-symbol+ 9)
(defconstant +type-charecter+ 10)

(defconstant +type-list-node+ 13)

(defconstant +type-node+ 14)


;;;; Type Tags

(defconstant +tag-mask+ 3)
(defconstant +tag-int+ 1)
(defconstant +tag-funcref+ 2)
(defconstant +tag-fail+ 3)

(defconstant +max-immediate-int+ (1- (expt 2 29)))
(defconstant +min-immediate-int+ (- (expt 2 29)))

(defconstant +word-alignment+ 4
  "Byte boundary to which objects should be aligned.")


;;; Sizes

(defconstant +word-size+ 32)
(defconstant +word-byte-size+ (/ +word-size+ 8))


;;; Backend State

(defclass backend-state ()
  ((thunk-functions
    :initform (make-hash-map)
    :accessor thunk-functions
    :documentation
    "Map from thunk function indices to the corresponding
     WASM-FUNCTION-SPEC objects")

   (meta-node-ref-functions
    :initform (make-hash-map)
    :accessor meta-node-ref-functions
    :documentation
    "Map from meta-nodes to the WASM-FUNCTION-SPEC objects of their
     meta-node reference functions.")

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

   (node-refs
    :initform (make-hash-map)
    :accessor node-refs
    :documentation
    "Map from `NODE' objects to the offsets, within the data section,
     of the runtime node objects.")

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
     (list* 'import (add-meta-import meta-node)))

    (meta-node
     (list 'meta-node meta-node))))

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

   VALUE-P is a flag for whether the result of the expression is a
   static value which does not require computation.

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
  value-p

  count
  common-p)

(defun value-block-operand-p (block)
  "Returns true if BLOCK represents an operand to the current
   function."

  (emptyp (value-block-instructions block)))


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

   (cyclic-references
    :initform (make-hash-map)
    :accessor cyclic-references
    :documentation
    "Map form `expression-block' objects to the number of cyclic
     references to the block.")

   (strict-blocks
    :accessor strict-blocks
    :documentation
    "Strictness expression of the `EXPRESSION-BLOCK' objects in the
     function's expression.")

   (get-operand
    :accessor get-operand
    :initarg :get-operand
    :documentation
    "Function which returns the `value-block' for an operand of a
     meta-node or node context function."))

  (:documentation
   "Stores the compilation state for a function."))

(defvar *function-block-state* nil
  "The compilation state for the function currently being compiled.")

(defstruct wasm-function-spec
  "Represents a specificiation of a WebAssembly function in terms of
   its parameter types (PARAMS), result types (RESULTS), local
   variable types LOCAL and the instructions comprising the function
   CODE.

   If EXPORT-NAME is non-NIL it designates the name by which the
   function is exported from the module."

  params
  results
  export-name

  locals
  code)

(defun get-operand-local (operand)
  "Returns the value block which references the parameter local
   variable in which the value of OPERAND is stored."

  (with-slots (argument-locals) *function-block-state*
    (ematch operand
      ((node-link- (node))
       (get node argument-locals)))))

(defun compile-function (expression operands &key (get-operand #'get-operand-local) (epilogue (constantly nil)) thunk)
  "Compiles a function comprising the expression EXPRESSION with
   operand nodes OPERANDS. Returns a WASM-FUNCTION-SPEC object."

  (let ((*function-block-state*
         (make-instance 'function-block-state
                        :get-operand get-operand)))
    (foreach #'add-operand operands)

    (let ((tridash.frontend.strictness:*analyze-nodes* nil)
          (tridash.frontend.strictness:*return-blocks* t))
      (setf (strict-blocks *function-block-state*)
            (analyze-expression expression)))

    (let* ((block (compile-expression expression :force-thunk thunk))
           (blocks (flatten-block block))
           (locals (locals-map blocks)))

      (multiple-value-bind (locals code)
          (-> (map-extend #'value-block-instructions blocks)
              (concatenate
               `((local.get (ref ,(value-block-label block))))
               (funcall epilogue (value-block-label block)))

              (make-function-body
               (make-operand-map (length operands))
               locals))

        (make-wasm-function-spec
         :params (coerce (repeat 'i32 (length operands)) 'list)
         :results '(i32)
         :locals locals
         :code code)))))

(defun add-operand (node)
  "Adds a local variable, to *FUNCTION-BLOCK-STATE*, and a
   `VALUE-BLOCK', for the operand node NODE."

  (with-slots (argument-locals) *function-block-state*
    (setf (get node argument-locals)
          (make-value-block :label (next-local)
                            :value-p t))))

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
                  (when (or (value-block-common-p block)
                            (value-block-operand-p block))
                    (incf (get block block-map 0))))
                operands)))

      (nlet add-blocks ((blocks (list block)))
        (setf flat-block (append blocks flat-block))

        (let ((next-blocks (map-extend #'block-operands blocks)))
          (foreach (compose #'add-to-block-map #'value-block-operands) blocks)

          (doseq ((block . count) block-map)
            (unless (value-block-operand-p block)
              (when (= count (value-block-count block))
                (push block next-blocks)
                (erase block-map block))))

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
      optimize-boxing
      (optimize-unboxing locals)
      expand-wasm-macros
      fold-constant-locals

      ;; Making Reference Load and Stores
      optimize-references
      make-load-references
      patch-stack-offsets

      ;; Replace symbolic block labels with relative block indices
      make-branches

      ;; Renumber locals and generate locals declarations
      (make-locals operands)))

(defun make-locals (instructions &optional (local-map (make-hash-map)))
  "Adds local variable declarations, for all local variables used in
   INSTRUCTIONS.

   Returns the list of local variable types in the first return value
   and INSTRUCTIONS, with all local variables labels converted to
   indices and prepended with the local variable declarations, in the
   second return value.

   LOCAL-MAP is the initial map from labels to indices to use. New
   labels are added to the map with the index being the size of the
   map."

  (labels ((map-instruction (instruction)
             (list
              (match instruction
                ((list (and (or 'local.get 'local.set 'local.tee) op) local)
                 `(,op ,(map-local local)))

                (_ instruction))))

           (map-local (local)
             (ensure-get local local-map
               (length local-map))))

    (let ((operands (length local-map))
          (body (map-wasm #'map-instruction instructions)))

      (values
       (coerce (repeat 'i32 (- (length local-map) operands)) 'list)
       body))))

(defun make-branches (instructions)
  "Replace symbolic block labels, in branch instructions, with
   relative block indices."

  (labels ((make-branch (instruction depth labels)
             (match instruction
               ((list* (and (or 'block 'loop 'then 'else)
                            block)
                       (guard label (symbolp label))
                       body)

                (let ((depth (1+ depth))
                      (labels (copy labels)))
                  (setf (get label labels) depth)

                  (cons block (map (rcurry #'make-branch depth labels) body))))

               ((list* (and (or 'block 'loop 'then 'else) block)
                       body)

                (cons block (map (rcurry #'make-branch (1+ depth) labels) body)))

               ((list* 'if body)
                (cons 'if (map (rcurry #'make-branch depth labels) body)))

               ((list (and (or 'br 'br_if) branch) label)
                (list branch (block-index label depth labels)))

               ((list* 'br_table branches)
                (cons 'br_table (map (rcurry #'block-index depth labels) branches)))

               (_ instruction)))

           (block-index (label depth labels)
             (etypecase label
               (integer label)
               (symbol
                (let ((block-depth (get label labels)))
                  (assert (integerp block-depth))
                  (assert (>= depth block-depth))

                  (- depth block-depth))))))

    (map (rcurry #'make-branch 0 (make-hash-map)) instructions)))


(defgeneric compile-expression (expression &key &allow-other-keys)
  (:documentation
   "Compiles a single intermediate expression.

    Returns a `VALUE-BLOCK' structure containing the instructions for
    computing the expression's value."))


;;;; Thunks

(defmethod compile-expression :around (expression &key (thunk nil) (force-thunk nil))
  "If :THUNK is true, compiles EXPRESSION to a thunk function,
   which is added to the THUNK-FUNCTIONS list of *BACKEND-STATE*, and
   returns a `value-block' which creates the thunk object."

  (let ((block (call-next-method)))
    (with-struct-slots value-block- (instructions common-p value-p)
        block

      (if (or force-thunk (and thunk instructions (not common-p) (not value-p)))
          (multiple-value-bind (fn closure)
              (make-thunk-function block)

            (make-thunk (add-thunk-function fn) closure))

          block))))

(defun make-thunk-function (block)
  "Creates a thunk function which computes the value of the
   `value-block' BLOCK.

   Returns two values: the thunk function (WASM-FUNCTION-SPEC) and the
   list of locals comprising the thunk's closure."

  (flet ((load-local (local index)
           `((local.get (ref $c))
             (i32.load (offset ,(* 4 index)))
             (local.set (ref ,(value-block-label local))))))

    (with-struct-slots value-block- (label instructions locals) block
      (multiple-value-bind (blocks closure)
          (flatten-block block)

        (decrement-block-count closure)

        (let ((closure-locals (sort (map-keys closure) :key #'value-block-label)))
          (multiple-value-bind (locals code)
              (->
               (append
                (map-extend #'load-local closure-locals (range 1))
                (map-extend #'value-block-instructions blocks)

                `((local.get (ref ,label))))

               (make-function-body
                (alist-hash-map '(($c . 0)))
                (locals-map blocks)))

            (values
             (make-wasm-function-spec
              :params '(i32)
              :results '(i32)
              :locals locals
              :code code)

             closure-locals)))))))

(defun decrement-block-count (block-map)
  "Decrement block count of each `VALUE-BLOCK' in BLOCK-MAP by the
   number of references to the block."

  (doseq ((block . count) block-map)
    (unless (value-block-operand-p block)
      (decf (value-block-count block)
            (1- count)))))

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
               (i32.store (offset ,(+ 12 (* 4 index)))))))

      (make-value-block
       :label result
       :operands closure

       :instructions
       `((i32.const ,size)
         (call (import "runtime" "alloc"))
         (local.tee (ref ,result))

         (i32.const ,+type-thunk+)
         i32.store

         (local.get (ref ,result))
         (i32.const ,funcref)
         (i32.store (offset 4))

         (local.get (ref ,result))
         (i32.const ,num-locals)
         (i32.store (offset 8))

         ,@(map-extend #'store-local closure (range 0)))))))


;;; Expression Blocks

(defmethod compile-expression ((block expression-block) &rest args &key)
  (with-struct-slots expression-block- (count expression) block
    (if (> count 1)
        (compile-expression-block block)
        (apply #'compile-expression expression args))))

(defun compile-expression-block (block)
  (with-slots (expression-blocks strict-blocks cyclic-references) *function-block-state*
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
                    :thunk (not (strict? strict-blocks block))))

                  (cycles (get block cyclic-references))
                  (ref-count (value-block-count vblock)))

              ;; If VBLOCK is referenced in a cyclic reference, set
              ;; its instructions to create an uninitialized resolved
              ;; thunk. No operands are added to VBLOCK
              ;;
              ;; Then return a new block, with the same label as
              ;; VBLOCK, which depends on VBLOCK and VALUE. The
              ;; instructions of this block initialize the resolved
              ;; thunk's value to VALUE.

              (cond
                (cycles
                 (setf (value-block-count vblock) (1+ cycles))

                 (setf
                  (value-block-instructions vblock)

                  `((i32.const ,(* 3 +word-byte-size+))
                    (call (import "runtime" "alloc"))
                    (local.tee (ref ,result))

                    (i32.const ,+type-resolved-thunk+)
                    i32.store

                    (local.get (ref ,result))
                    (i64.const 0)
                    (i64.store (offset 4))))

                 (make-value-block
                  :label result
                  :operands (list vblock value)

                  :count ref-count
                  :common-p t

                  :instructions

                  `((local.get (ref ,result))
                    (local.get (ref ,(value-block-label value)))
                    (i32.store (offset 4)))))

                (t
                 (setf (value-block-operands vblock)
                       (list value))

                 (setf (value-block-instructions vblock)
                       `((local.get (ref ,(value-block-label value)))
                         (local.set (ref ,result))))

                 vblock))))))))

(defmethod compile-expression ((cycle cyclic-reference) &key)
  "Handles cyclic references. Returns the `value-block' of the
   referenced expression."

  (with-struct-slots cyclic-reference- (expression) cycle
    (check-type expression expression-block)

    (let ((block (get expression (expression-blocks *function-block-state*))))
      (assert block)

      ;; Decrement reference count of block and increment its cyclic
      ;; reference count.

      (decf (value-block-count block))
      (incf (get expression (cyclic-references *function-block-state*) 0))

      (let ((result (next-local)))
        (make-value-block
         :label result
         :operands (list block)
         :instructions
         `((local.get (ref ,(value-block-label block)))
           (local.set (ref ,result))))))))


;;; Functor Expressions

(defconstant +arithmetic-operators+
  (alist-hash-map
   `(("+" . ((i32 . i32.add) (f32 . f32.add)))
     ("-" . ((i32 . i32.sub) (f32 . f32.sub)))
     ("*" . ((i32 . i32.mul) (f32 . f32.mul)))
     ("/" . ((i32 . i32.div_s) (f32 . f32.div_s)))
     ("%" . ((i32 . i32.rem_s) (f32 . f32.rem_s))))))

(defconstant +relational-operators+
  (alist-hash-map
   '(("<" . ((i32 . i32.lt_s) (f32 . f32.lt)))
     ("<=" . ((i32 . i32.le_s) (f32 . f32.le)))
     (">" . ((i32 . i32.gt_s) (f32 . f32.ge)))
     (">=" . ((i32 . i32.ge_s) (f32 . f32.ge))))))


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

      ((get name +relational-operators+)
       (let* ((operands (compile-operands arguments (strict-arguments meta-node)))
              (operand-labels (map #'value-block-label operands))
              (result (next-local)))

         (->> (compile-relational-expression it operand-labels result)
              (make-value-block :label result
                                :operands operands
                                :strict-p t
                                :immediate-p t
                                :instructions))))

      ((= name "if")
       (compile-if-expression arguments))

      (t
       (call-next-method
        meta-node
        (replace-nil arguments)
        outer-nodes)))))

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

(defun replace-nil (arguments)
  "Replace all NIL values in ARGUMENTS with a `value-block' that
   simply sets the argument value to the constant 0 (NULL pointer)."

  (map
   (lambda (arg)
     (or arg
         (let ((label (next-local)))
           (make-value-block
            :label label
            :strict-p t
            :value-p t

            :instructions
            `((i32.const 0)
              (local.set ,label))))))

   arguments))


;;;; Arithmetic Expressions

(defun compile-arithmetic-expression (instruction operands result)
  (let ((finst (get 'f32 instruction))
        (iinst (get 'i32 instruction)))

    (flet ((unbox (label)
             `((block $fail
                 (resolve ,label)
                 (unbox ,label (type number))

                 ;; Branch to either integer or float block This
                 ;; assumes that the type is checked beforehand and
                 ;; cannot be anything other than integer or float

                 (local.get (type ,label))
                 (i32.const ,+type-i32+)
                 i32.eq
                 (br_if $int)
                 (br $float))

               ;; Handle Failure
               (get-fail ,label)
               (box ,result (type fail))
               (br $out))))

      (destructuring-bind (l r) operands
        `((block $out
            (block $type-error
              (block $float
                (block $int
                  ,@(unbox l))

                ;; Left operand is an integer

                (block $float
                  (block $int
                    ,@(unbox r))

                  ;; Right operand is an integer

                  ;; Integer Arithmetic
                  (local.get (value ,l))
                  (local.get (value ,r))
                  ,iinst

                  (box ,result (type i32))
                  (br $out))

                ;; Right operand is a float

                ;; Convert left operand to float
                (local.get (value ,l))
                f32.convert_i32_s

                ;; Reinterpret right operand as a float
                (local.get (value ,r))
                f32.reinterpret_i32

                ;; Floating Point Arithmetic
                ,finst
                (box ,result (type f32))
                (br $out))

              ;; Left operand is a float

              (block $float
                (block $int
                  ,@(unbox r))

                ;; Right operand is an integer

                ;; Reinterpret left operand as float
                (local.get (value ,l))
                f32.reinterpret_i32

                ;; Convert right operand to float
                (local.get (value ,r))
                f32.convert_i32_s

                ;; Floating point arithmetic
                ,finst
                (box ,result (type f32))
                (br $out))

              ;; Right operand is a float

              ;; Reinterpret both operands as floats
              (local.get (value ,l))
              f32.reinterpret_i32
              (local.get (value ,r))
              f32.reinterpret_i32

              ,finst
              (box ,result (type f32))
              (br $out))

            ;; Type error - At least one operand is not a numeric
            ;; value

            ,@(make-type-error result)))))))


;;;; Comparison Expression

(defun compile-relational-expression (instruction operands result)
  (let ((finst (get 'f32 instruction))
        (iinst (get 'i32 instruction)))

    (flet ((unbox (label)
             `((block $fail
                 (resolve ,label)
                 (unbox ,label (type number))

                 ;; Branch to either integer or float block This
                 ;; assumes that the type is checked beforehand and
                 ;; cannot be anything other than integer or float

                 (local.get (type ,label))
                 (i32.const ,+type-i32+)
                 i32.eq
                 (br_if $int)
                 (br $float))

               ;; Handle Failure
               (get-fail ,label)
               (box ,result (type fail))
               (br $out))))

      (destructuring-bind (l r) operands
        `((block $out
            (block $type-error
              (block $float
                (block $int
                  ,@(unbox l))

                ;; Left operand is an integer

                (block $float
                  (block $int
                    ,@(unbox r))

                  ;; Right operand is an integer

                  ;; Integer Comparison
                  (local.get (value ,l))
                  (local.get (value ,r))
                  ,iinst

                  (box ,result (type boolean))
                  (br $out))

                ;; Right operand is a float

                ;; Convert left operand to float
                (local.get (value ,l))
                f32.convert_i32_s

                ;; Reinterpret right operand as a float
                (local.get (value ,r))
                f32.reinterpret_i32

                ;; Floating Point Comparison
                ,finst
                (box ,result (type boolean))
                (br $out))

              ;; Left operand is a float

              (block $float
                (block $int
                  ,@(unbox r))

                ;; Right operand is an integer

                ;; Reinterpret left operand as float
                (local.get (value ,l))
                f32.reinterpret_i32

                ;; Convert right operand to float
                (local.get (value ,r))
                f32.convert_i32_s

                ;; Floating Point Comparison
                ,finst
                (box ,result (type boolean))
                (br $out))

              ;; Right operand is a float

              ;; Reinterpret both operands as floats
              (local.get (value ,l))
              f32.reinterpret_i32
              (local.get (value ,r))
              f32.reinterpret_i32

              ,finst
              (box ,result (type boolean))
              (br $out))

            ;; Type error - At least one operand is not a numeric
            ;; value

            ,@(make-type-error result)))))))


;;;; Selection Expression

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
       `((block $out
           (block $type-error
             (block $fail
               (block $true
                 (block $false
                   (resolve ,test)
                   (unbox ,test (type boolean)))

                 ;; If false
                 (local.get (ref ,else))
                 (local.set (ref ,result))
                 (br $out))

               ;; If true
               (local.get (ref ,then))
               (local.set (ref ,result))
               (br $out))

             ;; Handle Failures
             (get-fail ,test)
             (box ,result (type fail))
             (br $out))

           ;; Type Error - Not a boolean value
           ,@(make-type-error result)))))))

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
                      (i32.store (offset ,offset)))))

             `((block $out
                 (block $type-error
                   (block $fail
                     (resolve ,operator)
                     (unbox ,operator (type funcref))

                     ;; Allocate array for arguments list
                     (i32.const ,(+ 8 (* 4 num-args)))
                     (call (import "runtime" "alloc"))
                     (local.tee (ref ,arg-list))
                     (i32.const ,+type-array+)
                     i32.store

                     ;; Store number of arguments
                     (local.get (ref ,arg-list))
                     (i32.const ,num-args)
                     (i32.store (offset 4))

                     ;; Store arguments
                     ,@(map-extend-to 'list #'push-operand operands (range 8 (+ 8 (* 4 num-args)) 4))

                     (local.get (type ,operator))
                     (i32.const ,+type-funcref+)
                     i32.eq

                     (if (result i32)
                         (then
                          (local.get (ref ,arg-list))
                          (i32.const 4)
                          i32.add
                          (local.get (value ,operator))
                          (call_indirect (type (func (param i32) (result i32)))))

                         (else
                          (local.get (ref ,arg-list))
                          (i32.const 4)
                          i32.add

                          (local.get (ref ,operator))
                          (i32.const 8)
                          i32.add

                          (local.get (ref ,operator))
                          (i32.load (offset 4))
                          (call_indirect (type (func (param i32 i32) (result i32))))))

                     (local.set (ref ,result))
                     (br $out))

                   ;; Handle failure in operator operand
                   (get-fail ,operator)
                   (box ,result (type fail))
                   (br $out))

                 ;; Type Error - Operator not a function reference
                 ,@(make-type-error result))))))))))


;;; Operands

(defun compile-operands (operands strict-operands)
  "Compiles the operands of an expression. Returns the list of
   `VALUE-BLOCK' objects for computing the values of the operands."

  (labels ((compile-operand (operand strict?)
             (compile-expression operand :thunk (null strict?))))

    (map #'compile-operand operands strict-operands)))

(defmethod compile-expression ((link node-link) &key)
  (funcall (get-operand *function-block-state*) link))

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
         :value-p t

         :instructions

         (if (emptyp arguments)
             `((call (import "runtime" "empty_list"))
               (local.set (ref ,result)))

             `((i32.const ,(+ 8 (* num-args 4)))
               (call (import "runtime" "alloc"))
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


(defun make-type-error (local)
  "Generate code which creates a type error failure."

  `((call (import "runtime" "make_fail_type_error"))
    (box ,local (type fail))))


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
              (compile-operands arguments (make-list (length arguments) :initial-element nil))))

        (ensure-get node meta-node-ref-functions
          (make-meta-node-ref-function node))

        (flet ((store-arg (operand index)
                 "Generate instructions which stores the value of
                  OPERAND at INDEX within the defaults array of the
                  function reference."

                 `((local.get (ref ,result))
                   (local.get (ref ,(value-block-label operand)))
                   (i32.store (offset ,(+ 12 (* 4 index)))))))

          (make-value-block
           :label result
           :operands operands

           :instructions
           (if (emptyp operands)

               `((i32.const (meta-node-ref ,node))
                 (box ,result (type funcref)))

               `((i32.const ,(+ 12 (* 4 (length operands))))
                 (call (import "runtime" "alloc"))
                 (local.tee (ref ,result))

                 (i32.const ,+type-funcref-args+)
                 i32.store

                 (local.get (ref ,result))
                 (i32.const (meta-node-ref ,node))
                 (i32.store (offset 4))

                 ;; Store number of default argument values
                 (local.get (ref ,result))
                 (i32.const ,(length operands))
                 (i32.store (offset 8))

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

        (make-ref-function-spec
         meta-node
         (value-block-label block)
         (flatten-block block))))))

(defun make-ref-function-spec (meta-node result blocks)
  "Create the WASM-FUNCTION-SPEC object for a meta-node reference
   function. RESULT is the variable in which the return is stored and
   BLOCKS is the list of blocks comprising the body of the function."

  (with-slots (operands) meta-node
    (labels ((load-operand (index)
               "Generate code which loads an operand from the
                arguments array parameter."

               `((local.get $args)
                 (i32.load (offset ,(+ 4 (* index 4))))
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
               `((local.get $defaults)
                 (i32.load (offset ,offset))
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

               `((block $out
                   (block $empty
                     ;; Check whether has rest arguments
                     (local.get $count)
                     (i32.const ,start)
                     i32.le_u
                     (br_if $empty)

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
                     (call (import "runtime" "alloc"))
                     (local.tee (ref ,start))

                     ;; Store object type
                     (i32.const ,+type-array+)
                     i32.store

                     ;; Store number of arguments
                     (local.get (ref ,start))
                     (local.get $count)
                     (i32.const ,start)
                     i32.sub
                     (i32.store (offset 4))

                     ;; Copy arguments using memcopy
                     (local.get (ref ,start))
                     (i32.const 8)
                     i32.add

                     (local.get (ref $args))
                     (i32.const ,(+ 4 (* 4 start)))
                     i32.add

                     (local.get $rest-size)
                     (call (import "runtime" "memcopy"))
                     (br $out))

                   ;; Set rest argument array to empty
                   (call (import "runtime" "empty_list"))
                   (local.set (ref ,start))))))

      (let* ((num-outer-nodes (length (outer-node-references meta-node)))
             (num-operands (length operands))
             (arity (meta-node-arity meta-node))
             (num-optional (num-optional arity)))

        (multiple-value-bind (locals code)
            (->
             `((local.get $args)
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
               (if (or (plusp num-optional)
                       (plusp num-outer-nodes))

                   '(($args . 0)
                     ($defaults . 1))

                   '(($args . 0))))
              (locals-map blocks)))

          (make-wasm-function-spec
           :params (if (or (plusp num-optional) (plusp num-outer-nodes))
                       '(i32 i32)
                       '(i32))
           :results '(i32)
           :locals locals
           :code code))))))

(defun make-arity-check (arity)
  "Generate code which checks that the number of arguments (stored in
   a $count local variable) is within the ARITY."

  (destructuring-bind (min . max) arity
    `(block $out
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

       (br_if $out)

       (call (import "runtime" "make_fail_arity_error"))
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
                   (i32.store (offset ,(+ 4 (* 4 index)))))))

          (make-value-block
           :label result
           :operands values

           :instructions
           `((i32.const ,(+ 4 (* 4 (length values))))
             (call (import "runtime" "alloc"))
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


;;; Node References

(defconstant +builtin-node-index-offset+ 6)

(defmethod compile-expression ((ref node-ref) &key)
  "Add a Tridash Node object to the constant data section and generate
   code which references the object."

  (with-slots (node-refs) *backend-state*
    (with-struct-slots node-ref- (node) ref
      (ensure-get node node-refs
        (let ((result (next-local))
              (object
               (concatenate
                (byte-encode +type-node+)
                (byte-encode
                 (+ +builtin-node-index-offset+
                    (node-index node))))))

          (make-value-block
           :label result
           :instructions
           `((i32.const (data ,(add-constant-data object)))
             (local.set (ref ,result)))

           :strict-p t
           :value-p t))))))


;;; Literals

;;;; Numbers

(defmethod compile-expression ((value integer) &key)
  (let ((result (next-local)))
    (make-value-block
     :label result
     :instructions
     `((constant ,result ,value))

     :immediate-p t
     :strict-p t
     :value-p t)))

(defmethod compile-expression ((value float) &key)
  (let ((result (next-local)))
    (make-value-block
     :label result
     :instructions (number-literal value 'f32 result)

     :immediate-p t
     :strict-p t
     :value-p t)))

(defun number-literal (value type result)
  (let ((instruction
         (ecase type
           (i32 'i32.const)
           (f32 'f32.const))))

    `((,instruction ,value)
      (box ,result (type ,type)))))


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

     :strict-p t
     :value-p t)))

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
     :strict-p t
     :value-p t)))


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

     :strict-p t
     :value-p t)))


;;;; No Value

(defmethod compile-expression ((null null) &key)
  (let ((result (next-local)))
    (make-value-block
     :label result
     :instructions
     `((call (import "runtime" "make_fail_no_value"))
       (local.set (ref ,result)))

     :value-p t)))


;;; Data Section

(defun add-constant-data (bytes)
  "Add the byte vector BYTES to the constant data section. The offset
   of the location where the byte vector is stored, relative to the
   start of the data section is returned.

   The byte vector is followed by the appropriate amount of padding
   bytes."

  (with-slots (data-section constant-offset) *backend-state*
    (let* ((size (length bytes))
           (padding (mod (- size) +word-alignment+)))

      (setf data-section
            (concatenate
             data-section
             bytes

             (repeat 0 padding)))

      (prog1 constant-offset
        (incf constant-offset (+ size padding))))))


;;; Object Creation

(defun make-tridash-array (var size &key clear (type +type-array+))
  "Generate code which creates a Tridash array, of SIZE elements, and
   stores the reference to the array in the local variable VAR.

   If :CLEAR is true, the generated code initializes the contents of the
   array to zero.

   If :TYPE is provided an array of that TYPE is created, rather than
   an ordinary array."

  ;; Allocate Memory for Array
  `((i32.const ,(* +word-byte-size+ (+ 2 size)))
    (call (import "runtime" "alloc"))
    (local.set (ref ,var))

    ,@(when clear
        `((local.get (ref ,var))
          (i32.const ,(* +word-byte-size+ (+ 2 size)))
          (call (import "runtime" "memclear"))))

    ;; Set Type
    (local.get (ref ,var))
    (i32.const ,type)
    i32.store

    ;; Set Size
    (local.get (ref ,var))
    (i32.const ,size)
    (i32.store (offset ,+word-byte-size+))))


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

(defun optimize-references (instructions)
  "Optimize the loading/storing of references from/onto the stack.

   LOCAL.GET/SET/TEE instructions for REF locals, are replaced with
   instructions for regular locals if it can be determined that there
   is no need to refresh or save the reference at that point."

  (->> instructions
       optimize-reference-loads
       optimize-reference-stores))

(defun optimize-reference-loads (instructions)
  "Replace LOCAL.GET REF instructions with regular LOCAL.GET
   instructions where it can be determined that the reference has
   already been loaded from the stack."

  (let ((live (make-hash-map))
        (at-start t))

    (labels ((optimize (instruction branches)
               "Replace REF locals with regular locals in LOCAL.GET
                instructions."

               (match instruction
                 ;; Reference Loads

                 ((list 'local.get (list 'ref label))
                  (list
                   (if (or (memberp label live) at-start)
                       (list 'local.get label)

                       (progn
                         (setf (get label live) branches)
                         (list 'local.get (list 'ref label))))))


                 ;; Reference Stores

                 ((list (or 'local.set 'local.tee) (list 'ref label))
                  (unless (memberp label live)
                    (setf (get label live) branches))

                  (list instruction))

                 ((list* (or 'call 'call_indirect) _)
                  (clear live)
                  (setf at-start nil)
                  (list instruction))

                 (_
                  (list instruction))))

             (exit-block (label branches)
               (declare (ignore branches))

               (->> (remove-if (curry #'memberp label) live :key #'cdr)
                    (setf live))))

      (optimize-with-blocks instructions #'optimize :block-exit #'exit-block))))

(defun optimize-reference-stores (instructions)
  "Replace LOCAL.SET REF instructions with regular LOCAL.SET
   instructions where it can be determined that there is not need to
   save the reference on the stack."

  (let ((store-indices (make-hash-map))
        (store-blocks (make-hash-map))
        (load-counts (make-hash-map)))

    (labels ((match-load-stores (instruction branches)
               "If INSTRUCTION is a LOCAL.SET REF, assign it a unique
                index. If INSTRUCTION is a LOCAL.GET REF, increment
                the number of loads for the last store (for that
                label) in LOAD-COUNTS."

               (match instruction
                 ;; Set Local Instructions

                 ((list (and (or 'local.set 'local.tee) instruction)
                        (list 'ref label))

                  (let ((index (length load-counts)))
                    (add-index index label branches)
                    (setf (get index load-counts) 0)
                    (setf (get index store-blocks) branches)

                    (list (list instruction (list 'ref label index)))))

                 ((list (or 'local.set 'local.tee)
                        (list 'ref label index))

                  (add-index index label branches)
                  (list instruction))

                 ;; Get Local Instructions

                 ((list 'local.get (list 'ref label))
                  (dolist (index (get label store-indices))
                    (incf (get index load-counts)))

                  (list instruction))

                 (_ (list instruction))))

             (add-index (index label branches)
               (with-hash-keys ((indices label)) store-indices
                 (setf
                  indices
                  (if (and (not (emptyp indices))
                           (= (get (first indices) store-blocks) branches))

                      (cons index (rest indices))
                      (cons index indices)))))

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

      (->> (optimize-with-blocks instructions #'match-load-stores)
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
                    (setf (get label offsets) (+ 4 size))
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
                at OFFSET. TYPE is the element size, I32 or I64."
               `((local.get $stack-base)
                 (,(ecase type
                     (i32 'i32.const)
                     (i64 'i64.const))
                   0)

                 (,(ecase type
                     (i32 'i32.store)
                     (i64 'i64.store))
                   (offset ,(+ 4 offset))))))

      (let ((patched (map-wasm #'patch-offset instructions)))
        (if (zerop size)
            patched
            (add-stack-instructions patched))))))


;;; Optimization

(defun optimize-boxing (instructions)
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
               (case type
                 (f32 (set-float-value local))
                 (i32 (set-int-value local))
                 (otherwise
                  `((box ,local (type ,type))))))

             (set-float-value (local)
               `(i32.reinterpret_f32
                 (local.set (value ,local))

                 (i32.const ,+type-f32+)
                 (local.set (type ,local))))

             (set-int-value (local)
               `((local.set (value ,local))
                 (i32.const ,+type-i32+)
                 (local.set (type ,local)))))

      (walk-wasm #'mark-used instructions)
      (map-wasm #'replace-box instructions))))

(defun fold-constant-locals (instructions)
  "Replaces references to constant values with the values themselves."

  (labels ((mark-constant (constants instruction it)
             "If INSTRUCTION is a constant push instruction followed
              by a LOCAL.SET/TEE, add the local variable to the
              CONSTANTS map."

             (match instruction
               ((and (list* (or 'i32.const 'f32.const) _) constant)
                (when-let (local (set-local? it))
                  (if (memberp local constants)
                      (setf (get local constants) nil)
                      (setf (get local constants) constant))

                  (values nil 1)))

               ((list (or 'local.set 'local.tee) local)
                (setf (get local constants) nil))))

           (set-local? (it)
             "If the next instruction is a LOCAL.SET/TEE return the
              local variable label."

             (advance it)

             (unless (endp it)
               (match (at it)
                 ((list (or 'local.set 'local.tee) local)
                  local))))

           (remove-constants (constants instruction it)
             "Remove LOCAL.SET/TEE instructions for constant locals
              and replace LOCAL.GET instructions with the constant for
              constant locals."

             (match instruction
               ((list* (or 'i32.const 'f32.const) _)
                (replace-next-set constants instruction it))

               ((list 'local.get local)
                (list
                 (or (get local constants)
                     instruction)))

               (_ (list instruction))))

           (replace-next-set (constants instruction it)
             "If the next instruction is a LOCAL.SET/LOCAL.TEE for a
              constant local, return the replacement instructions."

             (advance it)

             (match (start it)
               ((list 'local.set local)
                (if (get local constants)
                    (values nil 1)
                    (list instruction)))

               ((list 'local.tee local)
                (if (get local constants)
                    (values (list instruction) 1)
                    (list instruction)))

               (_ (list instruction)))))

    (let ((constants (make-hash-map)))
      (walk-group (curry #'mark-constant constants) instructions)
      (map-group (curry #'remove-constants constants) instructions))))

(defun optimize-unboxing (instructions locals)
  "Remove unnecessary UNBOX and RESOLVE instructions.

   Does not support LOOP or IF instructions and only supports BLOCK
   and branch instructions with explicit symbolic labels. INSTRUCTIONS
   may contain LOOP or IF instructions however they should not contain
   UNBOX or RESOLVE instructions."

  (let ((unboxed (make-hash-map))
        (resolved (make-hash-map))
        (branches (hash-set)))

    (declare (special unboxed resolved branches))

    (labels ((optimize-instructions (instructions)
               (map-extend #'optimize instructions))

             (optimize (instruction)
               (match instruction
                 ((list* 'block (guard label (symbolp label)) body)
                  (optimize-block label body))

                 ((list (or 'br 'br_if) label)
                  (setf branches (adjoin label branches))
                  (list instruction))

                 ((list* 'br_table labels)
                  (setf branches (union branches (coerce labels 'hash-set)))
                  (list instruction))

                 ((list 'unbox local (list 'type type))
                  (if (or (unboxed? local)
                          (value-block-immediate-p (get local locals)))

                      `((check-type ,local ,type))

                      (progn
                        (setf (get local unboxed) branches)
                        (list instruction))))

                 ((list 'resolve local)
                  (unless
                      (or (resolved? local)
                          (value-block-strict-p (get local locals)))

                    (setf (get local resolved) branches)
                    (list instruction)))

                 (_ (list instruction))))

             (optimize-block (label body)
               (let ((outer-branches branches))
                 (update-let
                     ((unboxed (copy unboxed)
                               (merge label outer-branches old unboxed))

                      (resolved (copy resolved)
                                (merge label outer-branches old resolved))

                      (branches (make-hash-set)
                                (progn (erase branches label)
                                       (union branches old))))

                   (declare (special unboxed resolved branches))

                   (list (list* 'block label (optimize-instructions body))))))

             (merge (label outer-branches old new)
               "Merge the map NEW of resolved/unboxed local variables
                into the MAP OLD. Additionally OUTER-BRANCHES,
                excluding LABEL, is merged into the branches of each
                local variable in NEW."

               (doseq ((local . branches) new)
                 (unless (memberp label branches)
                   (setf (get local old) (union branches outer-branches))))
               old)

             (unboxed? (local)
               (memberp local unboxed))

             (resolved? (local)
               (memberp local resolved)))

      (optimize-instructions instructions))))


;;;; Block Optimization

(defpattern wasm-block (label body)
  `(or (list* 'block (and (type symbol) ,label) ,body)
       (list* 'block ,body)))

(defpattern wasm-loop (label body)
  `(or (list* 'loop (and (type symbol) ,label) ,body)
       (list* 'loop ,body)))

(defun optimize-with-blocks (instructions fn &key block-enter block-exit)
  "Apply the function FN on each instruction in INSTRUCTIONS.

   The function is passed to arguments: the instruction and the set of
   possible branches preceding the instruction, which may result in
   the instruction not being executed.

   The function is not called on structured BLOCK, LOOP, IF, THEN,
   ELSE but only on the sub-instructions comprising the body.

   The function is not applied on branch instructions.

   For LOOP instructions the function is applied twice with the result
   of the first application passed to the second application.

   BLOCK-ENTER, if provided, is a function which is called prior to
   applying FN on the body of a block, with the block label and set of
   possible branches prior to the block, passed as
   arguments. BLOCK-EXIT is called after applying FN on all
   instructions in the body of a block."

  (let ((branches (make-hash-set))
        (blocks (make-hash-map))
        (label-counter 0)
        (depth 0))
    (declare (special branches blocks depth))

    (labels ((optimize-instructions (instructions)
               (map-extend #'optimize instructions))

             (optimize (instruction)
               (match instruction
                 ;; Blocks

                 ((wasm-block label body)
                  `((block ,@(ensure-list label)
                      ,@(optimize-block label body))))

                 ;; Loops

                 ((wasm-loop label body)
                  `((loop ,@(ensure-list label)
                       ,@(optimize-loop label body))))


                 ;; If Blocks

                 ((list* 'if body)
                  `((if ,@(optimize-instructions body))))

                 ((list* 'then body)
                  `((then ,@(optimize-block nil body))))

                 ((list* 'else body)
                  `((else ,@(optimize-block nil body))))


                 ;; Branch Instructions

                 ((list (or 'br 'br_if) label)
                  (setf branches
                        (adjoin (convert-branch label) branches))
                  (list instruction))

                 ((list* 'br_table labels)
                  (-<> (map #'convert-branch labels)
                       (coerce 'hash-set)
                       (union branches)
                       (setf branches <>))

                  (list instruction))

                 (_ (funcall fn instruction branches))))

             (optimize-loop (label body)
               (->> (optimize-block label body)
                    (optimize-block label)))

             (optimize-block (label body)
               (let* ((depth (1+ depth))
                      (blocks (copy blocks))
                      (label (add-label (or label depth))))
                 (declare (special depth blocks))

                 (when block-enter
                   (funcall block-enter label branches))

                 (prog1 (optimize-instructions body)
                   (erase branches label)

                   (when block-exit
                     (funcall block-exit label branches)))))

             (add-label (label)
               (prog1 (setf (get label blocks) label-counter)
                 (incf label-counter)))

             (convert-branch (label)
               (get
                (etypecase label
                  (integer (- depth label))
                  (symbol label))

                blocks)))

      (optimize-instructions instructions))))


;;; Boxing and Unboxing

(defun expand-wasm-macros (instructions)
  "Expand Web Assembly macro instructions."

  (labels ((expand-macro (instruction)
             (match instruction
               ((list 'resolve local)
                `((local.get (ref ,local))
                  (call (import "runtime" "resolve"))
                  (local.set (ref ,local))))

               ((list 'unbox local (list 'type type))
                (unbox-local local type))

               ((list 'check-type local type)
                `((local.get (ref ,local))
                  (i32.const ,+tag-mask+)
                  i32.and
                  (i32.const ,+tag-fail+)
                  i32.eq
                  (br_if $fail)

                  ,@(check-boxed-type local type)))

               ((list 'constant local value)
                (make-constant local value))

               ((list 'box local (list 'type type))
                (box-local local type))

               ((list 'get-fail local)
                `((local.get (ref ,local))))

               (_ (list instruction)))))

    (map-wasm #'expand-macro instructions)))


;;;; Unboxing

(defun unbox-local (local type)
  "Generate code which unboxes the value stored in LOCAL and checks
   that it is of type TYPE."

  (ecase type
    (number (unbox-number local))
    (boolean (unbox-boolean local))
    (funcref (unbox-funcref local))))

(defun unbox-number (local)
  "Generate code which unboxes an immediate or boxed numeric value."

  `((block $out
      (block $immediate                 ; Unbox boxed integer
        (block $pointer                 ; Untag
          (local.get ,local)
          (i32.const ,+tag-mask+)
          i32.and

          (br_table $pointer $immediate $type-error $fail))

        ;; Boxed Value
        (local.get (ref ,local))
        i32.load
        (local.tee (type ,local))

        ;; Check Type
        (i32.const ,+type-f32+)
        i32.gt_u
        (br_if $type-error)

        (local.get (ref ,local))
        (i32.load (offset 4))
        (local.set (value ,local))
        (br $out))

      ;; Immediate Integer
      (i32.const ,+type-i32+)
      (local.set (type ,local))

      (local.get ,local)
      (i32.const 2)
      i32.shr_s
      (local.set (value ,local))
      (br $int))))

(defun unbox-funcref (local)
  "Generates code which unboxes a function reference."

  `((block $out                         ; Unbox immediate integer
      (block $immediate                 ; Unbox boxed integer
        (block $pointer                 ; Untag
          (local.get ,local)
          (i32.const ,+tag-mask+)
          i32.and

          (br_table $pointer $type-error $immediate $fail))

        ;; Boxed Reference
        (local.get (ref ,local))
        i32.load
        (local.tee (type ,local))

        ;; Check Type
        (i32.const ,+type-funcref-args+)
        i32.ne
        (br_if $type-error)
        (br $out))

      ;; Immediate Reference
      (i32.const ,+type-funcref+)
      (local.set (type ,local))

      (local.get (ref ,local))
      (i32.const 2)
      i32.shr_u
      (local.tee (value ,local))

      ;; Check that value is not a boolean
      (i32.const 2)
      i32.lt_u
      (br_if $type-error))))

(defun unbox-boolean (local)
  "Generate code which unboxes a boolean value."

  `((block $unbox                       ; Untag
      (local.get (ref ,local))
      (i32.const ,+tag-mask+)
      i32.and
      (br_table $type-error $type-error $unbox $fail))

    (local.get (ref ,local))
    (i32.const 2)
    i32.shr_u
    (br_table $false $true $type-error)))


;;;; Type Checking

(defun check-boxed-type (local type)
  "Generate code that checks that LOCAL is of type TYPE."

  (ecase type
    (number (check-number local))
    (boolean (check-boolean local))))

(defun check-number (local)
  "Generate code that checks that LOCAL is a number."

  `((local.get (type ,local))
    (i32.const ,+type-f32+)
    i32.gt_u
    (br_if $type-error)))

(defun check-boolean (local)
  "Generate code which checks that LOCAL is a boolean."

  `((block $unbox                       ; Untag
      (local.get (ref ,local))
      (i32.const ,+tag-mask+)
      i32.and
      (br_table $type-error $type-error $unbox $fail))

    (local.get (value ,local))
    (br_table $false $true $type-error)))


;;;; Constants

(defun make-constant (local value)
  "Generate instructions which create the constant VALUE and store it
   in LOCAL. Currently only integer constants are supported."

  (etypecase value
    (integer
     (constant-integer local value))))

(defun constant-integer (local value)
  "Generate instructions which create a boxed integer with value
   VALUE and store it in LOCAL."

  `((i32.const ,value)
    (local.set (value ,local))

    (i32.const ,+type-i32+)
    (local.set (type ,local))

    ,@(if (< +min-immediate-int+ value +max-immediate-int+)

          `((i32.const
             ,(-> (ash value 2)
                  (logior +tag-int+)))

            (local.set (ref ,local)))

          `((i32.const 8)
            (call (import "runtime" "alloc"))
            (local.tee (ref ,local))

            (i32.const ,+type-i32+)
            i32.store

            (local.get (ref ,local))
            (i32.const ,value)
            (i32.store (offset 4))))))


;;;; Boxing

(defun box-local (local type)
  (ecase type
    (f32 (box-float local))
    (i32 (box-integer local))
    (character (box-character local))
    (funcref (box-funcref local))
    (fail
     `((local.set (ref ,local))))

    (boolean (box-boolean local))))

(defun box-float (local)
  `(i32.reinterpret_f32
    (local.set (value ,local))

    (i32.const 8)
    (call (import "runtime" "alloc"))
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
    i32.le_s

    (local.get (value ,local))
    (i32.const ,+min-immediate-int+)
    i32.ge_s

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
         (call (import "runtime" "alloc"))
         (local.tee (ref ,local))

         (i32.const ,+type-i32+)
         i32.store

         (local.get (ref ,local))
         (local.get (value ,local))
         (i32.store (offset 4))))))

(defun box-character (local)
  "Generates code which creates a boxed character object, with the
   reference to it stored in LOCAL."

  `((local.set (value ,local))

    (i32.const 8)
    (call (import "runtime" "alloc"))
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

(defun box-boolean (local)
  "Generate code which boxes the value of LOCAL into a boxed Boolean
   value."

  `((local.set (value ,local))

    (i32.const 6)
    (i32.const 2)
    (local.get (value ,local))
    select

    (local.set (ref ,local))))


;;; Utilities

(defun walk-wasm (fn instructions)
  "Apply FN on each instruction in INSTRUCTIONS.

   If FN returns non-NIL when applied on a block instruction, the
   instructions in the block are skipped."

  (labels ((walk (instruction)
             (unless (funcall fn instruction)
               (match instruction
                 ((list* (or 'block 'if 'then 'else 'loop)
                         instructions)
                  (foreach #'walk instructions))))))
    (foreach #'walk instructions)))

(defun map-wasm (fn instructions)
  "Replace each instruction in INSTRUCTIONS with the result of
   applying FN on the instruction.

   FN is applied on the contents of block instructions and not the
   blocks themselves."

  (labels ((map-instruction (instruction)
             (match instruction
               ((list* (and (or 'block 'if 'then 'else 'loop) block)
                       instructions)

                (list (list* block (map-extend #'map-instruction instructions))))

               ((list* 'result _)
                (list instruction))

               (_ (funcall fn instruction)))))

    (map-extend #'map-instruction instructions)))


(defun walk-group (fn instructions)
  "Apply FN on each instruction in INSTRUCTIONS.

   FN is additionally passed an iterator which can be used to
   lookahead at the following instructions.

   If the first return value of FN is true and the instruction is a
   block, the contents of the block are skipped. If the second value
   is non-NIL, it is interpreted as the number of additional
   instructions to skip (after the current instruction) before the
   next call to FN."

  (doiter (it instructions)
    (let ((instruction (at it)))
      (multiple-value-bind (skip n)
          (funcall fn instruction (copy it))

        (unless skip
          (match instruction
            ((list* (or 'block 'if 'then 'else 'loop)
                    body)
             (walk-group fn body))))

        (when n (advance-n it n))))))

(defun map-group (fn instructions)
  "Map a list of instructions to another list using the function FN.

   FN is applied on each instruction in INSTRUCTIONS and on an
   iterator which can be used to lookahead at the following
   instructions.

   The first return value of FN is interpreted as a list of
   instructions with which to replace the instruction that was passed
   as the first argument.

   If the second value is non-NIL, it is interpreted as the number of
   additional instructions to skip (after the current instruction)
   before the next call to FN. The additional skipped instructions are
   not included in the result."

  (let ((c (make-collector nil)))
    (doiter (it instructions)
      (let ((instruction (at it)))
        (match instruction
          ((list* (and (or 'block 'if 'then 'else 'loop) block)
                  instructions)

           (accumulate c (list* block (map-group fn instructions))))

          ((list* 'result _)
           (accumulate c instruction))

          (_
           (multiple-value-bind (instructions n)
               (funcall fn instruction (copy it))

             (when n
               (advance-n it n))

             (extend c instructions))))))

    (collector-sequence c)))
