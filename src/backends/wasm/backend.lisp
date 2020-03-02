;;;; backend.lisp
;;;;
;;;; Tridash Programming Language.
;;;; Copyright (C) 2020  Alexander Gutev
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

;;;; WebAssembly 32-bit Backend

(in-package :tridash.backend.wasm)

(defconstant +tridash-namespace+ "Tridash"
  "Namespace containing the Tridash runtime library")

(defconstant +tridash-prefix+ (mkstr +tridash-namespace+ ".")
  "Namespace containing the Tridash runtime library definitions.")

(defconstant +node-class+ (mkstr +tridash-prefix+ "Node")
  "Runtime node class name.")

(defconstant +node-context-class+ (mkstr +tridash-prefix+ "NodeContext")
  "Runtime node context class name.")


;;; Linker Options

(defvar *stack-size* (* 64 1024)
  "Amount of memory to allocate for the stack.")

(defparameter *runtime-imports*
  (alist-hash-map
   `(("memory"
      . ,(make-wasm-import
          :module "runtime"
          :name "memory"
          :type 'memory
          :desc (make-wasm-limit :min 1)))

     ("table"
      . ,(make-wasm-import
          :module "runtime"
          :name "table"
          :type 'table
          :desc (make-wasm-limit :min 2)))

     ("resolve"
      . ,(make-wasm-import
          :module "runtime"
          :name "resolve"
          :type 'func
          :desc '((i32) (i32))))

     ("alloc"
      . ,(make-wasm-import
          :module "runtime"
          :name "alloc"
          :type 'func
          :desc '((i32) (i32))))

     ("memcopy"
      . ,(make-wasm-import
          :module "runtime"
          :name "memcopy"
          :type 'func
          :desc '((i32 i32 i32) nil)))

     ("empty_list"
      . ,(make-wasm-import
          :module "runtime"
          :name "empty_list"
          :type 'func
          :desc '(nil (i32))))

     ("fail_type_error"
      . ,(make-wasm-import
          :module "runtime"
          :name "fail_type_error"
          :type 'func
          :desc '(nil (i32))))

     ("arity_error"
      . ,(make-wasm-import
          :module "runtime"
          :name "arity_error"
          :type 'func
          :desc '(nil (i32))))

     ("fail_no_value"
      . ,(make-wasm-import
          :module "runtime"
          :name "fail_no_value"
          :type 'func
          :desc '(nil (i32))))))

  "Runtime library functions import entries.")


;;; Linking

(defclass linker-state ()
  ((node-ids
    :initform (make-hash-map)
    :accessor node-ids
    :documentation
    "Map from `node' objects to their indices.")

   (context-ids
    :initform (make-hash-map)
    :accessor context-ids
    :documentation
    "Map storing the context identifiers of each context of each
     node. Each key is a `NODE' object and the corresponding value is
     a map from context identifiers to their JS identifiers.")

   (context-counter
    :initform 0
    :accessor context-counter
    :documentation
    "Counter for generating globally unique context identifiers.")

   (node-link-indices
    :initform (make-hash-map)
    :accessor node-link-indices
    :documentation
    "Map storing the dependency indices of each dependency of each
     node context. Each key is a `NODE-CONTEXT' object and the
     corresponding value is a map from `NODE-LINK' objects (of the
     dependency node) to their dependency indices.")

   (imports
    :initform (make-hash-map)
    :accessor imports
    :documentation
    "Map from import names to the corresponding WASM-IMPORT
     entries.")))

(defvar *linker-state* nil
  "Global linker state")

(defun node-path (node)
  "Return the name of the variable storing the runtime node object for
   NODE."

  (mkstr "node" (node-index node)))

(defun node-index (node)
  "Returns the index of NODE within the node table object."

  (with-slots (node-ids) *linker-state*
    (ensure-get node node-ids (length node-ids))))


;;; Backend Compile-Nodes Method

(defmethod compile-nodes ((backend (eql :wasm32)) table out-file &optional (options (make-hash-map :test #'cl:equalp)))
  "Compile the node and meta-node definitions, in the `NODE-TABLE'
   TABLE, to 32-bit WebAssembly."

  (with-slots (nodes meta-nodes) table
    (let* ((*backend-state* (make-instance 'backend-state))
           (*linker-state* (make-instance 'linker-state))
           (out-nodes (create-nodes nodes))
           (out-meta-nodes (create-meta-nodes meta-nodes))
           (bindings (init-nodes nodes)))

      (multiple-value-bind (wasm-module table-size memory-size)
          (link-wasm-module out-nodes out-meta-nodes)

        (output-wasm-file out-file wasm-module)

        (output-js-file
         (js-file-name (pathname out-file))

         (concatenate
          (make-loader-code
           out-file

           (concatenate
            (map-extend-to 'list #'first out-nodes)
            bindings)

           options

           :table-size table-size
           :memory-size (1+ (ceiling memory-size (* 64 1024)))
           :runtime-base memory-size
           :num-nodes (length out-nodes)))

         options)))))

(defun output-wasm-file (out-file module)
  "Serialize the WebAssembly module MODULE to the file at OUT-FILE."

  (with-open-file (stream out-file
                          :direction :output
                          :if-exists :supersede
                          :element-type 'unsigned-byte)

    (serialize-module module stream)))



;;; JavaScript Loader Code

(defun js-file-name (wasm-file)
  "Return the name of the output JavaScript file derived from the name
   of the output WebAssembly File."

  (cl-fad:merge-pathnames-as-file
   (cl-fad:pathname-directory-pathname wasm-file)
   (make-pathname :name (pathname-name wasm-file) :type "js")))

(defun output-js-file (out-file code options)
  "Serialize the JavaScript AST nodes in CODE to the file at
   OUT-FILE."

  (let ((*print-indented* (parse-boolean (get "indented" options))))

   (with-open-file (*standard-output* out-file :direction :output :if-exists :supersede)
     (output-code code))))

(defun make-loader-code (out-file code options &key table-size memory-size runtime-base num-nodes)
  "Generate the code which loads the WebAssembly runtime and compiled
   modules. CODE is the JavaScript node initialization code which
   should be executed after the modules are loaded."

  (let ((runtime (or (get "runtime-path" options) "runtime.wasm"))
        (module-path (or (get "module-path" options) "")))

    (list
     (-<>
      (js-call
       (js-member +tridash-namespace+ "load_sync")
       (js-object
        (list
         (list "runtime_path" (js-string runtime))
         (list "module_path" (js-string
                              (cl-fad:merge-pathnames-as-file module-path (file-namestring out-file))))
         (list "table_size" table-size)
         (list "memory_size" memory-size)
         (list "memory_base" runtime-base)
         (list "num_nodes" num-nodes))))

      (js-member "then")
      (js-call
       (js-lambda
        (list
         (js-object
          (list
           (list "module" "mod")
           (list "runtime" "runtime")
           (list "memory" "memory"))))

        (concatenate
         (list (js-var "nodes" (js-object)))
         code
         (list
          (js-return
           (js-object
            (list (list "module" "mod")
                  (list "runtime" "runtime")
                  (list "memory" "memory")
                  (list "nodes" "nodes"))))))))

      (js-call "=" (js-member "exports" "module") <>)))))



;;; Imports

(defun add-meta-import (meta-node &optional (state *linker-state*))
  "Adds META-NODE, which should be an `external-meta-node', to the
   module's map of imported functions. The parameter signature is
   deduced from the operands and the return type signature is assumed
   to be (i32)."

  (check-type meta-node external-meta-node)

  (match (attribute :wasm-name meta-node)
    ((or (list (eq (id-symbol "."))
               (and (type string) module)
               (and (type string) name))

         (list (and (type string) name)))

     (let ((key (list (or module "default") name)))
       (ensure-get key (imports state)
         (make-wasm-import
          :module module
          :name name
          :type 'func
          :desc (list (repeat 'i32 (length (operands meta-node))) '(i32))))

       key))

    (_
     (error 'undefined-external-meta-node-error
            :backend "Wasm32"
            :meta-node meta-node))))


;;; Nodes

(defun create-nodes (nodes)
  "Generate the code for each node in NODES. Each element is a list of
   two values, the JavaScript node definitions and the WebAssembly
   functions."

  (flet ((make-node (node)
           (multiple-value-list
            (create-node node))))
    (map-to 'list #'make-node nodes)))

(defgeneric create-node (node)
  (:documentation
   "Generate the definition for the `node' object NODE.

    Returns two values: the JavaScript code responsible for creating
    the runtime node object and the map of WebAssembly functions
    implementing each context's value function"))

(defmethod create-node (node)
  (flet ((make-context (context)
           (multiple-value-bind (js-code wasm-fn)
               (create-context node context)

             (list js-code (ensure-list wasm-fn)))))

    (let* ((contexts (map-to 'list #'make-context (contexts node)))
           (code (map #'first contexts))
           (fns (map-extend #'second contexts)))

      (values
       (concatenate
        (list (js-var (node-path node) (js-new +node-class+)))
        (store-in-public-nodes node (node-path node))
        code)

       fns))))

(defun create-context (node context)
  "Generate the initialization code for a `node-context', returned in
   the first value, and its WebAssembly function, returned in the
   second value."

  (flet ((make-store-value (label)
           `((i32.const $stack-base)
             (i32.const ,(* 4 (node-index node)))
             i32.add
             (local.get ,label)
             i32.store)))

   (destructuring-bind (id . context) context
     (with-slots (value-function operands) context
       (let* ((node-path (node-path node))
              (gcid (global-context-id)))

         (let*-if ((export-name (mkstr "c" gcid))
                   (context-fn
                    (compile-function value-function (map-keys operands)
                                      :epilogue #'make-store-value)))

             value-function

           (establish-dependency-indices context)

           (when context-fn
             (setf (wasm-function-spec-export-name context-fn)
                   export-name))

           (values
            (lexical-block
             (js-var "context" (make-context-expression node-path id context gcid))

             (when context-fn
               (js-call "="
                        (js-member "context" "compute")
                        (js-lambda
                         (list "previous" "values")
                         (list
                          (js-return
                           (make-js-call
                            (js-members "mod" "exports" export-name)
                            (map-to 'list
                                    (curry #'js-element "values")
                                    (range 0 (length operands)))))))))

             (js-call "=" (context-path node id) "context"))

            context-fn)))))))

(defun context-path (node context-id)
  "Returns a JS expression which references the context, with
   identifier CONTEXT-ID, of NODE."

  (js-element (js-member (node-path node) "contexts")
              (context-js-id node context-id)))

(defun context-js-id (node context-id)
  "Returns the JavaScript context identifier for the context with
   identifier CONTEXT-ID of NODE."

  (let ((ids (ensure-get node (context-ids *linker-state*) (make-hash-map))))
    (case context-id
      (:input
       (js-string "input"))

      (otherwise
       (ensure-get context-id ids (length ids))))))


(defun make-context-expression (node-path id context gcid)
  "Creates an expression which creates a new context for the context
   CONTEXT with id ID of the node which is referenced by the
   expression NODE-PATH. GCID is the global context identifier of the
   context."

  (case id
    (:input
     (js-call (js-member +node-context-class+ "create_input")
              node-path
              (global-context-id)))

    (otherwise
     (js-call (js-member +node-context-class+ "create")
              node-path
              (length (operands context))
              gcid))))

(defun global-context-id ()
  "Returns a new unique global context identifier."

  (with-slots (context-counter) *linker-state*
    (prog1 context-counter
      (incf context-counter))))

(defun establish-dependency-indices (context)
  "Establishes the indices of the operands of the `NODE-CONTEXT'
   CONTEXT, and adds them to the node link indices map of
   *LINKER-STATE*."

  (setf (get context (node-link-indices *linker-state*))
        (map-to 'hash-map #'cons
                (map-keys (operands context))
                (range 0))))


(defun store-in-public-nodes (node expr)
  "If NODE has a :PUBLIC-NAME attribute returns an assignment
   expression which stores EXPR in 'Tridash.nodes' under the key,
   given by the value of the :PUBLIC-NAME attribute"

  (awhen (attribute :public-name node)
    (-<>(js-element "nodes" (js-string it))
         (js-call "=" <> expr)
         list)))


;;; Node Bindings

(defun init-nodes (nodes)
  "Generate the binding initialization code of each `NODE' in NODES."

  (map-extend-to 'list #'init-node nodes))

(defun init-node (node)
  "Generate the JS code which initializes the bindings of NODE to its
   observers."

  (map-extend-to 'list (curry #'init-context node) (contexts node)))

(defun init-context (node context)
  "Generate the JS code which initializes the bindings of a node
   context to the node's observers."

  (destructuring-bind (id . context) context
    (bind-observers node id context)))

(defun bind-observers (node context-id context)
  "Generates code which binds the `NODE' NODE to its observers."

  (with-slots (operands) context
    (let ((context-path (context-path node context-id)))
      (labels ((reserve-observer (observer index weak-p)
                 (-> (js-member observer "reserve")
                     (js-call
                      "start" index "path" "value" "visited" (if weak-p "true" "false"))))

               (observer-index (observer link)
                 (-<>> link
                       node-link-context
                       (context observer)
                       (dependency-index <> node)))

               (dependency-index (context node)
                 (->> (node-link-indices *linker-state*)
                      (get context)
                      (get node)))

               (observer-context (observer link)
                 (->> link
                      node-link-context
                      (context-path observer)))

               (make-reserve (observer)
                 (destructuring-bind (observer . link) observer
                   (reserve-observer
                    (observer-context observer link)
                    (observer-index observer link)
                    (node-link-weak-p link))))

               (two-way-p (observer)
                 (and (node-link-two-way-p (cdr observer))
                      (get (car observer) operands)))

               (add-observer (observer)
                 (js-call
                  (js-member context-path "add_observer")
                  (context-path (car observer)
                                (node-link-context (cdr observer)))
                  (observer-index (car observer) (cdr observer)))))


        (let ((observers (coerce (remove-if #'two-way-p (observers node)) 'alist)))
          (concatenate
           (map #'add-observer observers)

           (list
            (js-call
             "="
             (js-member context-path "reserveObservers")

             (js-lambda
              '("start" "value" "path" "visited")

              (list
               (js-return
                (js-call
                 (js-member "Promise" "all")
                 (js-array
                  (map #'make-reserve observers))))))))))))))


;;; Meta-Nodes

(defun create-meta-nodes (meta-nodes)
  "Compile each meta-node in META-NODES to a WebAssembly
   function. Returns an association list where the keys are the
   meta-nodes and the values are their corresponding WebAssembly
   functions."

  (labels ((union-meta-nodes (set meta-nodes)
             "Adds all meta-nodes, and the meta-nodes nested in their
              definitions, to SET."

             (foreach (curry #'union-meta-node set) meta-nodes))

           (union-meta-node (set meta-node)
             "Adds META-NODE, and the meta-nodes nested in its
              definition, to SET."

             (unless (memberp meta-node set)
               (unless (external-meta-node? meta-node)
                 (nadjoin meta-node set)

                 (->> meta-node
                      definition
                      meta-nodes
                      (union-meta-nodes set))))))

    (let ((all-meta-nodes (make-hash-set)))
      (union-meta-nodes all-meta-nodes meta-nodes)
      (map-to 'list #'compile-meta-node all-meta-nodes))))

(defun compile-meta-node (meta-node)
  "Compile META-NODE to a WebAssembly function."

  (with-slots (contexts) meta-node
    (let* ((operands
            (concatenate
             (operand-nodes meta-node)
             (outer-node-operand-nodes meta-node)))

           (context (cdr (first contexts))))

      (with-slots (value-function) context
        (cons meta-node
              (compile-function value-function operands))))))


;;; Linking WebAssembly Code

(defun link-wasm-module (nodes meta-nodes)
  "Link the WebAssembly functions of NODES and META-NODES into a
   single module."

  (with-slots (imports) *linker-state*
    (multiple-value-bind (functions function-indices table)
        (link-functions
         (map-extend #'second nodes)
         meta-nodes)

      (extract-runtime-imports functions imports)

      (let ((export-names (exported-functions functions)))
        (multiple-value-bind (function-types functions imports)
            (patch-function-types functions (map-values imports))

          (multiple-value-bind (functions index-offset)
              (patch-imports functions imports)

            (let* ((functions
                    (->
                     (patch-function-indices functions index-offset function-indices table)
                     (patch-data-references *stack-size*)))

                   (exports (make-exports export-names function-indices index-offset))
                   (table-size (length table))
                   (table (make-table table index-offset))
                   (data (make-data-section (data-section *backend-state*) *stack-size*))
                   (memory-size (+ *stack-size* (length (data-section *backend-state*)))))

              (values
               (make-wasm-module
                :types function-types
                :imports (add-import-table-memory imports (+ 2 table-size) (ceiling memory-size (* 64 1024)))
                :functions functions
                :exports exports
                :elements (list table)
                :data (list data))

               (+ 2 table-size)
               memory-size))))))))

(defun add-import-table-memory (imports table-size mem-size)
  "Add entries for a table, with initial size TABLE-SIZE, and memory
   with initial number of pages MEM-SIZE to IMPORTS."

  (concatenate
   (list
    (make-wasm-import
     :module "runtime"
     :name "table"
     :type 'table
     :desc (make-wasm-limit :min table-size))

    (make-wasm-import
     :module "runtime"
     :name "memory"
     :type 'memory
     :desc (make-wasm-limit :min mem-size)))

   imports))

(defun link-functions (context-fns meta-node-fns)
  "Combine node context functions, meta-node functions, meta-node
   reference functions and thunk functions into a list.

   Returns three values:

   - List of all functions.

   - A map from function identifiers to the corresponding function
     indices. This includes a mapping for each exported
     function. Note: these indices are relative to the index of the
     first function

   - A map from function identifiers to the corresponding indices
     within the indirect call function reference table."

  (with-slots (thunk-functions meta-node-ref-functions) *backend-state*

    (let ((function-indices (make-hash-map))
          (table (make-hash-map))
          (functions (make-array (length context-fns)
                                 :adjustable t
                                 :fill-pointer t
                                 :initial-contents context-fns)))

      (labels ((add-function-index (entity function)
                 (->> (setf (get entity function-indices)
                            (length functions))

                      (add-export function)))

               (add-export (function index)
                 (awhen (wasm-function-spec-export-name function)
                   (setf (get it function-indices) index)))

               (add-to-table (thing index)
                 (setf (get thing table)
                       (list (+ 2 (length table)) index))))

        (foreach #'add-export functions (range 0))

        (doseq ((meta-node . fn) meta-node-fns)
          (add-function-index meta-node fn)
          (vector-push-extend fn functions))

        (doseq ((meta-node . fn) meta-node-ref-functions)
          (->> (add-function-index (list 'meta-node-ref meta-node) fn)
               (add-to-table (list 'meta-node-ref meta-node)))

          (vector-push-extend fn functions))

        (doseq ((thunk . fn) thunk-functions)
          (->> (add-function-index (list 'thunk thunk) fn)
               (add-to-table (list 'thunk thunk)))

          (vector-push-extend fn functions))

        (values
         functions
         function-indices
         table)))))


;;;; Function Type Signatures

(defun patch-function-types (functions imports)
  "Create a list of all function type signatures, of the functions in
   FUNCTIONS and the imported functions in IMPORTS, and replace
   references to the signatures in, the instructions comprising each
   function, with the actual type indices.

   Returns three values:

   - List of WASM-FUNCTION-TYPE objects describing each function type
     signature.

   - List of WASM-FUNCTION objects corresponding to each function,
     with the patched type signatures.

   - Map of imports with function imports patched with the correct
     type signature index."

  (let ((types (make-hash-map)))
    (labels ((make-fn (function)
               (with-struct-slots wasm-function-spec-
                   (params results locals code)
                   function

                 (make-wasm-function
                  :type (type-index params results)
                  :locals locals
                  :code (patch-types code))))

             (type-index (params result)
               (ensure-get (list params result) types (length types)))

             (patch-types (instructions)
               (map-wasm #'patch-type instructions))

             (patch-type (instruction)
               (match instruction
                 ((list 'call_indirect
                        (list 'type (list 'func (list* 'param params) (list* 'result results))))
                  `((call_indirect ,(type-index params results))))

                 (_ (list instruction))))

             (patch-import (import)
               (match import
                 ((wasm-import- (type (eq 'func))
                                (desc (list params results))
                                module
                                name)

                  (make-wasm-import
                   :module module
                   :name name
                   :type 'func
                   :desc (type-index params results)))

                 (_ import)))

             (make-type (type)
               (destructuring-bind (params results) type
                 (make-wasm-function-type :params params :results results))))

      (let ((functions (map #'make-fn functions))
            (imports (map #'patch-import imports)))

        (values
         (-<> (coerce types 'alist)
              (sort :key #'cdr)
              (map (compose #'make-type #'car) <>))

         functions
         imports)))))


;;;; Imports

(defun extract-runtime-imports (functions imports)
  "Add an entry for each runtime library function, that is invoked in
   the body of at least one of FUNCTIONS, to IMPORTS."

  (labels ((extract (function)
             (walk-wasm #'extract-instruction (wasm-function-spec-code function)))

           (extract-instruction (instruction)
             (match instruction
               ((list 'call (list 'import "runtime" name))
                (ensure-get (list "runtime" name) imports
                  (let ((import (get name *runtime-imports*)))
                    (check-type import wasm-import)
                    import))))))

    (foreach #'extract functions)))

(defun patch-imports (functions imports)
  "Patch calls to imported functions with the imported function
   indices. Returns the list of patched functions and the number of
   imported functions."

  (let* ((func-map
          (map-to
           'hash-map

           (lambda (import index)
             (with-struct-slots wasm-import- (module name) import
               (cons (list module name) index)))

           (remove-if-not (curry #'= 'func) imports :key #'wasm-import-type)
           (range 0))))

    (values
     (patch-function-instructions
      (lambda (instruction)
        (match instruction
          ((list 'call (list* 'import name))
           (let ((index (get name func-map)))
             (assert index)

             `(call ,index)))

          (_ instruction)))

      functions)

     (length func-map))))


;;;; Function Indices

(defun patch-function-indices (functions offset indices table)
  "Patch call instructions with the corresponding function index in
   INDICES, and patch references to meta-node-ref/thunk functions with
   the corresponding table element index in TABLE. OFFSET is added to
   each index."

  (patch-function-instructions
   (lambda (instruction)
     (match instruction
       ((list 'call (list 'meta-node (and (type meta-node) meta-node)))
        (let ((index (get meta-node indices)))
          (check-type index (integer 0))
          (list 'call (+ offset index))))

       ((list 'i32.const
              (and
               (or (list 'meta-node-ref (type meta-node))
                   (list 'thunk (type (integer 0))))
               thing))

        (let ((index (first (get thing table))))
          (check-type index (integer 0))
          (list 'i32.const (+ offset index))))

       (_ instruction)))

   functions))


;;;; References to constant data

(defun patch-data-references (functions data-start)
  "Patch references to objects within the constant data section, with
   the corresponding memory addresses, and patch references to the
   global stack pointer with the address at which it is
   stored. DATA-START is the address of the first byte of the constant
   memory section.

   NOTE: The stack pointer is assumed to be stored at the address
   immediately following the last byte of the constant data memory
   section. This also assumes that the size of DATA-SECTION (of
   *BACKEND-STATE*) is a multiple of the word size."

  (let ((stack-ptr (+ data-start (length (data-section *backend-state*)))))
    (patch-function-instructions
     (lambda (instruction)
       (match instruction
         ((list 'i32.const (list 'data offset))
          (list 'i32.const (+ data-start offset)))

         ((list 'i32.const '$stack-ptr)
          (list 'i32.const stack-ptr))

         ((list 'i32.const '$stack-base)
          (list 'i32.const (- *stack-size* 4)))

         (_ instruction)))

     functions)))


;;;; Exports

(defun exported-functions (functions)
  "Return the export names of the functions which are to be exported."

  (remove nil (map #'wasm-function-spec-export-name functions)))

(defun make-exports (names indices offset)
  "Create the list of WebAssembly export entries (WASM-EXPORT
   objects).

   NAMES is the list of the export names for the functions and INDICES
   is the map containing the mappings from export names to function
   indices. OFFSET is added to each function index."

  (map
   (lambda (name)
     (let ((index (get name indices)))
       (check-type index (integer 0))

       (make-wasm-export
        :name name
        :type 'func
        :index (+ offset index))))
   names))


;;;; Function Reference Table

(defun make-table (table offset)
  "Create a table element initialization entry which initializes the
   function reference table with the entries in TABLE, which is a map
   in which the keys are lists of the form (ELEMENT-INDEX
   . FUNC-INDEX). OFFSET is added to each function index."

  (make-wasm-table
   :index 0
   :offset '((i32.const 2))
   :functions
   (map (compose (curry #'+ offset) #'second)
        (sort (map-values table) :key #'first))))


;;;; Data Section

(defun make-data-section (bytes start)
  "Create a memory data initialization entry which initializes the
   memory bytes, beginning at address START to the contents of the
   byte array BYTES."

  (make-wasm-data :offset `((i32.const ,start)) :bytes bytes))


;;;; Utility Functions

(defun patch-function-instructions (fn functions)
  "Transforms each instruction, of each function in FUNCTIONS, by
   applying FN on the instruction."

  (flet ((patch-function (function)
           (with-struct-slots wasm-function- (type locals code)
               function

             (make-wasm-function
              :type type :locals locals
              :code (map-wasm (compose #'list fn) code)))))
    (map #'patch-function functions)))
