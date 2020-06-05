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

(defconstant +node-interface-class+ (js-member +tridash-namespace+ "Node")
  "Node class type which servers as an interface to the node object.")

(defconstant +module-class+ (js-member +tridash-namespace+ "Module")
  "Tridash module class.")


;;; Linker Options

(defvar *runtime-library-path* "backends/wasm/runtime.wasm")

(defvar *stack-size* (* 64 1024)
  "Amount of memory to allocate for the stack.")

(defparameter *runtime-imports*
  (alist-hash-map
   `(("memory"
      . ,(make-wasm-import
          :module "runtime"
          :name "memory"
          :type :memory
          :desc (make-wasm-limit :min 1)))

     ("table"
      . ,(make-wasm-import
          :module "runtime"
          :name "table"
          :type :table
          :desc (make-wasm-limit :min 2)))

     ("resolve"
      . ,(make-wasm-import
          :module "runtime"
          :name "resolve"
          :type :func
          :desc '((i32) (i32))))

     ("alloc"
      . ,(make-wasm-import
          :module "runtime"
          :name "alloc"
          :type :func
          :desc '((i32) (i32))))

     ("memcopy"
      . ,(make-wasm-import
          :module "runtime"
          :name "memcopy"
          :type :func
          :desc '((i32 i32 i32) nil)))

     ("memclear"
      . ,(make-wasm-import
          :module "runtime"
          :name "memclear"
          :type :func
          :desc '((i32 i32) nil)))

     ("copy_array"
      . ,(make-wasm-import
          :module "runtime"
          :name "copy_array"
          :type :func
          :desc '((i32) (i32))))

     ("empty_list"
      . ,(make-wasm-import
          :module "runtime"
          :name "empty_list"
          :type :func
          :desc '(nil (i32))))

     ("fail_type_error"
      . ,(make-wasm-import
          :module "runtime"
          :name "fail_type_error"
          :type :func
          :desc '(nil (i32))))

     ("make_fail_type_error"
      . ,(make-wasm-import
          :module "runtime"
          :name "make_fail_type_error"
          :type :func
          :desc '(nil (i32))))

     ("fail_arity_error"
      . ,(make-wasm-import
          :module "runtime"
          :name "fail_arity_error"
          :type :func
          :desc '(nil (i32))))

     ("make_fail_arity_error"
      . ,(make-wasm-import
	  :module "runtime"
	  :name "make_fail_arity_error"
	  :type :func
	  :desc '(nil (i32))))

     ("fail_no_value"
      . ,(make-wasm-import
          :module "runtime"
          :name "fail_no_value"
          :type :func
          :desc '(nil (i32))))

     ("make_fail_no_value"
      . ,(make-wasm-import
          :module "runtime"
          :name "make_fail_no_value"
          :type :func
          :desc '(nil (i32))))

     ("frem"
      . ,(make-wasm-import
          :module "runtime"
          :name "frem"
          :type :func
          :desc '((f32 f32) (f32))))))

  "Runtime library functions import entries.")


;;; Linking

(defclass linker-state ()
  ((node-ids
    :initform (make-hash-map)
    :accessor node-ids
    :documentation
    "Map from `node' objects to their indices.")

   (context-indices
    :initform (make-hash-map)
    :accessor context-indices
    :documentation
    "Map storing the context identifiers of each context of each
     node. Each key is a `NODE' object and the corresponding value is
     a map from context identifiers to their JS identifiers.")

   (public-nodes
    :initform (make-hash-map)
    :accessor public-nodes
    :documentation
    "Map from public node identifiers to the corresponding node index
     or meta-node identifier.")

   (initial-values
    :initform nil
    :accessor initial-values
    :documentation
    "List of initial node values to set, where each element is an
     INITIAL-VALUE object.")

   (imports
    :initform (make-hash-map)
    :accessor imports
    :documentation
    "Map from import names to the corresponding WASM-IMPORT
     entries."))

  (:documentation
   "WebAssembly Module Linker State."))

(defstruct initial-value
  "Node Initial Value.

   NODE-INDEX - Index of the node.
   CONTEXT-INDEX - Index of the context.
   EXPRESSION - Initial value expression"

  node-index
  context-index)

(defstruct node-export
  "Represents a node which is to be exported from the generated
   module.

   TYPE is the type of node, NODE or META-NODE.

   ID is the identifier with which the node is made available to
   external callers."

  type
  id)

(defvar *linker-state* nil
  "Global linker state")


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
           (compute (create-compute-function table))
           (init-state
            (create-init-state-function (length nodes) (initial-values *linker-state*))))

      (multiple-value-bind (wasm-module table-size memory-size)
          (link-wasm-module compute init-state out-nodes out-meta-nodes)

        (output-files
         out-file
         wasm-module

         (map-extend-to 'list #'first out-nodes)
         options

         :table-size table-size
         :memory-size (1+ (ceiling memory-size (* 64 1024)))
         :runtime-base memory-size
         :num-nodes (length out-nodes)
         :stack-size *stack-size*
         :imports (map-values (imports *linker-state*)))))))

(defun output-wasm-file (out-file module)
  "Serialize the WebAssembly module MODULE to the file at OUT-FILE."

  (with-open-file (stream out-file
                          :direction :output
                          :if-exists :supersede
                          :element-type 'unsigned-byte)

    (serialize-module module stream)))

(defun output-files (out-file wasm-module js-code options &rest load-options &key &allow-other-keys)
  "Write the generated code to the output files.

   OUT-FILE is the name of the primary WASM output file.
   WASM-MODULE is the generated WebAssembly Module.
   JS-CODE is the generated JavaScript code.
   OPTIONS is a map containing command-line output options.
   LOAD-OPTIONS are the keyword arguments passed to MAKE-LOADER-CODE."

  (with-hash-keys ((type "type")
                   (module-name "module-name")
                   (main-ui "main-ui")
                   (linkage "linkage"))
      options

    (let ((linkage (parse-linkage linkage))
          (code
           (apply #'make-loader-code out-file js-code wasm-module options load-options)))

      (unless (= linkage 'embed)
        (output-wasm-file out-file wasm-module))

      (match type
        ((cl:equalp "html")

         (with-open-file (*standard-output* out-file :direction :output :if-exists :supersede)
           (let ((*print-indented* (parse-boolean (get "indented" options))))
             (create-html-file
              (wrap-js-module
               (list* (js-var "exports" (js-object)) code)
               module-name)

              (get-root-node main-ui)))))

        (_
         (-> (pathname out-file)
             js-file-name
             (output-js-file code options)))))))

(defun wrap-js-module (code module-name)
  "If MODULE-NAME is non-NIL wrap CODE in an anonymous function with
   the result assigned to the variable MODULE-NAME."

  (if module-name

      (->> (js-member "exports" "module")
           js-return
           (list code)
           (js-lambda nil)
           js-call
           (js-var module-name)
           list)

      (lexical-block code)))


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

(defun make-loader-code (out-file code wasm-module options &key table-size memory-size runtime-base num-nodes stack-size imports)
  "Generate the code which loads the WebAssembly runtime and compiled
   modules. CODE is the JavaScript node initialization code which
   should be executed after the modules are loaded."

  (let ((runtime (or (get "runtime-path" options) *runtime-library-path*))
        (module-path (or (get "module-path" options) out-file)))

    (let ((linkage (parse-linkage (get "linkage" options))))

      (list
       (-<>
        (js-call
         (module-load-function linkage)

         (module-path linkage module-path wasm-module)
         (module-runtime-path linkage runtime)

         (js-object
          (list
           (list "table_size" table-size)
           (list "memory_size" memory-size)
           (list "memory_base" runtime-base)
           (list "stack_size" stack-size)
           (list "imports" (make-import-functions imports)))))

        (js-member "then")
        (js-call
         (js-lambda
          (list
           (js-object
            (list
             (list "module" "mod")
             (list "marshaller" "marshaller")
             (list "runtime" "runtime")
             (list "memory" "memory"))))

          (concatenate
           (list
            (make-module
             (js-members "mod" "exports" "compute")
             (js-members "mod" "exports" "init")
             "marshaller"
             num-nodes)

            (js-var "nodes" (make-exported-nodes (public-nodes *linker-state*))))

           code
           (list
            (js-return
             (js-object
              (list
               (list "module" "module")
               (list "runtime" "runtime")
               (list "memory" "memory")
               (list "nodes" "nodes")
               (list "set_values"
                     (js-call
                      (js-members "module" "set_node_values" "bind")
                      "module")))))))))

        (js-call "=" (js-member "exports" "module") <>))))))

(defun make-exported-nodes (public-nodes)
  "Generate code which creates the object storing the exported
   nodes (in PUBLIC-NODES)."

  (js-object
   (map-to
    'list

    (lambda (export)
      (destructuring-bind (name . export) export
        (list
         (js-string name)

         (with-struct-slots node-export- (type id) export
           (ecase type
             (node
              (destructuring-bind (node-index context-index) id
                (js-new +node-interface-class+
                        (list* "module" node-index (ensure-list context-index)))))

             (meta-node
              (js-members "mod" "exports" name)))))))

    public-nodes)))

(defun make-import-functions (imports)
  "Generate code which creates the object containing the functions
   imported by the module."

  (labels ((make-import-module (module)
             (destructuring-bind (name . imports) module
               (list
                (js-string name)

                (js-object
                 (map #'make-import imports)))))

           (make-import (import)
             (with-struct-slots wasm-import- (module name) import
               (list (js-string name) (function-name module name))))

           (function-name (module name)
             (if (= module "default")
                 name
                 (js-member module name)))

           (group-by-module (imports)
             (unless (emptyp imports)
               (let ((it (iterator imports)))
                 (nlet group ((groups nil))
                   (if (endp it)
                       groups

                       (let ((module (-> (at it)
                                         wasm-import-module
                                         module-name)))

                         (-<> (keep-same-module module it)
                              (cons module <>)
                              (cons groups)
                              group)))))))

           (keep-same-module (module it)
             (nlet keep ((imports nil))
               (if (endp it)
                   imports

                   (let ((import (at it)))
                     (if (= (module-name (wasm-import-module import)) module)
                         (progn
                           (advance it)
                           (keep (cons import imports)))
                         imports)))))

           (module-name (module)
             (typecase module
               ((or string symbol) (string module))
               (otherwise module))))

    (-<> (remove-if-not (curry #'= :func) imports :key #'wasm-import-type)
         (remove "runtime" <> :key #'wasm-import-module)
         (sort :key (compose #'module-name #'wasm-import-module))
         group-by-module
         (map #'make-import-module <>)
         js-object)))


(defun module-load-function (linkage)
  "Returns the function, as a JS expression, for loading the
   WebAssembly module given the linkage type LINKAGE."

  (js-member
   +tridash-namespace+

   (ecase linkage
     (local "load_file")
     (remote "load_url")
     (embed "load_bytes"))))

(defun module-path (linkage module-path wasm-module)
  "Returns an expression which returns the path to the WebAssembly
   module given the linkage type."

  (ecase linkage
    ((local remote) (js-string module-path))
    (embed (embed-wasm-module wasm-module))))

(defun module-runtime-path (linkage runtime-path)
  "Returns an expression which returns the path to the WebAssembly
   runtime library given the linkage type."

  (ecase linkage
    ((local remote) (js-string runtime-path))
    (embed (embed-file runtime-path))))

(defun parse-linkage (linkage)
  "Parse the WebAssembly module linkage type from the value of the
   linkage output option."

  (match linkage
    ((or (cl:equalp "local") (eq nil))
     'local)

    ((cl:equalp "remote") 'remote)

    ((cl:equalp "embed") 'embed)))

(defun embed-file (path)
  "Return a JS expression containing the bytes of the file at PATH."

  (embed-bytes (read-file-into-byte-vector path)))

(defun embed-wasm-module (module)
  "Return a JS expression containing the bytes of a serialized
   WebAssembly module."

  (embed-bytes
   (with-output-to-sequence (stream)
     (serialize-module module stream))))

(defun embed-bytes (bytes)
  "Generate a JS expression which returns a byte array containing
   BYTES."

  (js-new "Uint8Array" (list (js-array bytes))))

;;; Tridash Module

(defun make-module (compute init_state marshaller num_nodes)
  "Generate code which creates the Tridash Module and assigns it to
   the variable `module`."

  (js-var
   "module"

   (js-new
    +module-class+

    (list compute init_state marshaller num_nodes))))


;;; Imports

(defun add-meta-import (meta-node &optional (state *linker-state*))
  "Adds META-NODE, which should be an `external-meta-node', to the
   module's map of imported functions. The parameter signature is
   deduced from the operands and the return type signature is assumed
   to be (i32)."

  (check-type meta-node external-meta-node)

  (match (attribute :wasm-name meta-node)
    ((or (list (eq (id-symbol "."))
               (and (type (or string symbol)) module)
               (and (type (or string symbol)) name))

         (and (not nil) (type (or string symbol)) name))

     (let* ((module (if module (string module) "default"))
            (name (string name))
            (key (list module name)))

       (ensure-get key (imports state)
         (make-wasm-import
          :module module
          :name name
          :type :func
          :desc (list (repeat 'i32 (length (operands meta-node))) '(i32))))

       key))

    (_
     (error 'undefined-external-meta-node-error
            :backend "Wasm32"
            :meta-node meta-node))))


;;; Main State Computation Function

(defun create-compute-function (table)
  "Generate the main state computation function."

  (with-slots (nodes) table
    (with-slots (context-indices) *linker-state*

      (labels ((make-context-blocks ()
                 (let ((contexts
                        (-> (coerce context-indices 'alist)
                            (sort :key #'third))))

                   (reduce #'make-context-block
                           contexts

                           :from-end t
                           :initial-value
                           `((local.get $current)
                             (br_table
                              ,@(map (compose (curry #'symb '$c) #'third) contexts))))))

               (make-context-block (context prev)
                 "Generate code which updates the state of the node
                  using the function of CONTEXT and pushes its
                  observers to the dirtied queue."

                 (destructuring-bind (context node index) context
                   (multiple-value-bind (byte bit)
                       (floor (node-index node) 32)

                     `((block ,(symb '$c index)
                         ,@prev)

                       ;; Mark node as visited in visited node array.
                       (local.get (ref $visited))
                       (local.get (ref $visited))
                       (i32.load (offset ,(+ 8 byte)))
                       (i32.const ,(ash 1 bit))
                       i32.or
                       (i32.store (offset ,(+ 8 byte)))

                       ,@(add-dirtied node context)

                       ,@(unless (null (value-function context))
                           ;; Call Context Function
                           `((local.get (ref $state))
                             (local.get (ref $old-state))
                             (call (context ,node ,context))
                             (local.set $value)

                             ;; Store Value
                             (local.get (ref $state))
                             (local.get $value)
                             (i32.store (offset ,(+ 8 (* 4 (node-index node)))))))

                       (br $loop)))))

               (add-dirtied (node context)
                 "Generate instructions which add the contexts of the
                  observers of NODE, in the `NODE-CONTEXT' CONTEXT, to
                  the dirty queue."

                 (with-slots (operands) context
                   (when-let (observers (context-observers node context))
                     (map-extend #'dirty-context observers))))

               (context-observers (node context)
                 "Return the list of context indices of the observers
                  of NODE in the context CONTEXT."

                 (->> (observers node)
                      (remove-if
                       (lambda (observer)
                         (destructuring-bind (node . link) observer
                           (or
                            (and (node-link-two-way-p link)
                                 (memberp node (operands context)))

                            (node-link-weak-p link)))))

                      (map-to 'list #'observer-index)))

               (observer-index (observer)
                 "Return the context index of the observer entry
                  OBSERVER."

                 (destructuring-bind (node . link) observer
                   (->> link
                        node-link-context
                        (context node)
                        (context-index node))))

               (dirty-context (index)
                 "Generate instructions which add the context with
                  index INDEX to the dirty queue."

                 (multiple-value-bind (byte bit) (floor index 32)
                   (let ((byte (+ 8 byte))
                         (mask (ash 1 bit)))
                     `((block $check
                         (local.get (ref $dirtied))
                         (i32.load (offset ,byte))
                         (i32.const ,mask)
                         i32.and
                         (br_if $check)

                         ;; Mark context as dirtied
                         (local.get (ref $dirtied))
                         (local.get (ref $dirtied))
                         (i32.load (offset ,byte))
                         (i32.const ,mask)
                         i32.or
                         (i32.store (offset ,byte))

                         ;; Add to queue
                         (local.get $queue-top)
                         (i32.const ,index)
                         i32.store

                         ;; Bump queue top pointer
                         (local.get $queue-top)
                         (i32.const 4)
                         i32.sub
                         (local.set $queue-top))))))

               (make-array-set (var size)
                 `((i32.const ,(+ 8 (* +word-byte-size+ size)))
                   (call (import "runtime" "alloc"))
                   (local.tee (ref ,var))
                   (i32.const ,(+ 8 (* +word-byte-size+ size)))
                   (call (import "runtime" "memclear"))

                   ;; Set Type to Integer Array
                   (local.get (ref ,var))
                   (i32.const ,+type-int-array+)
                   i32.store

                   ;; Set Size
                   (local.get (ref ,var))
                   (i32.const ,size)
                   (i32.store (offset ,+word-byte-size+)))))

        ;; Add Context Indices
        (doseq (node nodes)
          (foreach (curry #'context-index node)
                   (map-values (contexts node))))

        (let ((*function-block-state*
               (make-instance 'function-block-state)))

          (create-compute-function-spec
           ;; Create Dirtied Context Set
           `(,@(make-array-set '$dirtied (ceiling (length context-indices) +word-size+))

             ;; Create Visited Node Array
             ,@(make-array-set '$visited (ceiling (length nodes) +word-size+))

             ;; Reserve space for dirty queue on value stack
             (global.get $value-stack-ptr)
             (local.tee $queue-base)

             ;; Get size of queue
             (local.get (ref $dirty-queue))
             (i32.load (offset 4))
             (i32.const ,+word-byte-size+)
             i32.mul
             (local.tee $queue-size)
             i32.sub
             (local.tee $queue-top)
             (i32.const 4)
             i32.add

             ;; Copy queue onto value stack
             (local.get (ref $dirty-queue))
             (i32.const 8)
             i32.add
             (local.get $queue-size)
             (call (import "runtime" "memcopy"))

             ;; Update Loop
             (block $out
               (loop $loop
                  ;; Check if dirty queue is empty
                  (local.get $queue-top)
                  (local.get $queue-base)
                  i32.ge_u

                  (br_if $out)

                  ;; Bump Queue Top Pointer
                  (local.get $queue-top)
                  (i32.const ,+word-byte-size+)
                  i32.add
                  (local.set $queue-top)

                  ;; Dequeue next dirty node context
                  (local.get $queue-top)
                  i32.load
                  (local.set $current)

                  ,@(make-context-blocks)))

             (local.get (ref $visited)))))))))

(defun create-compute-function-spec (instructions)
  "Create a WASM-FUNCTION-SPEC containing the instructions
   INSTRUCTIONS."

  (multiple-value-bind (locals code)
      (make-function-body
       instructions
       (alist-hash-map '(($dirty-queue . 0) ($old-state . 1) ($state . 2)))
       nil)

    (make-wasm-function-spec
     :params '(i32 i32 i32)
     :results '(i32)
     :locals locals
     :code code

     :export-name "compute")))

(defun context-index (node context)
  "Returns the index of the `NODE-CONTEXT' CONTEXT of NODE."

  (with-slots (context-indices) *linker-state*
    (second
     (ensure-get context context-indices
       (list node (length context-indices))))))


(defun create-init-state-function (num-nodes initial-values)
  "Generate the function which initializes the state of the nodes to
   their initial values, if any. If there are no initial values the
   function simply creates an empty state array."

  (flet ((add-to-queue (init index)
           (with-struct-slots initial-value- (context-index) init
             `((local.get (ref $dirtied))
               (i32.const ,context-index)
               (i32.store (offset ,(* (+ index 2) +word-byte-size+))))))

         (store-no-value (index)
           `((local.get (ref $old-state))
             (local.get (ref $no-value))
             (i32.store (offset ,(+ 8 (* 4 index)))))))

    (let ((array-size (+ 8 (* 4 num-nodes))))
      (multiple-value-bind (locals code)
          (make-function-body
           (concatenate
            (make-tridash-array '$old-state num-nodes)

            ;; Initial values of all nodes to failure of type No-Value
            `((call (import "runtime" "make_fail_no_value"))
              (box $no-value (type fail))
              ,@(map-extend-to 'list #'store-no-value (range 0 num-nodes)))

            ;; Create initial dirty queue
            (if (emptyp initial-values)
                '((local.get (ref $old-state)))

                `(,@(make-tridash-array '$dirtied (length initial-values))

                    ,@(-<> (sort initial-values :key #'initial-value-context-index)
                           (map-extend #'add-to-queue <> (range 0)))

                    ;; Copy Old State Array
                    (i32.const ,array-size)
                    (call (import "runtime" "alloc"))
                    (local.tee (ref $new-state))
                    (local.get (ref $old-state))
                    (i32.const ,array-size)
                    (call (import "runtime" "memcopy"))

                    (local.get (ref $dirtied))
                    (local.get (ref $old-state))
                    (local.get (ref $new-state))

                    (call $compute)
                    drop

                    (local.get (ref $new-state)))))

           (make-hash-map)
           nil)

        (make-wasm-function-spec
         :params nil
         :results '(i32)
         :locals locals
         :code code

         :export-name "init")))))


;;; Nodes

(defun create-nodes (nodes)
  "Generate the code for each node in NODES. Each element is a list of
   two values, the JavaScript node definitions and the WebAssembly
   functions."

  (flet ((make-node (node)
           (multiple-value-list
            (create-node node))))

    ;; Ensure each node has a unique index
    (foreach #'node-index nodes)

    (map-to 'list #'make-node nodes)))

(defgeneric create-node (node)
  (:documentation
   "Generate the definition for the `node' object NODE.

    Returns two values: the JavaScript code responsible for creating
    the runtime node object and the map of WebAssembly functions
    implementing each context's value function"))

(defmethod create-node (node)
  (let* ((context-functions
          (remove nil (map-to 'list (curry #'create-context node) (contexts node)))))

    (awhen (attribute :public-name node)
      (setf (get it (public-nodes *linker-state*))
            (make-node-export
             :type 'node
             :id (list
                  (node-index node)
                  (awhen (get :input (contexts node))
                    (context-index node it))))))

    ;; If the node has an INIT context, add its initial value to
    ;; INITIAl-VALUES of *LINKER-STATE* and ensure it has an input
    ;; context.

    (awhen (cdr (get-init-context node))
      (context node :input)

      (push
       (make-initial-value
        :node-index (node-index node)
        :context-index (context-index node it))

       (initial-values *linker-state*)))

    (values
     nil
     context-functions)))

(defun create-context (node context)
  "Generate the initialization code for a `node-context', returned in
   the first value, and its WebAssembly function, returned in the
   second value."

  ;; Generate a context function which takes a single argument, the
  ;; node state array.

  ;; The context function should always return a thunk which, then
  ;; extracts the values from the state vector and performs the
  ;; computation.

  (let* ((context (cdr context)))
    (with-slots (value-function operands) context
      (when value-function
        (cons
         (context-index node context)
         (make-context-function context))))))

(defun make-context-function (context)
  "Generate the WASM code for the value computation function of
   CONTEXT."

  (let ((operands (make-hash-map)))
    (labels ((get-operand (operand)
               (ematch operand
                 ((node-link- node)
                  (ensure-get operand operands
                    (make-get-operand node 'state)))

                 ((list :previous-value (node-ref node))
                  (ensure-get (list 'old node) operands
                    (make-get-operand node 'old-state)))))

             (make-get-operand (node state)
               (let ((state (get state (argument-locals *function-block-state*)))
                     (result (next-local)))

                 (make-value-block
                  :label result
                  :operands (list state)
                  :value-p t

                  :instructions
                  `((local.get (ref ,(value-block-label state)))
                    (i32.load (offset ,(+ 8 (* 4 (node-index node)))))
                    (local.set (ref ,result)))))))

      (with-slots (value-function operands) context
        (compile-function value-function '(state old-state)
                          :get-operand #'get-operand
                          :thunk t)))))

(defun get-init-context (node)
  "If NODE has an initial value context, that is a context with no
   operands and a constant value function, it is returned otherwise
   NIL is returned."

  (find-if #'init-context? (contexts node)))

(defun init-context? (context)
  "Returns true if the node context CONTEXT with identifier ID is an
   initial value context, that is a context with no operands and a
   constant value function."

  (destructuring-bind (id . context) context
    (and (not (eq id :input))
         (emptyp (operands context)))))


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
        (let ((fn (compile-function value-function operands)))
          (awhen (attribute :public-name meta-node)
            (setf (get it (public-nodes *linker-state*))
                  (make-node-export :type 'meta-node :id it))

            (setf (wasm-function-spec-export-name fn) it))

          (cons meta-node fn))))))


;;; Linking WebAssembly Code

(defun link-wasm-module (compute init-state nodes meta-nodes)
  "Link the compute function, node context functions and meta-node
   functions into a single web assembly module."

  (with-slots (imports) *linker-state*
    (multiple-value-bind (functions function-indices table)
        (link-functions
         (map-extend #'second nodes)
         meta-nodes

         compute
         init-state)

      (extract-runtime-imports functions imports)

      (let ((export-names (exported-functions functions)))
        (multiple-value-bind (function-types functions imports)
            (patch-function-types functions (map-values imports))

          (multiple-value-bind (functions index-offset)
              (patch-imports functions imports)

            (let* ((data-start (+ *stack-size* (* 4 2 (length nodes))))
                   (functions
                    (->
                     (patch-function-indices functions index-offset function-indices table)
                     (patch-data-references data-start)))

                   (exports (make-exports export-names function-indices index-offset))
                   (table-size (length table))
                   (table (make-table table index-offset))
                   (data (-> (data-section *backend-state*)
                             (make-object-descriptors data-start (object-descriptors *backend-state*))
                             (make-data-section data-start)))
                   (memory-size (+ data-start (length (data-section *backend-state*)))))

              (values
               (make-wasm-module
                :types function-types
                :imports (add-import-table-memory imports (+ 2 table-size) (ceiling memory-size (* 64 1024)))
                :functions functions
                :exports exports
                :elements (list table)
                :data (list data)

                :globals
                (list
                 (make-wasm-global
                  :type 'i32
                  :mutable-p t
                  :init `((i32.const ,(- data-start 4))))))

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
     :type :table
     :desc (make-wasm-limit :min table-size))

    (make-wasm-import
     :module "runtime"
     :name "memory"
     :type :memory
     :desc (make-wasm-limit :min mem-size)))

   imports))


(defun link-functions (context-fns meta-node-fns compute init-state)
  "Combine node context functions, meta-node functions, meta-node
   reference functions, thunk functions and COMPUTE function into a
   single list.

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
          (functions (make-array 0 :adjustable t :fill-pointer t)))

      (labels ((add-function-index (entity function)
                 (aprog1 (setf (get entity function-indices)
                            (length functions))

                   (add-export function it)))

               (add-export (function index)
                 (awhen (wasm-function-spec-export-name function)
                   (setf (get it function-indices) index)))

               (add-to-table (thing index)
                 (setf (get thing table)
                       (list (+ 2 (length table)) index))))

        (doseq ((context-index . fn) context-fns)
          (add-function-index (list 'context context-index) fn)
          (vector-push-extend fn functions))

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

        (vector-push-extend compute functions)
        (setf (get '$compute function-indices) (1- (length functions)))
        (add-export compute (1- (length functions)))

        (vector-push-extend init-state functions)
        (add-export init-state (1- (length functions)))

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
                 ((wasm-import- (type (eq :func))
                                (desc (list params results))
                                module
                                name)

                  (make-wasm-import
                   :module module
                   :name name
                   :type :func
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

           (remove-if-not (curry #'= :func) imports :key #'wasm-import-type)
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
       ((list 'call (list 'context node context))
        (let ((index (get (list 'context (context-index node context)) indices)))
          (check-type index (integer 0))
          (list 'call (+ offset index))))

       ((list 'call (list 'meta-node (and (type meta-node) meta-node)))
        (let ((index (get meta-node indices)))
          (check-type index (integer 0))
          (list 'call (+ offset index))))

       ((list 'call '$compute)
        (list 'call (+ offset (get '$compute indices))))

       ((list 'i32.const
              (and
               (or (list 'meta-node-ref (type meta-node))
                   (list 'thunk (type (integer 0))))
               thing))

        (let ((index (first (get thing table))))
          (check-type index (integer 0))
          (list 'i32.const index)))

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

         ((list 'global.get '$value-stack-ptr)
          '(global.get 0))

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
        :type :func
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

(defun make-object-descriptors (data-section data-start descriptors)
  "Initialize the buckets of the object descriptors, within the
   constant data section, with the correct key offsets."

  (flet ((make-bucket (bucket)
           (destructuring-bind (&optional key (value 0)) bucket
             (concatenate
              (byte-encode
               (if key (+ data-start key) 0))

              (byte-encode value)))))

    (doseq ((offset buckets) descriptors)
      (setf (subseq data-section (+ offset 8))
            (map-extend #'make-bucket buckets)))

    data-section))

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
