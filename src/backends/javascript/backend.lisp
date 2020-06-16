;;;; backend.lisp
;;;;
;;;; Tridash Programming Language.
;;;; Copyright (C) 2018-2020  Alexander Gutev
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

;;;; JavaScript Backend

(in-package :tridash.backend.js)


;;; Backend State

(defclass js-backend-state ()
  ((node-ids
    :initform (make-hash-map)
    :accessor node-ids
    :documentation
    "Map from `NODE' objects to their indices within the node table
     object.")

   (meta-node-ids
    :initform (make-hash-map)
    :accessor meta-node-ids
    :documentation
    "Map from `META-NODE' objects to global meta-node function
     identifiers.")

   (type-node-ids
    :initform (make-hash-map)
    :accessor type-node-ids
    :documentation
    "Map from `NODE' objects, serving as Tridash types, to their
     indices within the Tridash type node table.")

   (public-nodes
    :initform (make-hash-map)
    :accessor public-nodes
    :documentation
    "Map from public node identifiers to the corresponding node index
     or meta-node identifier.")

   (context-indices
    :initform (make-hash-map)
    :accessor context-indices
    :documentation
    "Map from `NODE-CONTEXT' objects to the corresponding context
     indices. Each value is a list of the form (NODE INDEX) where NODE
     is the node to which the context belongs and INDEX is the
     context's index.")

   (initial-values
    :initform nil
    :accessor initial-values
    :documentation
    "List of initial node values to set, where each element is an
     INITIAL-VALUE object.")))

(defstruct initial-value
  "Node Initial Value.

   NODE-INDEX - Index of the node.
   CONTEXT-INDEX - Index of the context.
   EXPRESSION - Initial value expression"

  node-index
  context-index
  expression)

(defvar *backend-state* nil
  "Special variable storing the `JS-BACKEND-STATE' object.")


;;; Code Generation Flags

(defconstant +runtime-path-var+ "TRIDASH_JS_RUNTIME"
  "Environment variable containing the path to the JavaScript Runtime
   Library.")

(defvar *debug-info-p* nil
  "Flag: If true debug information, such as the names of the nodes is
   included in the generated code.")

(defvar *runtime-library-path* "/usr/local/share/tridash/backends/javascript/tridash.min.js"
  "Path to the runtime library.")

(defvar *runtime-link-type* 'static
  "The type of linkage which should be used to link the runtime
   library. Can be one of the following symbols:

   STATIC - The runtime library is directly inserted in the generated code.

   DYNAMIC - A link to the runtime library, at the value of
   *RUNTIME-LIBRARY-PATH*, is inserted via a script tag.

   NIL - No linkage.")


;;;; Compilation

(defmethod compile-nodes ((backend (eql :javascript)) table out-file &optional (options (make-hash-map :test #'cl:equalp)))
  "Compile the node and meta-node definitions, in the `NODE-TABLE'
   TABLE, to JavaScript."

  (let ((*print-indented* (parse-boolean (get "indented" options)))
        (*debug-info-p* (parse-boolean (get "debug-info" options)))
        (*runtime-library-path* (runtime-path options))
        (*runtime-link-type* (parse-linkage-type (get "runtime-linkage" options)))

        (*backend-state* (make-instance 'js-backend-state)))

    (with-slots (initial-values public-nodes type-node-ids) *backend-state*
      (let ((compute (create-compute-function table))
            (code (generate-code table)))

        (print-output-code
         out-file

         (list
          (make-type-nodes type-node-ids)
          (make-module compute)
          code
          (make-exports public-nodes))

         options)))))

(defun runtime-path (options)
  "Returns the path to the runtime library. First OPTIONS is checked
   for whether it contains a runtime-path key. If not the runtime path
   environment variable is checked whether it is set. Finally if
   neither OPTIONS contains the path nor the environment variable is
   set, the current value of *RUNTIME-LIBRARY-PATH* is returned."

  (or (get "runtime-path" options)
      (uiop:getenvp +runtime-path-var+)
      *runtime-library-path*))


(defun parse-boolean (thing)
  "Converts THING to a boolean value."

  (match thing
    ((cl:equalp "true")
     (values t t))

    ((cl:equalp "false")
     (values nil t))

    ((ppcre "^[0-9]+$")
     (parse-boolean (parse-integer thing)))

    ((type integer)
     (values
      (not (zerop thing))
      t))

    (_ (values nil nil))))

(defun parse-linkage-type (linkage)
  "Parses the runtime library linkage type from the string LINKAGE."

  (match linkage
    ((or (cl:equalp "static") (eql nil))
     'static)

    ((cl:equalp "dynamic")
     'dynamic)

    ((cl:equalp "none")
     nil)))

(defun print-output-code (out-file code options)
  "Prints the JavaScript code represented by the AST nodes in CODE to
   *STANDARD-OUTPUT*."

  (with-open-file (*standard-output* out-file :direction :output :if-exists :supersede)
    (with-slots (initial-values) *backend-state*
      (with-hash-keys
          ((type "type")
           (module-name "module-name")
           (main-ui "main-ui"))
          options

        (match type
          ((cl:equalp "html")

           (let ((code (list code (make-html-set-initial-values initial-values))))
             (->>
              (get-root-node main-ui)
              (create-html-file

               (if module-name
                   (make-js-module code module-name)
                   (lexical-block
                    (js-var "exports" (js-object))
                    code))))))

          (_
           (-<> (make-set-initial-values initial-values)
                (list code <>)
                (wrap-module module-name)
                (output-code))))))))

(defun get-root-node (node)
  "Gets the root HTML node specified by NODE."

  (multiple-value-bind (module node) (node-path->name node)
    (->> (get-module module *global-module-table*)
         (tridash.frontend::lookup-node node)
         (element-node))))


(defun wrap-module (code module-name)
  "If MODULE-NAME is non-NIL wrap CODE in an anonymous function with
   the result assigned to the variable MODULE-NAME."

  (if module-name
      (make-js-module code module-name)
      code))

(defun make-js-module (code module-name)
  "Wrap CODE in an anonymous function with the result assigned to a
   variable with identifier MODULE-NAME."

  (list
   (js-var module-name (js-object))
   (js-call
    (js-lambda '("exports") code)
    (list module-name))))



;;; Creating Tridash Module

(defun make-module (compute)
  "Generate code which creates the Tridash Module and assigns it to
   the variable `module`."

  (js-var
   "module"

   (js-new
    +module-class+

    (list compute))))

(defun make-exports (public-nodes)
  "Generate code which creates the object storing the exported
   nodes (in PUBLIC-NODES). The object is assigned to
   `exports.nodes`."

  (list
   (js-call
    "="
    (js-member "exports" "nodes")

    (js-object
     (map-to
      'list

      (lambda (export)
        (destructuring-bind (name node-index context-index) export
          (list
           (js-string name)
           (js-new +node-interface-class+
                   (list* "module" node-index (ensure-list context-index))))))

      public-nodes)))

   (js-call
    "="
    (js-member "exports" "set_values")

    (js-call
     (js-members "module" "set_node_values" "bind")
     "module"))))

(defun make-type-nodes (type-nodes)
  "Generate code which creates the NodeRef objects for the nodes, for
   which there is a raw node reference."

  (flet ((make-type-node (node)
           (destructuring-bind (node . id) node
             (js-var
              id

              (js-new +node-ref-class+
                      (list (node-index node)))))))

    (map-to 'list #'make-type-node type-nodes)))


;;; Initialize nodes to initial values

(defun make-html-set-initial-values (initial-values)
  "Generates the setting of initial node values code for HTML files."

  (when initial-values
    (make-onloaded-method
     (list (make-set-initial-values initial-values)))))

(defun make-set-initial-values (initial-values)
  "Generates code which sets the initial state of the module."

  (when initial-values
    (list
     (js-call
      (js-members "module" "set_values")

      (js-array
       (map
        (lambda (init)
          (js-array
           (list (initial-value-context-index init)
                 (initial-value-node-index init)
                 (initial-value-expression init))))

        initial-values))))))



;;; Generate the main state computation function

(defun create-compute-function (table)
  "Generate the main state computation function of the program."

  (with-slots (nodes) table
    (with-slots (context-indices) *backend-state*

      (labels ((make-context-case (context)
                 "Generate the switch statement case for CONTEXT."

                 (destructuring-bind (context node index) context
                   (with-slots (operands) context
                     (list
                      index

                      (list
                       (js-call
                        "="
                        (js-element "changed" (node-index node))
                        "true")

                       (when-let
                           ((observers
                             (->> (observers node)
                                  (remove-if
                                   (lambda (observer)
                                     (destructuring-bind (node . link) observer
                                       (or
                                        (and (node-link-two-way-p link)
                                             (memberp node operands))

                                        (node-link-weak-p link)))))

                                  (map-to 'list #'observer-index))))

                         (make-js-call (js-member "dirtied" "push") observers))

                       (unless (emptyp operands)
                         (js-call
                          "="

                          (node-value-var node)
                          (make-set-node-value context)))

                       (js-break))))))

               (observer-index (observer)
                 (destructuring-bind (node . link) observer
                   (->> link
                        node-link-context
                        (context node)
                        (context-index node))))

               (make-node-var (node)
                 "Generate a variable state for the variable storing
                  the value of NODE."

                 (js-var
                  (node-value-var node)
                  (js-element "state" (node-index node))))

               (node-value (node)
                 "Create an entry for NODE to be placed in the state
                  object."

                 (list (node-index node) (node-value-var node))))

        ;; Add Context Indices
        (doseq (node nodes)
          (foreach (curry #'context-index node)
                   (map-values (contexts node))))

        (js-lambda
         (list "dirtied" "state")

         (list
          (map-to 'list #'make-node-var nodes)

          (js-var "visited" (js-object))
          (js-call "=" "dirtied" (js-call (js-member "dirtied" "slice")))

          (js-var "changed" (js-object))

          (js-while
           (js-member "dirtied" "length")

           (js-block
            (js-var "node" (js-call (js-member "dirtied" "pop")))
            (js-if
             (js-call "!" (js-element "visited" "node"))

             (js-block
              (js-call "=" (js-element "visited" "node") "true")
              (js-switch
               "node"

               (-<> (coerce context-indices 'alist)
                    (sort :key #'third)
                    (map #'make-context-case <>)))))))

          (js-return
           (js-object
            (list
             (list "changed" "changed")
             (list "state" (js-object (map-to 'list #'node-value nodes))))))))))))

(defun context-index (node context)
  "Returns the index of the `NODE-CONTEXT' CONTEXT of NODE."

  (with-slots (context-indices) *backend-state*
    (second
     (ensure-get context context-indices
       (list node (length context-indices))))))

(defun make-set-node-value (context)
  "Generate a thunk function which computes the new value of a node
   using the function of CONTEXT."

  (let ((operands (make-hash-map)))
    (flet ((get-operand (operand)
             (ematch operand
               ((node-link node)
                (ensure-get node operands
                  (make-value-block
                   :expression (node-value-var node))))

               ((list :previous-value (and (node-ref node) ref))
                (ensure-get ref operands
                  (make-value-block
                   :expression (js-element "state" (node-index node))))))))

      (with-slots (value-function) context
        (thunk
         (compile-function value-function #'get-operand :protect nil))))))

(defun node-value-var (node)
  "Return the name of the variable in which the value of NODE is
   stored. This variable is only available inside the state
   computation function."

  (mkstr "node" (node-index node)))


;;;; Code Generation

(defun generate-code (table)
  "Generates the JavaScript code for the node and meta-node
   definitions in the `NODE-TABLE' TABLE."

  (with-slots (nodes meta-nodes) table
    (concatenate
     (create-nodes nodes)
     (create-meta-nodes meta-nodes))))


;;; Creating nodes

(defun node-index (node)
  "Returns the index of NODE within the program state"

  (with-slots (node-ids) *backend-state*
   (ensure-get node node-ids (length node-ids))))

(defun create-nodes (nodes)
  "Generate the node creation code of each `NODE' in NODES."

  (map-extend-to 'list #'create-node nodes))


(defgeneric create-node (node)
  (:documentation
   "Generate the node creation code of NODE."))

(defmethod create-node (node)
  "Generate the node creation code, which creates the dependency
   queues and the value computation function."

  (awhen (attribute :public-name node)
    (setf (get it (public-nodes *backend-state*))
          (list
           (node-index node)
           (awhen (get :input (contexts node))
             (context-index node it)))))

  ;; If the node has an INIT context, add its initial value to
  ;; INITIAl-VALUES of *BACKEND-STATE* and ensure it has an input
  ;; context.

  (awhen (cdr (get-init-context node))
    (context node :input)

    (push
     (make-initial-value
      :node-index (node-index node)
      :context-index (context-index node it)
      :expression
      (multiple-value-bind (block expression)
          (compile-function (value-function it) nil :protect nil :return-value nil)

        (if (and (null block) (non-computing? expression))
            expression
            (thunk (list block (js-return expression))))))

     (initial-values *backend-state*)))

  nil)

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


;;; Creating meta-nodes

(defun create-meta-nodes (meta-nodes)
  "Generates the meta-node functions of each `META-NODE' in META-NODES."

  (labels ((union-meta-nodes (set meta-nodes)
             "Adds all meta-nodes, and the meta-nodes nested in their
              definitions, to SET."

             (foreach (curry #'union-meta-node set) meta-nodes))

           (union-meta-node (set meta-node)
             "Adds META-NODE, and the meta-nodes nested in its
              definition, to SET."

             (unless (memberp meta-node set)
               (nadjoin meta-node set)

               (unless (external-meta-node? meta-node)
                 (->> meta-node
                      definition
                      meta-nodes
                      (union-meta-nodes set))))))

    (let ((all-meta-nodes (make-hash-set)))
      (union-meta-nodes all-meta-nodes meta-nodes)
      (map-extend-to 'list #'create-meta-node all-meta-nodes))))

(defgeneric create-meta-node (meta-node)
  (:documentation
   "Generates the meta-node function of META-NODE.")

  (:method ((meta-node external-meta-node))
    nil)

  (:method ((meta-node final-meta-node))
    (create-function-meta-node meta-node)))
