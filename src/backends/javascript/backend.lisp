;;;; backend.lisp
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

;;;; JavaScript Backend

(in-package :tridash.backend.js)


;;;; Backend State

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

   (node-link-indices
    :initform (make-hash-map)
    :accessor node-link-indices
    :documentation
    "Map storing the dependency indices of each dependency of each
     node context. Each key is a `NODE-CONTEXT' object and the
     corresponding value is a map from `NODE-LINK' objects (of the
     dependency node) to their dependency indices.")

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

   (initial-values
    :initform nil
    :accessor initial-values
    :documentation
    "List of initial node values to set. Each element is a list where
     the first element is the node path and the second element is the
     initial value.")))

(defvar *backend-state* nil
  "Special variable storing the `JS-BACKEND-STATE' object.")

(defvar *current-node* nil
  "The node whose definition code is currently being generated.")

(defparameter *node-path* 'access-node
  "Function which takes a node as an argument and returns an
   expression which references that node.")


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


;;;; Code Array

(defvar *output-code* nil
  "Array into which the output JavaScript AST nodes are added.")

(defun make-code-array ()
  "Creates an empty array suitable for pushing JavaScript AST nodes to
   it."

  (make-array 0 :adjustable t :fill-pointer t))

(defun append-code (&rest asts)
  "Appends each ast node in ASTS to the *OUTPUT-CODE* array."

  (foreach (rcurry #'vector-push-extend *output-code*) asts))


;;;; Compilation

(defmethod compile-nodes ((backend (eql :javascript)) table &optional (options (make-hash-map :test #'cl:equalp)))
  "Compile the node and meta-node definitions, in the `NODE-TABLE'
   TABLE, to JavaScript."

  (let ((*print-indented* (parse-boolean (get "indented" options)))
        (*debug-info-p* (parse-boolean (get "debug-info" options)))
        (*runtime-library-path* (runtime-path options))
        (*runtime-link-type* (parse-linkage-type (get "runtime-linkage" options))))

    (let ((*backend-state* (make-instance 'js-backend-state))
          (defs (make-code-array))
          (bindings (make-code-array)))

      (generate-code table defs bindings)
      (print-output-code (list defs bindings) options))))

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

(defun print-output-code (code options)
  "Prints the JavaScript code represented by the AST nodes in CODE to
   *STANDARD-OUTPUT*."

  (with-slots (initial-values) *backend-state*
    (with-hash-keys ((type "type") (main-ui "main-ui")) options
      (match type
        ((cl:equalp "html")
         (->>
          (get-root-node main-ui)
          (create-html-file
           (lexical-block (list code (make-html-set-initial-values initial-values))))))

        (_
         (-<> (make-set-initial-values initial-values)
              (list code <>)
              (lexical-block)
              (output-code)))))))

(defun get-root-node (node)
  "Gets the root HTML node specified by NODE."

  (multiple-value-bind (module node) (node-path->name node)
    (->> (get-module module *global-module-table*)
         (tridash.frontend::lookup-node node)
         (element-node))))


;; Initialize nodes to initial values

(defun make-html-set-initial-values (initial-values)
  "Generates the setting of initial node values code for HTML files."

  (when initial-values
    (make-onloaded-method
     (list (make-set-initial-values initial-values)))))

(defun make-set-initial-values (initial-values)
  "Generates code which sets the initial values of the nodes and
   dispatches those values to their observer nodes."

  (when initial-values
    (let ((state (make-instance 'function-block-state)))
      (iter
        (for (node init-value) in initial-values)

        (multiple-value-bind (block expression)
            (compile-function init-value nil :return-value nil :state state)

          (when block (collect block into blocks))
          (collect (js-array (list node expression)) into expressions))

        (finally
         (return
           (list
            blocks
            (js-call (js-member +tridash-namespace+ "set_values")
                     (js-array expressions)))))))))


;;; Access Node Expressions

(defun access-node (node)
  "Returns an expression which references NODE."

  (mkstr "node" (node-index node)))

(defun node-index (node)
  "Returns the index of NODE within the node table variable."

  (with-slots (node-ids) *backend-state*
   (ensure-get node node-ids (length node-ids))))

(defun node-path (node)
  "Returns an expression which references NODE, by calling the
   function bound to *NODE-PATH*."

  (funcall *node-path* node))


;;; Context Identifiers

(defun global-context-id ()
  "Returns a new unique global context identifier."

  (with-slots (context-counter) *backend-state*
    (prog1 context-counter
      (incf context-counter))))

(defun context-js-id (node context-id)
  "Returns the JavaScript context identifier for the context with
   identifier CONTEXT-ID of NODE."

  (let ((ids (ensure-get node (context-ids *backend-state*) (make-hash-map))))
    (case context-id
      (:input
       (js-string "input"))

      (otherwise
       (ensure-get context-id ids (length ids))))))

(defun context-path (node context-id)
  "Returns a JS expression which references the context, with
   identifier CONTEXT-ID, of NODE."

  (js-element (js-member (node-path node) "contexts")
              (context-js-id node context-id)))


;;;; Code Generation

(defun generate-code (table &optional (defs *output-code*) (bindings *output-code*))
  "Generates the JavaScript code for the node and meta-node
   definitions in the `NODE-TABLE' TABLE. DEFS is the code array into
   which the node definition code is appended. BINDINGS is the code
   array into which the node binding code is appended."

  (with-slots (nodes meta-nodes) table
    (let ((*output-code* defs))
      (create-nodes nodes)
      (create-meta-nodes meta-nodes)
      (create-type-nodes *backend-state*))

    (let ((*output-code* bindings))
      (init-nodes nodes))))


;;;; Creating nodes

(defun create-nodes (nodes)
  "Generate the node creation code of each `NODE' in NODES."

  (foreach #'create-node nodes))


(defgeneric create-node (node)
  (:documentation
   "Generate the node creation code of NODE. This includes the
    creation of the node, its dependency queues and its value
    computation function, however it does not include the binding of
    the node to its observers."))

(defmethod create-node (node)
  "Generate the node creation code, which creates the dependency
   queues and the value computation function."

  (let ((*current-node* node)
        (path (node-path node)))

    (append-code
     (if (typep path '(or string symbol))
         (js-var path (js-new +node-class+))
         (js-call "=" path (js-new +node-class+))))

    (when *debug-info-p*
      (append-code
       (js-call "=" (js-member path "name") (js-string (name node)))))

    (append-code
     (store-in-public-nodes node path))

    ;; If the node has an INIT context, add its initial value to
    ;; INITIAl-VALUES of *BACKEND-STATE* and ensure it has an input
    ;; context.

    (awhen (cdr (get-init-context node))
      (context node :input)
      (push (list path (value-function it)) (initial-values *backend-state*)))

    (foreach (rcurry #'create-context node) (contexts node))))

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

(defun create-context (context node)
  "Generates the initialization code for a `NODE-CONTEXT'. ID is the
   context identifier, CONTEXT is the `NODE-CONTEXT' itself and NODE
   is the `NODE' to which the context belongs."

  (unless (init-context? context)
    (destructuring-bind (id . context) context
      (establish-dependency-indices context)

      (let ((node-path (node-path node))
            (context-path (context-path node id)))

        (multiple-value-bind (function previous-values)
            (create-compute-function context)

          (append-code
           (lexical-block
            (js-var "context" (make-context-expression node-path id context))

            (when function
              (js-call "=" (js-member "context" "compute") function))

            (make-save-previous-node-values node context "context" previous-values)

            (js-call "=" context-path "context"))))))))

(defun make-save-previous-node-values (node context var previous-values)
  "Generates code which overrides the `reserve_hook' method of a
   context to store the values of nodes, of which the previous values
   are referenced by CONTEXT's value function, in the
   'previous_values' array.

   NODE is the node to which CONTEXT belongs. VAR is the variable with
   which the context is referenced. PREVIOUS-VALUES is the map of
   nodes of which the previous-values are referenced."

  (labels ((save-node-value (operand)
             (destructuring-bind (operand . index) operand

               (if (= operand node)
                   (save-self index)

                   (js-if
                    (js-call "==" "index" (dependency-index context operand))
                    (js-call
                     (js-member "reserve" "add_precondition")
                     (js-call
                      (js-member "path" "then")
                      (js-lambda
                       nil

                       (list
                        (js-call
                         "="

                         (js-element
                          (js-member var "previous_values")
                          index)

                         (js-member (node-path operand) "value"))))))))))

           (save-self (index)
             (js-if "first"
                    (js-call "="
                             (js-element (js-member var "previous_values") index)
                             (js-member (node-path node) "value")))))

    (unless (emptyp previous-values)
      (list
       (js-call "=" (js-member var "previous_values") (js-array))

       (js-call
        "="

        (js-member var "reserve_hook")
        (js-lambda
         (list "reserve" "index" "path" "first")

         (map-to 'list #'save-node-value previous-values)))))))


(defun make-context-expression (node-path id context)
  "Creates an expression which creates a new context for the context
   CONTEXT with id ID of the node which is referenced by the
   expression NODE-PATH."

  (case id
    (:input
     (js-call (js-member +node-context-class+ "create_input")
              node-path
              (global-context-id)))

    (otherwise
     (js-call (js-member +node-context-class+ "create")
              node-path
              (length (operands context))
              (global-context-id)))))

(defun establish-dependency-indices (context)
  "Establishes the indices of the operands of the `NODE-CONTEXT'
   CONTEXT, and adds them to the node link indices map of
   *BACKEND-STATE*."

  (foreach (curry #'dependency-index context) (map-keys (operands context))))

(defun dependency-index (context operand)
  "Returns the index of the operand (of CONTEXT). If OPERAND does not have an index,
   a new index is assigned to it."

  (let ((operands (ensure-get context (node-link-indices *backend-state*) (make-hash-map))))
    (ensure-get operand operands (length operands))))


;;;; Creating meta-nodes

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
      (foreach (compose #'append-code #'create-meta-node) all-meta-nodes))))

(defgeneric create-meta-node (meta-node)
  (:documentation
   "Generates the meta-node function of META-NODE.")

  (:method ((meta-node external-meta-node))
    nil)

  (:method ((meta-node final-meta-node))
    (let ((*current-node* meta-node))
      (create-function-meta-node meta-node))))


;;;; Generate dispatch methods

(defun init-nodes (nodes)
  "Generates the initialization code of each `NODE' in NODES."

  (foreach #'init-node nodes))

(defun init-node (node)
  "Generates the initialization code of NODE. This includes the
   binding of the node to its observers."

  (foreach (rcurry #'init-context node) (contexts node)))

(defun init-context (context node)
  "Generates the initialization code of the node context CONTEXT, with
   identifier CONTEXT-ID, of the node NODE."

  (unless (init-context? context)
    (bind-observers node (car context) (cdr context))))

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
          (foreach #'append-code (map #'add-observer observers))

          (append-code
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
                 (map #'make-reserve observers)))))))))))))
