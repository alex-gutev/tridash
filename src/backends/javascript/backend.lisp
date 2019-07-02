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

(defvar *node-ids* nil
  "Hash-table mapping node-objects to their indices within the node
   table object.")

(defvar *node-link-indices* nil
  "Hash-table storing the dependency indices of each dependency of
   each node context. Each key is a `NODE-CONTEXT' object and the
   corresponding value is a hash-table mapping `NODE-LINK' objects (of
   the dependency node) to their dependency index.")

(defvar *meta-node-ids*
  "Hash-table mapping `META-NODE' objects to global meta-node function
   identifiers.")

(defvar *context-ids* nil
  "Hash-table containing the context identifiers of each context of
   each node. Each key is a `NODE' object and the corresponding value
   is a hash-table mapping context identifiers to their JS
   identifiers.")

(defvar *context-counter* 0
  "Counter for generating globally unique context identifiers")

(defvar *current-node* nil
  "The node whose definition code is currently being generated.")

(defvar *expression-groups* nil
  "Set of expression-group expressions for which code has already been
   generated. The mapped value is an expression (a variable name) with
   which the value of the expression can be accessed.")


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

(defvar *initial-values* nil
  "List of initial node values to set. Each element is a list where
   the first element is the node path and the second element is the
   initial value.")

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

    (let ((*node-ids* (make-hash-map))
          (*node-link-indices* (make-hash-map))
          (*meta-node-ids* (make-hash-map))
          (*context-ids* (make-hash-map))
          (*context-counter* 0)
          (*initial-values* nil)
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

  (with-hash-keys ((type "type") (main-ui "main-ui")) options
    (match type
      ((cl:equalp "html")
       (->>
        (get-root-node main-ui)
        (create-html-file
         (lexical-block (list code (make-html-set-initial-values *initial-values*))))))

      (_
       (-<> (make-set-initial-values *initial-values*)
            (list code <>)
            (lexical-block)
            (output-code))))))

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
     (let ((*expression-groups* (make-hash-map)))
       (list (make-set-initial-values initial-values))))))

(defun make-set-initial-values (initial-values)
  "Generates code which sets the initial values of the nodes and
   dispatches those values to their observer nodes."

  (when initial-values
    (iter
      (for (node init-value) in initial-values)

      (multiple-value-bind (block expression)
          (make-expression init-value :return-variable (var-name))

        (when block (collect block into blocks))
        (collect (js-array (list node expression)) into expressions))

      (finally
       (return
         (list
          blocks
          (js-call (js-member +tridash-namespace+ "set_values")
                   (js-array expressions))))))))


;;; Context Identifiers

(defun global-context-id ()
  "Returns a new unique global context identifier."

  (prog1 *context-counter*
    (incf *context-counter*)))

(defun context-js-id (node context-id)
  "Returns the JavaScript context identifier for the context with
   identifier CONTEXT-ID of NODE."

  (let ((ids (ensure-get node *context-ids* (make-hash-map))))
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
      (create-meta-nodes meta-nodes))

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

    (awhen (attribute :public-name node)
      (append-code
       (-<> (js-member +tridash-namespace+ "nodes")
            (js-element (js-string it))
            (js-call "=" <> path))))

    ;; If the node has an INIT context, add its initial value to
    ;; *INITIAL-VALUES* and ensure it has an input context.

    (awhen (cdr (get-init-context node))
      (context node :input)
      (push (list path (value-function it)) *initial-values*))

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

        (append-code
         (lexical-block
          (js-var "context" (make-context-expression node-path id context))

          (awhen (create-compute-function context)
            (js-call "=" (js-member "context" "compute") it))

          (js-call "=" context-path "context")))))))


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
   CONTEXT, and adds them to *NODE-LINK-INDICES*."

  (foreach (curry #'dependency-index context) (map-keys (operands context))))

(defun dependency-index (context operand)
  "Returns the index of the operand (of CONTEXT). If OPERAND does not have an index,
   a new index is assigned to it."

  (let ((operands (ensure-get context *node-link-indices* (make-hash-map))))
    (ensure-get operand operands (length operands))))


;;;; Creating meta-nodes

(defun create-meta-nodes (meta-nodes)
  "Generates the meta-node functions of each `META-NODE' in META-NODES."

  (foreach (compose #'append-code #'create-meta-node) meta-nodes))

(defun create-meta-node (meta-node)
  "Generates the meta-node function of META-NODE."

  (let ((*current-node* meta-node))
    (when (not (external-meta-node? meta-node))
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

  (let* ((context-path (context-path node context-id)))

    (doseq ((observer . link) (observers node))
      (unless (get observer (operands context))
        (with-accessors ((link-context node-link-context)) link
          (append-code
           (js-call
            (js-member context-path "add_observer")
            (context-path observer link-context)
            (dependency-index (context observer link-context) node))))))))
