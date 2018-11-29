;;;; backend.lisp
;;;;
;;;; Tridash Programming Language.
;;;; Copyright (C) 2018  Alexander Gutev
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


;;;; Symbol Names

(defconstant +tridash-prefix+ "Tridash."
  "Namespace containing the Tridash runtime library definitions.")

(defconstant +node-class+ (mkstr +tridash-prefix+ "Node")
  "Runtime node class name.")

(defconstant +node-context-class+ (mkstr +tridash-prefix+ "NodeContext")
  "Runtime node context class name.")

(defvar *node-table-var* "node_table"
  "Global node table variable.")


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

(defvar *meta-node-types* (make-hash-table :test #'eq)
  "Hash-table mapping `META-NODE' objects to the the symbols SYNC or
   ASYNC indicating whether the meta-node is evaluated synchronously
   or asynchronously.")

(defvar *lazy-nodes* nil
  "Hash-table of nodes which should be evaluated lazily. If a node
   should be evaluated lazily it is present in the table with the
   corresponding value T.")

(defvar *context-ids* nil
  "Hash-table containing the context identifiers of each context of
   each node. Each key is a `NODE' object and the corresponding value
   is a hash-table mapping context identifiers to their JS
   identifiers.")

(defvar *context-counter* 0
  "Counter for generating globally unique context identifiers")


;;; Code Generation Flags

(defvar *debug-info-p* nil
  "Flag: If true debug information, such as the names of the nodes is
   included in the generated code.")


;;;; Code Array

(defvar *output-code* nil
  "Array into which the output JavaScript AST nodes are added.")

(defun make-code-array ()
  "Creates an empty array suitable for pushing JavaScript AST nodes to
   it."
  (make-array 0 :adjustable t :fill-pointer t))

(defun append-code (&rest asts)
  "Appends each ast node in ASTS to the *OUTPUT-CODE* array."

  (mapc (rcurry #'vector-push-extend *output-code*) asts))


;;;; Compilation

(defmethod compile-nodes ((backend (eql :javascript)) table &rest options &key &allow-other-keys)
  "Compile the node and meta-node definitions, in the `NODE-TABLE'
   TABLE, to JavaScript."

  (let ((*node-ids* (make-hash-table :test #'eq))
        (*node-link-indices* (make-hash-table :test #'eq))
        (*meta-node-ids* (make-hash-table :test #'eq))
        (*context-ids* (make-hash-table :test #'eq))
        (*lazy-nodes* (find-lazy-nodes table))
        (*context-counter* 0)
        (*output-code* (make-code-array)))

    (make-preamble)

    (maphash-values #'generate-code (modules table))
    (print-output-code (make-lexical-block *output-code*) options table)))

(defun print-output-code (code options module-table)
  "Prints the JavaScript code represented by the AST nodes in CODE to
   *STANDARD-OUTPUT*."

  (destructuring-bind (&key (type :code) main-ui) options
    (case type
      (:html
       (->>
        (get-root-node main-ui module-table)
        (create-html-file code)))

      (:code
       (output-code code)))))

(defun get-root-node (node module-table)
  "Gets the root HTML node specified by NODE."

  (destructuring-bind (module node) node
    (->> (get-module module module-table)
         (tridash.frontend::lookup-node node)
         (element-node))))


;;; Context Identifiers

(defun global-context-id ()
  "Returns a new unique global context identifier."

  (prog1 *context-counter*
    (incf *context-counter*)))

(defun context-js-id (node context-id)
  "Returns the JavaScript context identifier for the context with
   identifier CONTEXT-ID of NODE."

  (let ((ids (ensure-gethash node *context-ids* (make-hash-table :test #'equal))))
    (case context-id
      (:input
       (js-string "input"))

      (otherwise
       (ensure-gethash context-id ids (hash-table-count ids))))))

(defun context-path (node context-id)
  "Returns a JS expression which references the context, with
   identifier CONTEXT-ID, of NODE."

  (js-element (js-member (node-path node) "contexts")
              (context-js-id node context-id)))


;;; Querying for Lazy Evaluation

(defun lazy-node? (context)
  "Returns true if the CONTEXT should be evaluated lazily. CONTEXT
   should be evaluated lazily if it is present in *LAZY-NODES* (with
   value T) and has a non-NIL value function."

  (and (gethash context *lazy-nodes*)
       (value-function context)))


;;;; Code Generation

(defun generate-code (table)
  "Generates the JavaScript code for the node and meta-node
   definitions in the `NODE-TABLE' TABLE."

  (with-slots (nodes meta-nodes) table
    (create-nodes nodes)
    (create-meta-nodes meta-nodes)

    (init-nodes nodes)))

(defun make-preamble ()
  "Creates the code which should appear before any node
   definitions. Currently this contains only the declaration of the
   node table variable."

  (append-code
   (js-var *node-table-var* (js-object))))


;;;; Creating nodes

(defun create-nodes (nodes)
  "Generate the node creation code of each `NODE' in NODES."

  (maphash-values #'create-node nodes))

(defgeneric create-node (node)
  (:documentation
   "Generate the node creation code of NODE. This includes the
    creation of the node, its dependency queues and its value
    computation function, however it does not include the binding of
    the node to its observers."))

(defmethod create-node (node)
  "Generate the node creation code, which creates the dependency
   queues and the value computation function."

  (let ((path (node-path node)))
    (append-code
     (js-call '= path (js-new +node-class+)))

    (when *debug-info-p*
      (append-code
       (js-call '= (js-member path "name") (js-string (name node)))))

    (maphash (rcurry #'create-context node) (contexts node))))


(defun create-context (id context node)
  "Generates the initialization code for a `NODE-CONTEXT'. ID is the
   context identifier, CONTEXT is the `NODE-CONTEXT' itself and NODE
   is the `NODE' to which the context belongs."

  (establish-dependency-indices context)

  (let ((node-path (node-path node))
        (context-path (context-path node id)))

    (with-slots (operands) context
      (append-code
       (lexical-block
        (js-var "context"
                (js-call (js-member +node-context-class+ "create")
                         node-path (hash-table-count operands) (global-context-id)))

        (awhen (create-compute-function context)
          (js-call '= (js-member "context" "compute") it))

        (js-call '= context-path "context"))))))


(defun establish-dependency-indices (context)
  "Establishes the indices of the operands of the `NODE-CONTEXT'
   CONTEXT, and adds them to *NODE-LINK-INDICES*."

  (maphash-keys (curry #'dependency-index context) (operands context)))

(defun dependency-index (context operand)
  "Returns the index of the operand (of CONTEXT). If OPERAND does not have an index,
   a new index is assigned to it."

  (let ((operands (ensure-gethash context *node-link-indices* (make-hash-table :test #'eq))))
    (ensure-gethash operand operands (hash-table-count operands))))


;;;; Creating meta-nodes

(defun create-meta-nodes (meta-nodes)
  "Generates the meta-node functions of each `META-NODE' in META-NODES."

  (maphash-values (compose #'append-code #'create-meta-node) meta-nodes))

(defun create-meta-node (meta-node)
  "Generates the meta-node function of META-NODE."

  (case (meta-node-type meta-node)
    (sync
     (create-function-meta-node meta-node))
    (async
     (create-async-meta-node meta-node))))


(defun async-meta-node? (meta-node)
  (eq (meta-node-type meta-node) 'async))

(defgeneric meta-node-type (meta-node)
  (:documentation
   "Returns SYNC if the meta-node computes its value synchronously,
    ASYNC if the meta-node computes its value asynchronously.")

  (:method ((meta-node symbol)) 'sync))

(defmethod meta-node-type ((meta-node meta-node))
  "Returns SYNC if `META-NODE' has no subnodes other than its operand
   nodes, that is the entire subgraph has been coalesced into the
   value function. Returns ASYNC if it has other subnodes besides the
   operand nodes."

  (with-slots (operands definition) meta-node
    (ensure-gethash
     meta-node
     *meta-node-types*

     (cond
       ;; If meta-node has no subnodes other than the operand nodes, it
       ;; can be coalesced to a single function
       ((= (hash-table-count (nodes definition))
           (length operands))

        'sync)

       (t 'async)))))


;;;; Generate dispatch methods

(defun init-nodes (nodes)
  "Generates the initialization code of each `NODE' in NODES."

  (maphash-values #'init-node nodes))

(defun init-node (node)
  "Generates the initialization code of NODE. This includes the
   binding of the node to its observers."

  (maphash (rcurry #'init-context node) (contexts node)))

(defun init-context (context-id context node)
  "Generates the initialization code of the node context CONTEXT, with
   identifier CONTEXT-ID, of the node NODE."

  (bind-observers node context-id context))

(defun bind-observers (node context-id context)
  "Generates code which binds the `NODE' NODE to its observers."

  (let* ((context-path (context-path node context-id)))

    (dohash (observer link (observers node))
      (unless (gethash observer (operands context))
        (with-accessors ((link-context node-link-context)) link
         (append-code
          (js-call
           (js-member context-path "add_observer")
           (context-path observer link-context)
           (dependency-index (context observer link-context) node))))))))
