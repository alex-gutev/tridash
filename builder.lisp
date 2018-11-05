;;;; builder.lisp
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

;;;; Build the node definition structures out of the parsed source
;;;; files.

(in-package :tridash.frontend)


;;;; Builder State

(defparameter *top-level* t
  "Flag for whether the current node is at the top-level.")

(defparameter *meta-node* nil
  "The meta-node whose subgraph is currently being built. NIL when the
   global graph is being built.")

(defparameter *source-node* nil
  "If the node currently being processed appears as the target of a
   binding, this variable is bound to the source node of the binding,
   otherwise is NIL.")


;;;; Utility Functions and Macros

(defmacro at-source (&body body)
  "Sets the node position to source in the dynamic extent of the forms
   in BODY. All declarations, processed by the forms in body, are
   treated as appearing in source position."

  `(let (*source-node*)
     ,@body))

(defmacro with-source-node (source &body body)
  "Evaluates the forms in BODY, with *SOURCE-NODE* bound to the
   result of the evaluation of the form SOURCE."

  `(let ((*source-node* ,source))
     ,@body))


;;;; Generic Functions

(defgeneric process-declaration (decl table)
  (:documentation
   "Processes the declaration, creates the node(s) described by the
    declaration and adds them to TABLE. Returns the node created, if any, and
    the table to which it was added (or in which it was found)."))

(defgeneric process-functor (operator operands table)
  (:documentation
   "Processes the declaration functor, creates the node(s) described
    by the declaration and adds them to TABLE. Returns the node
    created, if any, and the table to which it was added (or in which
    it was found)."))


;;;; Build Graph

(defun build-graph (parser &optional (*global-module-table* (make-instance 'module-table)))
  "Builds the graph from the objects returned by successively calling
   PARSER."

  (build-parsed-nodes parser)
  (finish-build-graph)

  *global-module-table*)

(defun build-parsed-nodes (parser &optional (*global-module-table* *global-module-table*))
  "Builds the `NODE' objects from the node declarations returned by
   successively calling PARSER and adds them to the `NODE-TABLE' of
   the current module in the `MODULE-TABLE' given in the second
   argument. This function does not build meta-node definitions."

  (with-slots (node-table) *global-module-table*
    (loop
       for decl = (funcall parser)
       while decl
       do
         (process-declaration decl node-table))))

(defun build-node (node &optional (*global-module-table* *global-module-table*))
  "Builds the `NODE' object from the node declarations NODE and adds
   it to the `NODE-TABLE' of the current module in the `MODULE-TABLE'
   given in the second argument."

  (process-declaration node (node-table *global-module-table*)))

(defun finish-build-graph (&optional (*global-module-table* *global-module-table*))
  "Builds the definitions of all meta-nodes in all modules, and
   performs node coalescing. This function should be called
   separately, to finish building the graph, after building individual
   nodes using BUILD-PARTIAL-GRAPH and BUILD-NODE. This function
   should not be called after calling BUILD-GRAPH."

  (with-slots (modules) *global-module-table*
    (maphash-values #'finish-build-module modules)
    (maphash-values #'coalesce-nodes modules)))

(defun finish-build-module (node-table)
  "Builds the definitions of the meta-nodes in NODE-TABLE."

  (create-value-functions node-table)

  (find-outer-node-references node-table)
  (add-outer-node-operands node-table))

(defun create-value-functions (graph)
  "Creates the value function of each node in GRAPH."

  (build-meta-node-graphs graph))


;;;; Build Meta-Node

(defun build-meta-node-graphs (table)
  "Builds the body of each meta-node, in the node table TABLE."

  (maphash-values (rcurry #'build-meta-node-graph table) (meta-nodes table)))

(defun build-meta-node-graph (meta-node outer-table)
  "Builds the graph corresponding to the body of the node
   meta-node. OUTER-TABLE is the node table in which the meta-node
   definition is located."

  (let ((table (make-inner-node-table outer-table))
        (*meta-node* meta-node))

    (add-operand-nodes (operands meta-node) table)

    (let* ((last-node (process-node-list (definition meta-node) table t)))
      (make-meta-node-function meta-node last-node)
      (create-value-functions table)
      (setf (definition meta-node) table))))

(defun add-operand-nodes (names table)
  "Creates a node for each element in NAMES, the element being the
   node name, and adds the nodes to TABLE. The nodes are marked as
   input nodes of TABLE"

  (dolist (name names)
    (add-input (add-node name (make-instance 'node :name name) table) table)))

(defun make-meta-node-function (meta-node last-node)
  "Creates the value function of the meta-node META-NODE. LAST-NODE is
   the last-node in the meta-node's definition."

  (with-slots (output-nodes contexts) meta-node
    (cond
      ((plusp (hash-table-count output-nodes))
       (with-slots (value-function) (context meta-node nil)
         (setf value-function
               (cons :object
                     (iter
                       (for (name node) in-hashtable output-nodes)
                       (collect (list name (add-binding node meta-node :context nil :add-function nil))))))))

      ((and last-node (zerop (hash-table-count contexts)))
       (add-binding last-node meta-node :context nil)))))


;;;; Methods: Processing Declaration

(defmethod process-declaration ((functor list) table)
  "Processes the functor by calling PROCESS-FUNCTOR with the OPERATOR
   argument being the CAR of FUNCTOR and OPERANDS being the CDR of
   FUNCTOR."

  (destructuring-bind (operator . operands) functor
    (process-functor operator operands table)))

(defmethod process-declaration ((name symbol) table)
  "Creates a node with identifier NAME and adds it to table, if table
   does not already contain a node with that identifier. Returns the
   newly created, or existing, node."

  (values (ensure-node name table) table))

(defmethod process-declaration (literal table)
  "Method for literal values (everything which is not a symbol or list
   is considered a literal). The literal value is simply returned."

  (declare (ignore table))
  literal)


;;; Special Nodes

(defmethod process-declaration ((name (eql +self-node+)) table)
  "Returns the current meta-node, bound to *META-NODE*. If *META-NODE*
   is NIL an error condition is signaled."

  (declare (ignore table))

  (if *meta-node*
      *meta-node*
      (error "Cannot reference node 'self' outside of a meta-node definition")))


;;;; Methods: Processing Functors

;;; Operators

;;; Operator Declarations

(defmethod process-functor ((operator (eql +op-operator+)) args table)
  "Registers a node as an infix operator with a specific precedence
   and associativity."

  (declare (ignore table))
  (destructuring-bind (op precedence &optional associativity) args
    (add-operator op precedence (operator-associativity associativity))))

(defun operator-associativity (assoc)
  "Returns the operator precedence (LEFT or RIGHT) for the precedence
   given as an argument to the op operator."

  (cond
    ((eq assoc (id-symbol "left"))
     'left)
    ((eq assoc (id-symbol "right"))
     'right)
    (t 'right)))


;;; Module Declarations

(defmethod process-functor ((operator (eql +module-operator+)) args table)
  "Changes the current module to the MODULE specified in ARGS."

  (declare (ignore table))

  (destructuring-bind (module) args
    (change-module module)))

(defmethod process-functor ((operator (eql +use-operator+)) args table)
  "Adds a module as a node to the TABLE."

  (iter (for module in args)
        (process-declaration (list +alias-operator+ module module) table)))

(defmethod process-functor ((operator (eql +alias-operator+)) args table)
  "Adds an alias for a module to TABLE."

  (destructuring-bind (module alias) args
    (match (gethash alias (all-nodes table))
      ((type node)
       (error 'alias-clash-error :alias alias :module module))

      ((and (type node-table) (not (eql table)))
       (error 'alias-taken-error :alias alias :module module))

      (nil
       (setf (module-alias alias table) (get-module module))))))

(defmethod process-functor ((operator (eql +import-operator+)) args table)
  "Imports nodes directly into TABLE, from another module."

  (with-slots (all-nodes) table
    (destructuring-bind (module &rest nodes) args
      (let ((module (get-module module)))
        (if nodes
            (mapcar (rcurry #'import-node module table) nodes)
            (maphash-keys (rcurry #'import-node module table) (all-nodes module)))))))

(defun import-node (name module table)
  "Import node NAME from MODULE into TABLE."

  (let* ((node (lookup-node name module)))
    (when (node? node)
      (with-slots (all-nodes) table
        (when (aand (gethash node all-nodes) (not (eq it node)))
          (error 'node-clash-error :name name :module module))

        (add-node name node table)))))


;;; Definitions

(defmethod process-functor ((operator (eql +def-operator+)) operands table)
  "Processes a meta-node definition. Creates a new meta-node and adds
   it to TABLE. If TABLE already contains a node with the same
   identifier but it is not a meta-node an error condition is
   signaled."

  (destructuring-bind ((name . args) . body) operands
    (add-meta-node
     name
     (make-instance 'meta-node :name name :operands args :definition body)
     table)))


;;; Conditions

(defmethod process-functor ((operator (eql +case-operator+)) operands table)
  "Process case expressions. A case expressions contains a list of
   conditions each of the form <condition> : <value>. The case
   expression resolves to the <value> of the first <condition> which
   resolves to true."

  (flet ((make-if (case expr)
           (match case
             ((list (eql +def-operator+) cond node)
              (list (id-symbol "if") cond node expr))

             (_ case))))
    (process-declaration
     (reduce #'make-if operands :from-end t :initial-value nil)
     table)))


;;; Node lists

(defmethod process-functor ((operator (eql +list-operator+)) nodes table)
  "Processes a list of nodes, returns the last node in the list."

  (process-node-list nodes table))

(defun process-node-list (nodes table &optional (*top-level* *top-level*))
  "Process a list of nodes, returns the last node in the list."

  (let ((top-level *top-level*))
    (loop
       for (decl . rest) on nodes
       ;; At top-level if not last node or list node is at top-level
       for *top-level* = (or rest top-level)
       for (node node-table) = (multiple-value-list (process-declaration decl table))
       finally
         (return (values node node-table)))))


;;; Outer nodes

(defmethod process-functor ((operator (eql +outer-operator+)) args table)
  (with-slots (outer-table) table
    (unless outer-table
      (error "Outer node ~a referenced from global node table" (cons operator args)))

    (destructuring-bind (name) args
      (multiple-value-bind (outer-node outer-table)
          (lookup-node name outer-table)

        (if outer-node
            (add-outer-node outer-node outer-table table)
            (error "Node ~a not found in any outer node table" name))))))

(defun add-outer-node (outer-node outer-table table)
  "Adds a reference to NODE, which is located in an outer node-table (OUTER-TABLE),
   to the current meta-node being built (the value of *META-NODE*)."

  (with-slots (outer-nodes) *meta-node*
    (let ((name (cdr (ensure-gethash outer-node outer-nodes (cons outer-table (outer-node-name *meta-node*))))))
      (values (ensure-node name table) table))))


;;; Bindings

(defmethod process-functor ((operator (eql +bind-operator+)) operands table)
  "Establishes a binding from the first node to the second node in
   OPERANDS."

  (destructuring-bind (source target) operands
    (with-source-node (at-source (process-declaration source table))
      (let* ((target (let (*top-level*) (process-declaration target table)))
             (value-link (add-binding *source-node* target)))

        (unless *top-level*
          (let* ((name (cons operator operands))
                 (cond-node (ensure-node name table))
                 (cond-link (add-binding cond-node target :context *source-node* :add-function nil)))

            (setf (value-function (context target *source-node*))
                  `(if ,cond-link ,value-link ,(node-link :self)))

            (values cond-node table)))))))


;;; Output Nodes

(defmethod process-functor ((operator (eql +out-operator+)) operands table)
  "Creates the output node and adds it to the set of output nodes of
   the meta-node currently bound to *META-NODE*."

  (destructuring-bind (name) operands
    (with-slots (output-nodes) *meta-node*
      (aprog1 (ensure-node (cons operator operands) table)
        (setf (gethash name output-nodes) it)))))


;;; Subnodes

(defmethod process-functor ((operator (eql +subnode-operator+)) operands table)
  "Creates a node with a value function that accesses a particular
   field of an object. The binding is created in both directions, from
   the object node (the node containing the object value) to the
   subnode and from the subnode to the object node. In the latter
   direction the value of the field of the object value, stored in the
   object node, is updated."

  (destructuring-bind (node key) operands
    (process-subnode (process-declaration node table) node key table)))


(defgeneric process-subnode (object-node object-decl key table)
  (:documentation
   "Generic function for processing subnode expressions."))

(defmethod process-subnode ((object-node node) object-decl key table)
  "Creates a node which references a field of an object stored in
   another node."

  (let* ((name (list +subnode-operator+ object-decl key))
         (subnode (ensure-node name table)))
    (make-source-subnode object-decl key subnode table)

    (handler-case
        (make-target-subnode object-decl key subnode table)
      (target-node-error ()))

    (values subnode table)))


(defun make-source-subnode (node key subnode table)
  "Makes the value function of the subnode for when it appears as the
   source of a binding. NODE is the object node referenced, KEY is the
   subnode key and SUBNODE is the subnode `NODE' object."

  (let ((node (at-source (process-declaration node table))))
    (create-context (subnode node)
      (setf value-function (list :member (bind node) key)))))

(defun make-target-subnode (node key subnode table)
  "Binds the subnode to the object node and updates the value function
   of the object node. NODE is the object node declaration, KEY is the
   field and SUBNODE is the subnode `NODE' object."

  (with-source-node subnode
    (let ((object-node (process-declaration node table)))
      (create-context (object-node :object)
        (setf value-function (list :object)))

      (ensure-binding (subnode object-node :context :object :add-function nil)
          (link)
        (push (list key link) (cdr (value-function (context object-node :object))))))))

(defmethod process-subnode ((module node-table) object-decl key table)
  "Returns the node with identifier KEY in the module MODULE."

  (declare (ignore object-decl table))
  (values (lookup-node key module) module))

;;; Meta-Node instances

(defmethod process-functor (operator operands table)
  "Creates a node with a VALUE-FUNCTION that invokes the meta-node
   with identifier OPERATOR and with operands OPERANDS. The operand
   nodes are added as dependencies of the node."

  (acond
    ((lookup-meta-node operator table)
     (make-meta-node-instance it operator operands table))
    (t
     (error "Operator ~s is neither a special operator nor a node meta-node" operator))))


;;; Meta-Node Instances

(defun make-meta-node-instance (meta-node operator operands table)
  "Creates a node which invokes the meta-node META-NODE with operands
   OPERANDS, and adds it to TABLE."

  (match meta-node
    ((list :type _)
     (create-type-conversion-node meta-node operator operands table))

    (_
     (if *source-node*
         (error 'target-node-error :node (cons operator operands))
         (add-meta-node-instance meta-node operator operands table)))))


(defun add-meta-node-instance (meta-node operator operands table)
  "Creates a node with a VALUE-FUNCTION function that invokes the
   meta-node META-NODE with operands OPERANDS, and adds it to
   TABLE. If TABLE already contains such a node it is returned."

  (multiple-value-bind (instance operands table)
      (create-instance-node meta-node operator operands table)
    (add-meta-node-value-function instance meta-node operands)

    (values instance table)))

(defun create-instance-node (meta-node operator operands table)
  "Creates a meta-node instance node. The node is created with
   name (CONS OPERATOR OPERANDS) and added to TABLE. Additionally
   processes the operand node declarations in OPERANDS. Does not
   create the value function of the instance node. Returns three
   values: the instance node, the operand nodes and the table to which
   the instance was added."

  (let ((name (cons operator operands)))
    (multiple-value-bind (operands table) (process-operands operands table)

      ;; Add META-NODE to the meta node references of *META-NODE*
      (when (and *meta-node*
                 (not (eq meta-node *meta-node*))
                 (meta-node? meta-node))
        (ensure-gethash meta-node (meta-node-references *meta-node*)))

      (values (ensure-node name table t) operands table))))

(defun add-meta-node-value-function (instance meta-node operands &key (context meta-node))
  "Creates the value function of the meta-node instance INSTANCE,
   which invokes META-NODE with operands OPERANDS. OPERANDS are bound
   to INSTANCE and added as operands to the context CONTEXT."

  (create-context (instance context)
    (add-to-instances instance meta-node)
    (setf value-function (cons meta-node (bind-operands instance operands :context context)))))

(defun add-to-instances (instance meta-node)
  "Adds the meta-node instance INSTANCE to the list of instances of
   META-NODE."

  (when (meta-node? meta-node)
    (push (cons instance *meta-node*) (instances meta-node))))


;;; Type Conversion Nodes

(defun create-type-conversion-node (meta-node operator operands table)
  "Creates a node with a value function that converts the received
   value to a particular type. A two-way binding between the node and
   operand node (in OPERANDS) is created."

  (destructuring-bind (node) operands
    (multiple-value-bind (instance operands table)
        (create-instance-node meta-node operator operands table)

      (add-meta-node-value-function instance meta-node operands :context (first operands))

      (with-source-node instance
        (handler-case
            (let ((node (process-declaration node table)))
              (add-meta-node-value-function node meta-node (list instance) :context instance))
          (target-node-error ())))

      (values instance table))))


;;; Binding Operands

(defun process-operands (operands table)
  "Creates the operand nodes and adds them to table if they are not
   already in table. Returns the list of `node' objects as the first
   value and the table, in which a node object was found or created,
   with the greatest depth."

  (let ((*top-level* nil)
        (*source-node* nil)
        (*create-nodes* t))
    (iter
      (with op-table = (node-table *global-module-table*))
      (for operand in operands)

      (multiple-value-bind (node node-table)
          (at-source (process-declaration operand table))

        (collect node into nodes)

        (when (and node-table (> (depth node-table) (depth op-table)))
          (setf op-table node-table)))

      (finally (return (values nodes op-table))))))

(defun bind-operands (node operands &key context)
  "Establishes bindings between the operands and the meta-node
   instance."

  (flet ((bind-operand (operand)
           (if (value? operand)
               operand
               (add-binding operand node :context context :add-function nil))))
  (mapcar #'bind-operand operands)))
