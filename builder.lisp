;;;; builder.lisp
;;;;
;;;; Metalink Programming Language.
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

(in-package :metalink.frontend)


;;;; Special Operators

(defconstant +outer-operator+ (id-symbol "..")
  "Special operator for referencing nodes defined in an outer scope.")

(defconstant +out-operator+ (id-symbol "out")
  "Special operator for creating output nodes, from meta-nodes.")

(defconstant +subnode-operator+ (id-symbol ".")
  "Special operator for accessing meta-node output nodes from outside
   the meta-node.")

(defconstant +self-node+ (id-symbol "self")
  "Special node representing the value of the current meta-node.")


;;;; Builder State

(defparameter *global-node-table* nil
  "The global node table.")

(defparameter *top-level* t
  "Flag for whether the current node is at the top-level.")

(defparameter *meta-node* nil
  "The meta-node whose subgraph is currently being built. NIL when the
   global graph is being built.")

(defparameter *target?* nil
  "True if the node currently being built appears as the target of a
   binding.")


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


;;;; Build Graphs

(defun build-graph (parser)
  "Builds the graph from the objects returned by successively calling
   PARSER."

  (let ((*global-node-table* (make-instance 'node-table)))
    (loop
       for decl = (funcall parser)
       while decl
       do
         (process-declaration decl *global-node-table*))

    (build-meta-node-graphs *global-node-table*)
    (maphash-values #'create-value-function (all-nodes *global-node-table*))

    (find-outer-node-references *global-node-table*)
    (add-outer-node-operands *global-node-table*)

    *global-node-table*))

(defun build-meta-node-graphs (table)
  "Builds the body of each meta-node, in the node table TABLE."

  (maphash-values (rcurry #'build-meta-node-graph table) (meta-nodes table)))

(defun build-meta-node-graph (meta-node outer-table)
  "Builds the graph corresponding to the body of the node
   meta-node. OUTER-TABLE is the node table in which the meta-node
   definition is located."

  (let ((table (make-inner-node-table outer-table))
        (*meta-node* meta-node))
    (add-local-nodes (operands meta-node) table)

    (let* ((last-node (process-node-list (definition meta-node) table t)))

      (make-meta-node-function meta-node last-node)

      (build-meta-node-graphs table)
      (maphash-values #'create-value-function (all-nodes table))

      (setf (definition meta-node) table))))

(defun add-local-nodes (names table)
  "Creates a node for each element in NAMES, the element being the
   node name, and adds the nodes to TABLE."

  (dolist (name names)
    (add-node name (make-instance 'node :name name) table)))

(defun make-meta-node-function (meta-node last-node)
  "Creates the value function of the meta-node META-NODE. LAST-NODE is
   the last-node in the meta-node's definition."

  (with-slots (output-nodes value-function) meta-node
    (cond
      ((plusp (hash-table-count output-nodes))
       (push (cons :object
                   (iter
                     (for (name node) in-hashtable output-nodes)
                     (collect (list name (add-binding node meta-node nil)))))
             value-function))

      ((and last-node (null value-function))
       (add-binding last-node meta-node)))))


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
  "Establishes a one-way binding from the first to the second node in
   OPERANDS."

  (let ((source (process-declaration (first operands) table))
        (target (let ((*target?* t)) (process-declaration (second operands) table))))

    (let ((value-link (add-binding source target)))
      (unless *top-level*
        (let* ((name (cons operator operands))
               (cond-node (ensure-node name table))
               (cond-link (add-binding cond-node target nil)))

          (add-condition target cond-link value-link)
          (values cond-node table))))))


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
  "Creates a node with a value function that accesses an output node
   of a set of outputs returned by a meta-node instance."

  (destructuring-bind (node key) operands
    (let ((node (process-declaration node table)))
      (let* ((name (cons operator operands))
             (subnode (ensure-node name table)))

        (if *target?*
            (make-target-subnode node key subnode)
            (make-source-subnode node key subnode))
        subnode))))

(defun make-source-subnode (node key subnode)
  "Makes the value function of the subnode for when it appears as the
   source of a binding. NODE is the object node referenced, KEY is the
   subnode key and SUBNODE is the `NODE' object of the subnode."

  (unless (value-function subnode)
    (push (list :member (add-binding node subnode nil) key)
          (value-function subnode))))

(defun make-target-subnode (node key subnode)
  "Makes the value function of the subnode for when it appears as the
   target of a binding. NODE is the object node referenced, KEY is the
   subnode key and SUBNODE is the `NODE' object of the subnode."

  (with-slots (value-function) node
    (unless value-function
      (push (list :object) value-function))

    (push (list key (add-binding subnode node nil))
          (cdar value-function))))


;;; Meta-Node instances

(defmethod process-functor (operator operands table)
  "Creates a node with a VALUE-FUNCTION that invokes the meta-node
   with identifier OPERATOR and with operands OPERANDS. The operand
   nodes are added as dependencies of the node."

  (acond
    ((lookup-meta-node operator table)
     (add-meta-node-instance it operator operands table))
    (t
     (error "Operator ~s is neither a special operator nor a node meta-node" operator))))


;;; Meta-Node Instances

(defun add-meta-node-instance (meta-node operator operands table)
  "Creates a node with a VALUE-FUNCTION function that invokes the
   meta-node META-NODE with operands OPERANDS, and adds it to
   table. If table already contains such a node it is returned."

  (let ((name (cons operator operands)))
    (multiple-value-bind (operands table) (process-operands operands table)

      ;; Add META-NODE to the meta node references of *META-NODE*
      (when (and *meta-node*
                 (not (eq meta-node *meta-node*))
                 (meta-node? meta-node))
        (ensure-gethash meta-node (meta-node-references *meta-node*)))

      (let* ((node (ensure-node name table)))
        (unless (value-function node)
          (add-to-instances node meta-node)

          (push (cons meta-node (bind-operands node operands))
                (value-function node)))

        (values node table)))))

(defun add-to-instances (instance meta-node)
  "Adds the meta-node instance INSTANCE to the list of instances of
   META-NODE."

  (when (meta-node? meta-node)
    (push (cons instance *meta-node*) (instances meta-node))))


(defun process-operands (operands table)
  "Creates the operand nodes and adds them to table if they are not
   already in table. Returns the list of `node' objects as the first
   value and the table, in which a node object was found or created,
   with the greatest depth."

  (let ((*top-level* nil))
    (iter
      (with op-table = *global-node-table*)
      (for operand in operands)
      (multiple-value-bind (node node-table)
          (process-declaration operand table)

        (collect node into nodes)

        (when (and node-table (> (depth node-table) (depth op-table)))
          (setf op-table node-table)))

      (finally (return (values nodes op-table))))))

(defun bind-operands (node operands)
  "Establishes bindings between the operands and the meta-node
   instance."

  (flet ((bind-operand (operand)
           (if (value? operand)
               operand
               (add-binding operand node nil))))
  (mapcar #'bind-operand operands)))
