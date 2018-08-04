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


(defparameter *global-node-table* nil
  "The global node table.")

(defparameter *top-level* t
  "Flag for whether the current node is at the top-level.")


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
    *global-node-table*))

(defun build-meta-node-graphs (table)
  "Builds the body of each meta-node, in the node table TABLE."

  (maphash-values (rcurry #'build-meta-node-graph table) (meta-nodes table)))

(defun build-meta-node-graph (meta-node outer-table)
  "Builds the graph corresponding to the body of the node
   meta-node. OUTER-TABLE is the node table in which the meta-node
   definition is located."

  (let ((table (make-inner-node-table outer-table)))
    (add-local-nodes (operands meta-node) table)
    
    (let* ((*top-level* nil)
           (last-node (process-node-list (definition meta-node) table)))

      (when last-node
        (add-binding last-node meta-node))

      (build-meta-node-graphs table)
      (setf (definition meta-node) table))))

(defun add-local-nodes (names table)
  "Creates a node for each element in NAMES, the element being the
   node name, and adds the nodes to TABLE."
  
  (dolist (name names)
    (add-node name (make-instance 'node :name name) table)))


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

(defun process-node-list (nodes table)
  "Process a list of nodes, returns the last node in the list."
  
  (let ((top-level *top-level*))
    (loop
       for (decl . rest) on nodes
       ;; At top-level if not last node or list node is at top-level
       for *top-level* = (or rest top-level)
       for (node node-table) = (multiple-value-list (process-declaration decl table))
       finally
         (return (values node node-table)))))

;;; Bindings

(defmethod process-functor ((operator (eql +bind-operator+)) operands table)
  "Establishes a one-way binding from the first to the second node in
   OPERANDS."

  (multiple-value-bind (operands table) (process-operands operands table)
    (destructuring-bind (source target) operands
      (let ((value-link (add-binding source target)))
        (unless *top-level*
          (let* ((name (cons operator operands))
                 (cond-node (ensure-node name table))
                 (cond-link (add-binding cond-node target nil)))
          
            (add-condition target cond-link value-link)
            (values cond-node table)))))))


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
      
      (let* ((node (ensure-node name table)))
        (unless (value-function node)
          (appendf (value-function node)
                   (list (cons meta-node (bind-operands node operands)))))

        (values node table)))))


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
