;;;; node-table.lisp
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

;;;; `node-table' class and associated functions.

(in-package :tridash.frontend)

(defclass node-table ()
  ((name
    :accessor name
    :initarg :name
    :initform nil
    :documentation
    "The name of the module.")

   (outer-table
    :accessor outer-table
    :initarg :outer-table
    :initform nil
    :documentation
    "The node-table corresponding to the outer scope of this table.")

   (depth
    :accessor depth
    :initarg :depth
    :initform 0
    :documentation
    "The depth of the current table. The global table is at depth 0.")

   (nodes
    :accessor nodes
    :initform (make-hash-map)
    :documentation
    "Map of the actual nodes (excluding meta-nodes and module
     aliases).")

   (meta-nodes
    :accessor meta-nodes
    :initform (make-hash-map)
    :documentation
    "Map of the meta-nodes.")

   (module-aliases
    :accessor module-aliases
    :initarg :module-aliases
    :initform (make-hash-map)
    :documentation
    "Map of the model aliases. Each key is the alias symbol and the
     corresponding value is the module's `NODE-TABLE'.")

   (all-nodes
    :accessor all-nodes
    :initarg :all-nodes
    :initform (make-hash-map)
    :documentation
    "Map of all nodes.")

   (public-nodes
    :accessor public-nodes
    :initarg :public-nodes
    :initform (make-hash-map)
    :documentation
    "Map of nodes which are exported from the module. When this module
     is imported into another module by import(<module name>), the
     nodes in this table are imported into the module.")

   (operator-nodes
    :accessor operator-nodes
    :initarg :operator-nodes
    :initform (copy +infix-operators+)
    :documentation
    "Operator table for the module. The keys are the names of the
     nodes which can appear as infix operators and the corresponding
     values are lists containing the operator's precedence and
     associativity.")

   (input-nodes
    :accessor input-nodes
    :initform (make-hash-set)
    :documentation
    "Set of all input nodes of the graph."))

  (:documentation
   "Symbol table mapping node identifiers to node objects. Has
    multiple tables for all types of (pseudo) nodes to facilitate
    looking up a node of a particular type."))


;;;; Constructor Functions

(defun make-inner-node-table (table)
  "Creates a new node-table that is contained in TABLE."

  (with-slots (name meta-nodes module-aliases operator-nodes) table
    (make-instance 'node-table
                   :name name
                   :outer-table table
                   :depth (1+ (depth table))
                   :module-aliases (copy module-aliases)
                   :operator-nodes (copy operator-nodes)
                   :all-nodes (copy module-aliases))))


;;;; Predicates

(defun global-table? (table)
  (zerop (depth table)))


;;;; Node lookup functions

(defvar *create-nodes* t
  "Flag: If true ENSURE-NODE will create a new node rather than
   searching up the outer node tables.")

(defvar *search-module* nil
  "The name of the module in which the node is currently being looked up")

(defparameter *return-meta-node* nil
  "Flag for whether meta-nodes should be returned by
   PROCESS-DECLARATION. If NIL PROCESS-DECLARATION signals an error if
   the node returned is a meta-node.")


(defun ensure-node (name table &optional (create *create-nodes*))
  "Searches for the node with identifier NAME in TABLE. If CREATE is
   true (defaults to the value of *CREATE-NODES*) and the node is not
   found in TABLE, it is created otherwise it is searched for in the
   enclosing tables of TABLE."

  (flet ((create-node (c)
           (declare (ignore c))
           (when create
             (return-from ensure-node
               (add-node name (make-instance 'node :name name) table)))))

    (handler-bind
        ((non-existent-node #'create-node))
      (lookup-node name table (unless create (outer-table table))))))

(defun lookup-node (name table &optional (next-table (and table (outer-table table))))
  "Searches for the node with identifier NAME, in TABLE. If the node
   is not found in TABLE, TABLE's OUTER-TABLE is searched
   recursively. If the the node is found it is returned in the first
   value along with the table, in which it was found, in the second
   value. If no node is found a `NON-EXISTENT-NODE' condition is
   signaled."

  (let ((*search-module* (or *search-module* (and table (name table)))))

   (unless table
     (error 'non-existent-node :node name :module-name *search-module*))

    (aif (get name (all-nodes table))
         (values it table)
         (lookup-node name next-table))))

(defun lookup-meta-node (meta-node table)
  "Searches for a meta-node by processing the declaration (using
   PROCESS-DECLARATION) with *CREATE-NODES* set to nil, that is no
   immediate nodes are created if the node is not found. If
   PROCESS-DECLARATION returns a meta-node it is returned. If
   PROCESS-DECLARATION returns a node which is not a meta-node a
   `NODE-TYPE-ERROR' condition is signaled. If no node is found but
   META-NODE names a primitive operator (in *PRIMITIVE-OPS*) it is
   returned."

  (let ((*create-nodes* nil)
        (*return-meta-node* t))
    (aprog1 (process-declaration meta-node table)
      (unless (meta-node? it)
        (error 'node-type-error :node it :expected 'meta-node)))))

(defun node-type (node)
  "Returns a symbol identifying the type of NODE. META-NODE is
   returned if NODE is a meta-node. NODE is returned if NODE is an
   ordinary node. MODULE is returned if NODE is a
   module (`NODE-TABLE') otherwise LITERAL is returned."

  (typecase node
    (meta-node 'meta-node)
    (node 'node)
    (node-table 'module)
    (otherwise 'literal)))

(defun in-home-module? (node table)
  "Returns true if NODE was declared in the module TABLE, false if it
   was imported into it."

  (eq (home-module node) table))


;;;; Adding nodes

(defun add-node (name node table)
  "Adds the ordinary node NODE with name NAME to TABLE."

  (with-slots (all-nodes nodes) table
    (when (memberp name all-nodes)
      (error 'node-exists-error :node name :node-table table))

    (unless (get :module (attributes node))
      (setf (get :module (attributes node)) table))

    (setf (get name all-nodes) node)
    (setf (get name nodes) node)))

(defun add-meta-node (name node table)
  "Adds the `META-NODE' NODE with name NAME to TABLE."

  (with-slots (all-nodes nodes meta-nodes) table
    (cond
      ((memberp name +special-operators+)
       (error 'special-operator-name-error :node name))

      ((aand (get name all-nodes) (not (meta-node? it)))
       (error 'meta-node-name-collision :node name :node-table table)))

    (unless (get :module (attributes node))
      (setf (get :module (attributes node)) table))

    (setf (get name all-nodes) node)
    (setf (get name meta-nodes) node)))

(defun add-external-meta-node (name table)
  "Adds a stub for an externally defined meta-node with name NAME to
   TABLE."

  (add-meta-node
   name
   (make-instance 'external-meta-node :name name)
   table))


;;; Removing nodes

(defun remove-node (name table)
  "Removes the ordinary node with name NAME from TABLE."

  (when (erase (nodes table) name)
    (erase (all-nodes table) name)))


;;; Input Nodes

(defun add-input (node table)
  "Adds NODE to the input nodes of TABLE and sets its :INPUT attribute
   to T."

  (unless (input-node? node)
    (setf (attribute :input node) t)
    (context node :input)
    (nadjoin node (input-nodes table))))


;;; Importing and Exporting Nodes

(defun import-node (name module table)
  "Import node NAME from MODULE into TABLE."

  (let* ((node (lookup-node name module)))
    (with-slots (all-nodes module-aliases) table
      (when (aand (get name all-nodes) (/= it node))
        (error 'import-node-error
               :node name
               :module module
               :node-table table))

      (when (= (node-type node) 'module)
        (setf (get name module-aliases) node))

      (setf (get name all-nodes) node)

      (awhen (get name (operator-nodes module))
        (add-operator name (first it) (second it) (operator-nodes table))))))

(defun export-node (name table)
  "Adds the node with name NAME to the PUBLIC-NODES of table."

  (setf (get name (public-nodes table))
        (lookup-node name table)))


;;;; Module Aliases

(defun module-alias (alias table)
  "Returns the `NODE-TABLE' of the module with alias ALIAS in TABLE."

  (get alias (module-aliases table)))

(defun (setf module-alias) (module alias table)
  "Adds ALIAS as an alias for MODULE in TABLE."

  (setf (get alias (all-nodes table)) module)
  (setf (get alias (module-aliases table)) module))
