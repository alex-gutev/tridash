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
  ((outer-table
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

   (all-nodes
    :accessor all-nodes
    :initarg :all-nodes
    :initform (make-hash-table :test #'equal)
    :documentation
    "Hash-table containing all nodes (including meta-nodes).")

   (nodes
    :accessor nodes
    :initform (make-hash-table :test #'equal)
    :documentation
    "Hash-table only containing actual nodes (excluding meta-nodes).")

   (meta-nodes
    :accessor meta-nodes
    :initform (make-hash-table :test #'eq)
    :documentation
    "Hash-table containing the meta-nodes.")

   (module-aliases
    :accessor module-aliases
    :initarg :module-aliases
    :initform (make-hash-table :test #'eq)
    :documentation
    "Hash-table mapping module alias symbols to the `NODE-TABLE' of
     the module.")

   (operator-nodes
    :accessor operator-nodes
    :initarg :operator-nodes
    :initform (copy-hash-table +infix-operators+)
    :documentation
    "Operator table for the module. The keys are the names of the
     nodes which can appear as infix operators and the corresponding
     values are lists containing the operator's precedence and
     associativity.")

   (input-nodes
    :accessor input-nodes
    :initform nil
    :documentation
    "List containing the input nodes of the graph."))

  (:documentation
   "Stores the hash-table mapping node names to `node'
    objects. Additionally nodes are separated into different tables
    for nodes and meta-nodes."))


;;; Constructor Function

(defun make-inner-node-table (table)
  "Creates a new node-table that is contained in TABLE."

  (make-instance 'node-table
                 :outer-table table
                 :depth (1+ (depth table))
                 :module-aliases (copy-hash-table (module-aliases table))
                 :operator-nodes (copy-hash-table (operator-nodes table))
                 :all-nodes (copy-hash-table (module-aliases table))))


;;; Predicates

(defun global-table? (table)
  (zerop (depth table)))


;;; Node lookup functions

(defvar *create-nodes* t
  "Flag: If true ENSURE-NODE will create a new node rather than
   searching up the outer node tables.")

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

  (unless table
    (error 'non-existent-node :node name :node-table table))

  (aif (gethash name (all-nodes table))
       (values it table)
       (lookup-node name next-table)))


(defun lookup-meta-node (meta-node table)
  "Searches for a meta-node by processing the declaration (using
   PROCESS-DECLARATION) with *CREATE-NODES* set to nil, that is no
   immediate nodes are created if the node is not found. If a
   PROCESS-DECLARATION returns a meta-node it is returned. If
   PROCESS-DECLARATION returns a node which is not a meta-node a
   `NODE-TYPE-ERROR' condition is signaled. If no node is found but
   META-NODE names a primitive operator (in *PRIMITIVE-OPS*) it is
   returned."

  (flet ((primitive-op (c)
           (declare (ignore c))
           (awhen (gethash meta-node *primitive-ops*)
             (return-from lookup-meta-node it))))

    (let ((*create-nodes* nil))
      (handler-bind
          ((non-existent-node #'primitive-op))

        (aprog1 (process-declaration meta-node table)
          (unless (meta-node? it)
            (error 'node-type-error :node it :expected 'meta-node)))))))


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


;;; Adding nodes

(defun add-node (name node table)
  (with-slots (all-nodes nodes) table
    (setf (gethash name all-nodes) node)
    (setf (gethash name nodes) node)))

(defun add-meta-node (name node table)
  (with-slots (all-nodes nodes meta-nodes) table
    (when (aand (gethash name all-nodes) (not (meta-node? it)))
      (error "~a already names a node which is not a meta-node" name))

    (setf (gethash name all-nodes) node)
    (setf (gethash name meta-nodes) node)))


;;; Removing nodes

(defun remove-node (name table)
  "Removes the node with name NAME from TABLE."

  (remhash name (nodes table))
  (remhash name (all-nodes table)))


;;; Input Nodes

(defun add-input (node table)
  "Adds NODE to the input nodes of TABLE and sets its :INPUT attribute
   to T."

  (unless (input-node? node)
    (setf (gethash :input (attributes node)) t)
    (context node :input)
    (push node (input-nodes table))))


;;;; Module Aliases

(defun module-alias (alias table)
  "Returns the `NODE-TABLE' of the module with alias ALIAS in TABLE."

  (gethash alias (module-aliases table)))

(defun (setf module-alias) (module alias table)
  "Adds ALIAS as an alias for MODULE in TABLE."

  (setf (gethash alias (all-nodes table)) module)
  (setf (gethash alias (module-aliases table)) module))
