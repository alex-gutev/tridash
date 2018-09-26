;;;; node-table.lisp
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

;;;; `node-table' class and associated functions.

(in-package :metalink.frontend)

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
    "Hash-table containing the meta-nodes."))

  (:documentation
   "Stores the hash-table mapping node names to `node'
    objects. Additionally nodes are separated into different tables
    for nodes and meta-nodes."))


;;; Constructor Function

(defun make-inner-node-table (table)
  "Creates a new node-table that is contained in TABLE."
  (make-instance 'node-table :outer-table table :depth (1+ (depth table))))


;;; Predicates

(defun global-table? (table)
  (zerop (depth table)))


;;; Node lookup functions

(defun lookup-node (name table)
  "Searches for the node with identifier NAME, in TABLE. If the node
   is not found in TABLE, TABLE's OUTER-TABLE is searched
   recursively (if it is not NIL). Returns two values, the node
   found (NIL if none found) and the table in which it was found."

  (when table
    (aif (gethash name (all-nodes table))
         (values it table)
         (lookup-node name (outer-table table)))))

(defun lookup-meta-node (meta-node table)
  "Searches for the meta-node with identifier META-NODE in TABLE. If
   the meta-node is not found in TABLE, TABLE's OUTER-TABLE is
   searched recursively (if it is not NIL). If META-NODE names a
   primitive operator (found in *PRIMITIVE-OPS*) it is returned."

  (or (aand (lookup-node meta-node table)
         (meta-node? it)
         it)
      (gethash meta-node *primitive-ops*)))

(defun ensure-node (name table &optional (create t))
  "Returns the node with identifier NAME in TABLE. If table does not
  contain such a node, and CREATE is true a new node is created, added
  to table and returned, otherwise an error condition is signaled. If
  TABLE does contain a node with identifier NAME however it is a
  `meta-node' an error condition is signalled."

  (acond
    ((gethash name (all-nodes table))
     (when (meta-node? it)
       (error "~a is a node meta-node" name))
     it)

    (create
     (add-node name (make-instance 'node :name name) table))

    (t
     (error "No node ~a in local scope" name))))


;;; Adding nodes

(defun add-node (name node table)
  (with-slots (all-nodes nodes) table
    (setf (gethash name all-nodes) node)
    (setf (gethash name nodes) node)))

(defun add-meta-node (name node table)
  (with-slots (all-nodes nodes meta-nodes) table
    (when (gethash name nodes)
      (error "~a already names a node which is not a meta-node" name))

    (setf (gethash name all-nodes) node)
    (setf (gethash name meta-nodes) node)))


;;; Utility Functions

(defun input-nodes (graph)
  "Returns the list of nodes which have no one-way dependencies."

  (iter
    (for (nil node) in-hashtable (nodes graph))
    (when (input-node? node) (collect node))))

(defun input-node? (node)
  "Returns true if NODE is an input node that is it has no one-way
   dependencies."

  (iter (for (nil link) in-hashtable (dependencies node))
        (always (node-link-2-way-p link))))
