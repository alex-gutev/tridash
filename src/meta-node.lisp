;;;; meta-node.lisp
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

;;;; `meta-node' class definition.

(in-package :tridash.frontend)

(defclass meta-node (node)
  ((operands
    :initarg :operands
    :initform nil
    :accessor operands
    :documentation
    "List containing the symbols naming the meta-node's operands.")

   (outer-nodes
    :initform (make-hash-map)
    :accessor outer-nodes
    :documentation
    "Map of nodes (found in outer node tables) referenced from the
     meta-node's definition. Each key is the outer `NODE' object with
     the corresponding value being the name of the local node to
     which it is bound.")

   (meta-node-references
    :initform (make-hash-set)
    :accessor meta-node-references
    :documentation
    "Set of meta-nodes of which there are instances in the
     meta-node's definition.")

   (definition
    :initarg :definition
    :initform nil
    :accessor definition
    :documentation
    "The graph corresponding to the body of the meta-node. Prior to
     the graph being built the list of node declarations, making up
     the body, are stored in this slot. After the meta-node's
     definition is built, this slot contains a `FLAT-NODE-TABLE'
     containing the nodes in the meta-node's body.")

   (instances
    :initform nil
    :accessor instances
    :documentation
    "List of instances of the meta-node. Each element is of the
     form (CONS NODE META-NODE) where NODE is the instance NODE itself
     and META-NODE is the META-NODE in which it appears, NIL if it is
     at global scope."))

  (:documentation
   "Stores the definition of a meta-node."))

(defclass external-meta-node (meta-node)
  ()

  (:documentation
   "Node stub for a meta-node which is defined externally."))


;;; Utilities

(defun process-meta-node (fn)
  "Returns a function of one argument, a meta-node, that applies FN on
   the definition of the meta-node. If the definition of the meta-node
   is NIL, FN is not applied on it."

  (lambda (meta-node)
    (awhen (definition meta-node)
      (funcall fn it))))

(defun node-macro-function (meta-node)
  "Returns the meta-nodes macro function if it is a macro."

  (get :macro-function (attributes meta-node)))

(defun (setf node-macro-function) (fn meta-node)
  "Sets the macro function of a meta-node. This essentially makes
   META-NODE a macro."

  (setf (get :macro-function (attributes meta-node)) fn))


;;; Predicates

(defun meta-node? (x)
  "Returns true if X is a `meta-node'."
  (typep x 'meta-node))

(defun external-meta-node? (x)
  "Returns true if X is an externally defined meta-node."
  (typep x 'external-meta-node))


;;; Meta-Node Attributes

(defun target-meta-node (meta-node)
  "Returns the name of the meta-node to use for the binding from the
   meta-node instance to the meta-node operands."

  (attribute :target-node meta-node))


;;; Outer Node References

(defun unique-node-name (hash-table prefix)
  "Generates a new unique node identifier. The identifier generated is
   a CONS with with the CAR set to PREFIX and the CDR set to the
   number of entries in HASH-TABLE."

  (cons prefix (length hash-table)))

(defun outer-node-name (meta-node)
  "Generates a new name for a local node which will be used to
   reference an outer node."

  (unique-node-name (outer-nodes meta-node) 'ex))

(defun outer-node (node outer-table meta-node)
  "Returns the name of the local node which is bound to the outer node
   NODE, from the table OUTER-TABLE, in META-NODE."

  (with-slots (outer-nodes) meta-node
    (cdr (ensure-get node outer-nodes (cons outer-table (outer-node-name meta-node))))))
