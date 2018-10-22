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
    "Symbols naming the meta-node operands.")

   (outer-nodes
    :initform (make-hash-table :test #'eq)
    :accessor outer-nodes
    :documentation
    "Set of nodes (found in outer node tables) referenced from the
     meta-node's definition. Each key is the outer `NODE' object with
     the corresponding value being the name of the local node to
     which it is bound.")

   (meta-node-references
    :initform (make-hash-table :test #'eq)
    :accessor meta-node-references
    :documentation
    "Set of meta-nodes of which there are instances in the
     meta-node's definition.")

   (output-nodes
    :initform (make-hash-table :test #'equal)
    :accessor output-nodes
    :documentation
    "Set (hash-table) of output nodes. Each KEY is the output node's
     name and the corresponding value is the output `NODE' object.")

   (definition
    :initarg :definition
    :initform nil
    :accessor definition
    :documentation
    "The graph corresponding to the body of the meta-node.")

   (instances
    :initform nil
    :accessor instances
    :documentation
    "List of instances of the meta-node. Each element is of the
     form (CONS NODE META-NODE) where NODE is the instance NODE itself
     and META-NODE is the META-NODE in which it appears, NIL if it is
     at global scope.")))


(defun meta-node? (x)
  "Returns true if X is a `meta-node'."
  (typep x 'meta-node))

(defun unique-node-name (hash-table prefix)
  "Generates a new unique node identifier. The identifier generated is
   a CONS with PREFIX being the CAR and the CDR being the current
   number of entries in HASH-TABLE."

  (cons prefix (hash-table-count hash-table)))

(defun outer-node-name (meta-node)
  "Generates a new name for a local node which will be used to
   reference an outer node."

  (unique-node-name (outer-nodes meta-node) 'ex))
