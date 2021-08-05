;;;; ir.lisp
;;;;
;;;; Tridash Programming Language.
;;;; Copyright (C) 2021  Alexander Gutev <alex.gutev@mail.bg>
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

;;;; Intermediate Representation

(in-package :tridash.ir)

(defclass node ()
  ((ref-count
    :initarg :ref-count
    :initform 1
    :accessor ref-count

    :documentation
    "Number of times the value of this node is referenced."))

  (:documentation
   "Represents a node which computes a value."))

(defclass constant-value (node)
  ((value
    :initarg :value
    :accessor value))

  (:documentation
   "Represents a constant (literal) value."))

(defclass none (node)
  ()

  (:documentation
   "Represents a node which does not evaluate to a value."))

(defclass link (node)
  ((node
    :initarg :node
    :accessor node
    :documentation
    "The referenced node."))

  (:documentation
   "Represents a node which references the value of another node."))

(defclass node-ref (node)
  ((node
    :initarg :node
    :accessor node))

  (:documentation
   "Represents a reference to a raw node object, rather than the
     value of a node."))


(defclass functor (node)
  ((operator
    :initarg :operator
    :accessor operator
    :documentation
    "The operator being applied.")

   (operands
    :initarg :operands
    :accessor operands
    :documentation "The arguments the operator is being applied on.")

   (outer-nodes
    :initarg :outer-nodes
    :initform (make-hash-map)
    :accessor outer-nodes
    :documentation
    "Map mapping nodes, referenced internally by the meta-nodes, to
     the IR nodes which compute their values."))

  (:documentation
   "Represents the application of a functor on a number of
    arguments."))

(defclass meta-node-ref (node)
  ((meta-node
    :initarg :meta-node
    :accessor meta-node
    :documentation
    "The referenced meta-node.")

   (optional-arguments
    :initarg :optional-arguments
    :accessor optional-arguments
    :documentation
    "List of IR nodes computing the default values of the optional arguments.")

   (outer-nodes
    :initarg :outer-nodes
    :accessor outer-nodes
    :documentation
    "Map mapping nodes, referenced internally by the meta-nodes, to
     the IR nodes which compute their values."))

  (:documentation
   "Represents a reference to a meta-node."))

(defclass object (node)
  ((properties
    :initarg :entries
    :accessor properties
    :documentation
    "The list of the object's properties. Each element of of the
     form (KEY . VALUE) where KEY is a symbol identifying the property
     and VALUE is the IR node which computes the property's value."))

  (:documentation
   "Represents the creation of an object node."))

(defclass argument-list (node)
  ((arguments
    :initarg :arguments
    :initform nil
    :accessor arguments))

  (:documentation
   "Represents a rest argument list."))
