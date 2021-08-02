;;;; expressions.lisp
;;;;
;;;; Tridash Programming Language.
;;;; Copyright (C) 2019-2021  Alexander Gutev
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
  ()

  (:documentation
   "Base class representing a computation node in the intermediate
    representation."))

(defclass link (node)
  ((node
    :initarg :node
    :accessor node
    :documentation
    "Linked node.")

   (context
    :initarg :context
    :accessor context
    :documentation
    "Identifier of the context of the binding.")

   (two-way-p
    :initarg :two-way-p
    :initform nil
    :accessor two-way-p
    :documentation
    "Flag for whether the binding is two way.")

   (weak-p
    :initarg :weak-p
    :initform nil
    :accessor weak-p
    :documentation
    "Deprecated flag for whether changes in the source node's state
     trigger updates in the target node's state."))

  (:documentation
   "Represents a reference to the value of another node."))

(defun link (node &key context two-way-p)
  (make-instance 'link
                 :node node
                 :context context
                 :two-way-p two-way-p))

(defclass node-ref (node)
  ((node
    :initarg :node
    :accessor node
    :documentation
    "The reference node."))

  (:documentation
   "Represents a reference to a raw node object."))

(defun node-ref (node)
  (make-instance 'node-ref :node node))


(defclass functor (node)
  ((operator
    :initarg :operator
    :accessor operator
    :documentation
    "Functor operator.")

   (operands
    :initarg :operands
    :accessor operands
    :documentation
    "Functor operands")

   (outer-nodes
    :initarg :outer-nodes
    :accessor outer-nodes
    :documentation
    "Outer node references."))

  (:documentation
   "Represents an application of an operator on a number of
    arguments."))

(defun functor (operator operands &key (outer-nodes (make-hash-map)))
  (make-instance 'functor
                 :operator operator
                 :operands operands
                 :outer-nodes outer-nodes))

(defclass object (node)
  ((entries
    :initarg :entries
    :accessor entries
    :documentation
    "Object entries list. Each element is a LIST of the form (KEY
     VALUE) where KEY is a symbol naming the key and VALUE is a `NODE'
     representing the value."))

  (:documentation
   "Represents an object node with one or more subnodes."))

(defun object (&optional entries)
  (make-instance 'object :entries entries))


(defclass expression-block (node)
  ((expression :initarg :expression
               :accessor expression)

   (use-count :initarg :count
          :initform 1
          :accessor use-count)))

(defun expression-block (expression &key (count 1))
  (make-instance 'expression-block
                 :expression expression
                 :count count))

(defclass cyclic-reference (node)
  ((expression
    :initarg :expression
    :accessor expression
    :documentation
    "The reference expression."))

  (:documentation
   "Represents a cyclic reference."))

(defun cyclic-reference (expression)
  (make-instance 'cyclic-reference :expression expression))


(defclass meta-node-ref (node)
  ((meta-node
    :initarg :meta-node
    :accessor meta-node
    :documentation
    "The reference meta-node.")

   (optional-arguments
    :initarg :optional-arguments
    :initform nil
    :accessor optional-arguments
    :documentation
    "List of nodes computing the values of the meta-node's default
     arguments.")

   (outer-nodes
    :initarg :outer-nodes
    :initform (make-hash-map)
    :accessor outer-nodes
    :documentation
    "Map containing the meta-node's outer-node references."))

  (:documentation
   "Represents a reference to a meta-node."))

(defun meta-node-ref (meta-node &key optional (outer-nodes (make-hash-map)))
  (make-instance 'meta-node-ref
                 :meta-node meta-node
                 :optional-arguments optional
                 :outer-nodes outer-nodes))

(defclass argument-list (node)
  ((arguments
    :initarg :arguments
    :accessor arguments
    :documentation
    "List of arguments"))

  (:documentation
   "Represents a rest argument list."))

(defun argument-list (arguments)
  (make-instance 'argument-list :arguments arguments))
