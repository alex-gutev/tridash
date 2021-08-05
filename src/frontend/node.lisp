;;;; node.lisp
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

;;;; Base `node' class definition.

(in-package :tridash.frontend)

;;; NODE Object Definition

(defclass node ()
  ((name
    :initarg :name
    :accessor name
    :documentation "The node's identifier name.")

   (dependencies
    :initform (make-hash-map)
    :initarg :dependencies
    :accessor dependencies
    :documentation
    "Map of the node's dependency nodes, i.e. the nodes on which this
     node's value depends. Each key is a `NODE' object and the
     corresponding value is the `IR:LINK' object.")

   (observers
    :initform (make-hash-map)
    :initarg :observers
    :accessor observers
    :documentation
    "Map of the node's observer nodes, i.e. the nodes whose values are
     dependent on the value of this node. Each key is a `NODE' object
     and the corresponding value is the `IR:LINK' object of this
     node in the observer node's DEPENDENCIES map.")

   (contexts
    :accessor contexts
    :initform (make-hash-map)
    :documentation
    "Map of the node's contexts. Each key is a context identifier and
     the corresponding value is the `NODE-CONTEXT' object.")

   (attributes
    :accessor attributes
    :initform (make-hash-map :test #'cl:equalp)
    :documentation
    "Map of the node's attributes (miscellaneous key-value pairs)."))

  (:documentation
   "Base node class. Stores the binding information about a node,
    i.e. the node's dependencies and observers, and the node's
    contexts."))

;;; Node Context Definition

(defclass node-context ()
  ((operands
    :initarg :operands
    :initform (make-hash-map)
    :accessor operands
    :documentation
    "Map containing the dependency nodes which are operands to this
     context's value function. Each key is the dependency `NODE'
     object and the corresponding value is the `IR:LINK' object.")

   (value-function
    :initarg :value-function
    :initform nil
    :accessor value-function
    :documentation
    "The context's value function."))

  (:documentation
   "Node context. A node context stores information about the node's
    value is computed at a particular moment (context)."))


;;; Hash Function

(defmethod hash ((node node))
  (hash (name node)))

;;; Utility Functions

(defun attribute (attribute node &optional default)
  "Retrieves a node attribute. ATTRIBUTE is converted to a string if
   it is a symbol. If NODE does not have such an attribute, DEFAULT is
   returned."

  (get (string attribute) (attributes node) default))

(defun (setf attribute) (value attribute node)
  "Sets the value of a node attribute. ATTRIBUTE is converted to a
   string if it is a symbol."

  (setf (get (string attribute) (attributes node)) value))

(defun home-module (node)
  "Returns the module in which NODE was originally declared."

  (get :module (attributes node)))

(defun bool-value (value)
  "Converts value to a boolean. 0 and NIL are treated as boolean
   false, everything else is treated as boolean true."

  (match value
    ((eql 1) t)

    ((eql 't) t)

    ((cl:equalp "true")
     t)

    ((type symbol)
     (likep (symbol-name value) "true"))))
