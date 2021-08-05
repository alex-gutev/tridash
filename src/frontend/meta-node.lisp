;;;; meta-node.lisp
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

;;;; `meta-node' class definition.

(in-package :tridash.frontend)

;;; Meta-Node Class Definitions

(defclass meta-node (node)
  ()

  (:documentation
   "Base class representing a meta-node."))

(defclass meta-node-spec (meta-node)
  ((operands
    :initarg :operands
    :initform nil
    :accessor operands
    :documentation
    "`OPERAND-SPEC' object describing the meta-node's operands.")

   (definition-spec
    :initarg :definition
    :initform nil
    :accessor definition-spec
    :documentation
    "List of parsed expressions comprising the meta-node's
     definition."))

  (:documentation
   "Represents a meta-node which has not been built yet. Contains all
    the information necessary for compiling the meta-node."))

(defclass built-meta-node (meta-node)
  ((operand-nodes
    :initarg :operand-nodes
    :initform nil
    :accessor operand-nodes
    :documentation
    "List containing the local nodes to which the values of the
     arguments are bound.")

   (outer-nodes
    :initform (make-hash-map)
    :accessor outer-nodes
    :documentation
    "Map mapping nodes, declared in an outer node table, to the
     corresponding local nodes within the definition.")

   (meta-node-references
    :initform nil
    :accessor meta-node-references
    :documentation
    "Set of meta-nodes used within the definition.")

   (definition
    :initarg :definition
    :initform nil
    :accessor definition
    :documentation
    "A `MODULE' storing the nodes comprising the definition."))

  (:documentation
   "Represents a meta-node which has been built, however the final
    processing steps have not been performed yet."))

(defclass final-meta-node (built-meta-node)
  ()

  (:documentation
   "Represents a meta-node which has been built with all the final
    processing steps performed."))

(defclass external-meta-node (meta-node)
  ((operands
    :initarg :operands
    :initform nil
    :accessor operands
    :documentation
    "`OPERAND-SPEC' object describing the meta-node's operands."))

  (:documentation
   "Represents a meta-node which is defined externally, i.e. the
    source for it is not available."))

;;; Operand Specifiers

(defclass operand-spec ()
  ((required
    :initarg :required
    :accessor required
    :documentation
    "List of the names of the required arguments.")

   (optional
    :initarg :optional
    :accessor optional
    :documentation
    "List of the optional arguments. Each element is of the form (NAME
    . VALUE) where NAME is the name of the argument, and VALUE is an
    expression computing the default value.")

   (rest-argument
    :initarg :rest-argument
    :accessor rest-argument
    :documentation
    "The name of the argument receiving the remaining arguments
     following the last required/optional argument. If NIL then the
     meta-node does not accept more arguments than the number of
     required and optional arguments."))

  (:documentation
   "Describes the structure of the operands of a meta-node."))
