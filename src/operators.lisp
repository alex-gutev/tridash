;;;; operators.lisp
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

(in-package :tridash.frontend)

;;;; Special Operator Constants

;;; Basic Operators

(defconstant +bind-operator+ :bind
  "Operator for establishing bindings between nodes.")

(defconstant +op-operator+ (id-symbol "/operator")
  "Operator for marking nodes as infix operators.")

(defconstant +quote-operator+ (id-symbol "/quote")
  "Operator for suppressing processing of declarations.")

(defconstant +ref-operator+ (id-symbol :ref)
  "Operator for referencing another node without creating a binding.")


;;; Contexts

(defconstant +context-operator+ (id-symbol "/context")
  "Operator for explicitly specifying the context to which bindings
   are established.")

(defconstant +state-operator+ (id-symbol "/state")
  "Operator for establishing a binding between different states of a
   node.")


;;; Subnodes and Outer Nodes.

(defconstant +outer-operator+ :outer
  "Special operator for referencing nodes defined in an outer scope.")

(defconstant +subnode-operator+ :subnode
  "Special operator for accessing sub-nodes of a node.")


;;; Meta-nodes

(defconstant +def-operator+ :define
  "Operator for defining meta-nodes.")

(defconstant +extern-operator+ (id-symbol "/external")
  "Special operator for adding stubs for meta-nodes which are defined
   externally.")

(defconstant +self-node+ (id-symbol "self")
  "Special node which references the current meta-node.")


;;; Attributes

(defconstant +attribute-operator+ (id-symbol "/attribute")
  "Special operator for setting node attributes.")

(defconstant +attribute-processor-operator+ (id-symbol "/attribute-processor")
  "Special operator for setting a meta-node to process a particular
   attribute.")


;;; Modules

(defconstant +module-operator+ (id-symbol "/module")
  "Operator which sets the current module.")

(defconstant +import-operator+ (id-symbol "/import")
  "Operator for importing nodes, from another module, into the current
   module.")

(defconstant +alias-operator+ (id-symbol "/use-as")
  "Operator for adding an alias for a module to the current module.")

(defconstant +use-operator+ (id-symbol "/use")
  "Operator for using a module, using its name as an alias, from the
   current module.")

(defconstant +export-operator+ (id-symbol "/export")
  "Operator for adding a node to the public nodes table of the current
   module.")

(defconstant +in-module-operator+ (id-symbol "/in")
  "Operator for referencing a node in another module which does not
   have an alias in the current module.")


(defconstant +special-operators+
  (list +bind-operator+
        +context-operator+

        +def-operator+
        +extern-operator+
        +op-operator+
        +outer-operator+
        +subnode-operator+
        +attribute-operator+
        +module-operator+
        +import-operator+
        +use-operator+
        +export-operator+
        +in-module-operator+
        +list-operator+

        +quote-operator+)

  "List of all special operators")


;;; Infix Operators

(defconstant +infix-operators+
  (alist-hash-map
   `((:open-paren 900)))

  "Initial infix operator table.")
