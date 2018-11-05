;;;; builder.lisp
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

(defconstant +bind-operator+ (id-symbol "->")
  "Operator for establishing bindings between nodes.")

(defconstant +def-operator+ (id-symbol ":")
  "Operator for defining meta-nodes.")

(defconstant +macro-operator+ (id-symbol "macro")
  "Operator for marking meta-nodes to be expanded at compile-time.")

(defconstant +op-operator+ (id-symbol "op")
  "Operator for marking nodes as infix operators.")


;;; Subnodes and Outer Nodes.

(defconstant +outer-operator+ (id-symbol "..")
  "Special operator for referencing nodes defined in an outer scope.")

(defconstant +subnode-operator+ (id-symbol ".")
  "Special operator for accessing meta-node output nodes from outside
   the meta-node.")


;;; Meta-nodes

(defconstant +self-node+ (id-symbol "self")
  "Special node representing the value of the current meta-node.")

(defconstant +out-operator+ (id-symbol "out")
  "Special operator for creating output nodes, from meta-nodes.")


;;; Conditionals

(defconstant +case-operator+ (id-symbol "case")
  "Case operator.")


;;; Modules

(defconstant +module-operator+ (id-symbol "module")
  "Operator which sets the current module.")

(defconstant +import-operator+ (id-symbol "import")
  "Operator for importing nodes, from another module, into the current
   module.")

(defconstant +alias-operator+ (id-symbol "alias")
  "Operator for adding an alias for a module to the current module.")

(defconstant +use-operator+ (id-symbol "use")
  "Operator for using a module, using its name as an alias, from the
   current module.")


;;; Infix Operators

(defconstant +infix-operators+
  (alist-hash-table
   `((,+bind-operator+ 10 right)
     (,+def-operator+ 5 right)
     (:open-paren 200)
     (,+subnode-operator+ 500)))

  "Initial infix operator table.")
