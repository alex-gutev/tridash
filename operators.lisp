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
  "Special operator for accessing sub-nodes of a node.")


;;; Meta-nodes

(defconstant +self-node+ (id-symbol "self")
  "Special node which references the current meta-node.")

(defconstant +out-operator+ (id-symbol "out")
  "Special operator for creating output nodes, from inside meta-node
   definitions.")

(defconstant +target-operator+ (id-symbol "target-node")
  "Special operator for specifying the name of the meta-node to use
   when binding the meta-node to its operands.")


;;; Conditionals

(defconstant +case-operator+ (id-symbol "case")
  "Case conditional operator.")


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

(defconstant +export-operator+ (id-symbol "export")
  "Operator for adding a node to the public nodes table of the current
   module.")


;;; Externally-Defined Meta-Nodes

(defconstant +extern-operator+ (id-symbol "extern")
  "Special operator for adding stubs for meta-nodes which are defined
   externally.")


;;; Infix Operators

(defconstant +infix-operators+
  (alist-hash-table
   `((,+bind-operator+ 10 :right)
     (,+def-operator+ 5 :right)
     (:open-paren 200)
     (,+subnode-operator+ 500 :left)))

  "Initial infix operator table.")
