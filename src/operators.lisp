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

(defconstant +bind-operator+ (id-symbol "->")
  "Operator for establishing bindings between nodes.")

(defconstant +def-operator+ (id-symbol ":")
  "Operator for defining meta-nodes.")

(defconstant +op-operator+ (id-symbol ":op")
  "Operator for marking nodes as infix operators.")


;;; Subnodes and Outer Nodes.

(defconstant +outer-operator+ (id-symbol "..")
  "Special operator for referencing nodes defined in an outer scope.")

(defconstant +subnode-operator+ (id-symbol ".")
  "Special operator for accessing sub-nodes of a node.")


;;; Meta-nodes

(defconstant +self-node+ (id-symbol "self")
  "Special node which references the current meta-node.")


;;; Attributes

(defconstant +attribute-operator+ (id-symbol ":attribute")
  "Special operator for setting node attributes.")


;;; Conditionals

(defconstant +case-operator+ (id-symbol "case")
  "Case conditional operator.")


;;; Modules

(defconstant +module-operator+ (id-symbol ":module")
  "Operator which sets the current module.")

(defconstant +import-operator+ (id-symbol ":import")
  "Operator for importing nodes, from another module, into the current
   module.")

(defconstant +alias-operator+ (id-symbol ":alias")
  "Operator for adding an alias for a module to the current module.")

(defconstant +use-operator+ (id-symbol ":use")
  "Operator for using a module, using its name as an alias, from the
   current module.")

(defconstant +export-operator+ (id-symbol ":export")
  "Operator for adding a node to the public nodes table of the current
   module.")

(defconstant +in-module-operator+ (id-symbol ":in")
  "Operator for referencing a node in another module which does not
   have an alias in the current module.")


;;; Externally-Defined Meta-Nodes

(defconstant +extern-operator+ (id-symbol ":extern")
  "Special operator for adding stubs for meta-nodes which are defined
   externally.")


(defconstant +special-operators+
    (list +bind-operator+
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
          +list-operator+)

  "List of all special operators")


;;; Infix Operators

(defconstant +infix-operators+
  (alist-hash-map
   `((,+bind-operator+ 10 :right)
     (,+def-operator+ 5 :right)
     (:open-paren 900)
     (,+subnode-operator+ 1000 :left)))

  "Initial infix operator table.")
