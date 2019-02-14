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

(define-constant +bind-operator+ (id-symbol "->")
  :documentation
  "Operator for establishing bindings between nodes.")

(define-constant +def-operator+ (id-symbol ":")
  :documentation
  "Operator for defining meta-nodes.")

(define-constant +op-operator+ (id-symbol ":op")
  :documentation
  "Operator for marking nodes as infix operators.")


;;; Subnodes and Outer Nodes.

(define-constant +outer-operator+ (id-symbol "..")
  :documentation
  "Special operator for referencing nodes defined in an outer scope.")

(define-constant +subnode-operator+ (id-symbol ".")
  :documentation
  "Special operator for accessing sub-nodes of a node.")


;;; Meta-nodes

(define-constant +self-node+ (id-symbol "self")
  :documentation
  "Special node which references the current meta-node.")


;;; Attributes

(define-constant +attribute-operator+ (id-symbol ":attribute")
  :documentation
  "Special operator for setting node attributes.")


;;; Conditionals

(define-constant +case-operator+ (id-symbol "case")
  :documentation
  "Case conditional operator.")


;;; Modules

(define-constant +module-operator+ (id-symbol ":module")
  :documentation
  "Operator which sets the current module.")

(define-constant +import-operator+ (id-symbol ":import")
  :documentation
  "Operator for importing nodes, from another module, into the current
   module.")

(define-constant +alias-operator+ (id-symbol ":alias")
  :documentation
  "Operator for adding an alias for a module to the current module.")

(define-constant +use-operator+ (id-symbol ":use")
  :documentation
  "Operator for using a module, using its name as an alias, from the
   current module.")

(define-constant +export-operator+ (id-symbol ":export")
  :documentation
  "Operator for adding a node to the public nodes table of the current
   module.")

(define-constant +in-module-operator+ (id-symbol ":in")
  :documentation
  "Operator for referencing a node in another module which does not
   have an alias in the current module.")


;;; Externally-Defined Meta-Nodes

(define-constant +extern-operator+ (id-symbol ":extern")
  :documentation
  "Special operator for adding stubs for meta-nodes which are defined
   externally.")


(define-constant +special-operators+
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

  :test #'equal
  :documentation "List of all special operators")


;;; Infix Operators

(defparameter +infix-operators+
  (alist-hash-table
   `((,+bind-operator+ 10 :right)
     (,+def-operator+ 5 :right)
     (:open-paren 900)
     (,+subnode-operator+ 1000 :left)))

  "Initial infix operator table.")
