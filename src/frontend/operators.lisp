;;;; operators.lisp
;;;;
;;;; Tridash Programming Language.
;;;; Copyright (C) 2021  Alexander Gutev
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

;;;; Special Expression Types

(defstruct (special-expression (:include functor-expression))
  "Represents a special expression, which is a functor expression of a
   special operator.")

;;; Basic Operators

(defconstant +bind-operator+ (id-symbol "->"))

(defstruct (bind-expression (:include special-expression))
  "Represents a binding declaration between two nodes.")


(defconstant +quote-operator+ (id-symbol "/quote"))

(defstruct (quote-expression (:include special-expression))
  "Represents a quote expression.")


(defconstant +ref-operator+ (id-symbol "&"))

(defstruct (ref-expression (:include special-expression))
  "Represents a raw node reference expression.")


(defconstant +register-operator-operator+ (id-symbol "/operator"))

(defstruct (register-operator-expression (:include special-expression))
  "Represents a register infix/prefix operator declaration.")


;;; Contexts

(defconstant +context-operator+ (id-symbol "/context"))

(defstruct (context-expression (:include special-expression))
  "Represents an explicit node context expression.")


(defconstant +old-value-operator+ (id-symbol "old"))

(defstruct (old-value-expression (:include special-expression))
  "Represents a reference to a node's previous value.")


;;; Subnodes and Outer Nodes.

(defconstant +outer-node-operator+ (id-symbol ".."))

(defstruct (outer-node-expression (:include special-expression))
  "Represents a reference to node defined in an outer scope.")


(defconstant +subnode-operator+ (id-symbol "."))

(defstruct (subnode-expression (:include special-expression))
  "Represents a reference to a subnode of a node.")


;;; Meta-nodes

(defconstant +define-meta-node-operator+ (id-symbol ":"))

(defstruct (define-meta-node-expression (:include special-expression))
  "Represents a meta-node definition.")


(defconstant +external-meta-node-operator+ (id-symbol "/external"))

(defstruct (external-meta-node-expression (:include special-expression))
  "Represents an external meta-node declaration.")


(defconstant +self-node+ (id-symbol "self")
  "Identifier of the special node which references the current
   meta-node.")


;;; Attributes

(defconstant +attribute-operator+ (id-symbol "/attribute"))

(defstruct (attribute-expression (:include special-expression))
  "Represents a node attribute declaration.")


(defconstant +attribute-processor-operator+ (id-symbol "/attribute-processor"))

(defstruct (attribute-processor-expression (:include special-expression))
  "Represents an attribute processor declaration.")


;;; Modules

(defconstant +module-operator+ (id-symbol "/module"))

(defstruct (module-expression (:include special-expression))
  "Represents a module declaration.")


(defconstant +import-operator+ (id-symbol "/import"))

(defstruct (import-expression (:include special-expression))
  "Represents an import node declaration.")


(defconstant +alias-operator+ (id-symbol "/use-as"))

(defstruct (alias-expression (:include special-expression))
  "Represents an alias module declaration.")


(defconstant +export-operator+ (id-symbol "/export"))

(defstruct (export-expression (:include special-expression))
  "Represents an export node declaration.")


(defconstant +in-module-operator+ (id-symbol "/in"))

(defstruct (in-module-expression (:include special-expression))
  "Represents a reference to a node in another module.")


;;; Infix Operators

(defconstant +infix-operators+
  (alist-hash-map
   `((:open-paren 900)))

  "Initial infix operator table.")
