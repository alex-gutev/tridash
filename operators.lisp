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

(defconstant +bind-operator+ (id-symbol "->"))

(defconstant +outer-operator+ (id-symbol "..")
  "Special operator for referencing nodes defined in an outer scope.")

(defconstant +out-operator+ (id-symbol "out")
  "Special operator for creating output nodes, from meta-nodes.")

(defconstant +subnode-operator+ (id-symbol ".")
  "Special operator for accessing meta-node output nodes from outside
   the meta-node.")

(defconstant +self-node+ (id-symbol "self")
  "Special node representing the value of the current meta-node.")

(defconstant +case-operator+ (id-symbol "case")
  "Case operator.")

(defconstant +module-operator+ (id-symbol "module")
  "Operator which sets the current module.")

(defconstant +import-operator+ (id-symbol "import"))

(defconstant +alias-operator+ (id-symbol "alias"))

(defconstant +use-operator+ (id-symbol "use"))
