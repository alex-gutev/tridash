;;;; symbols.lisp
;;;;
;;;; Tridash Programming Language.
;;;; Copyright (C) 2019  Alexander Gutev
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

;;;; Runtime Library Function/Class Symbols

(in-package :tridash.backend.js)


;;;; Symbol Constants

(defconstant +tridash-namespace+ "Tridash"
  "Namespace containing the Tridash runtime library")

(defconstant +thunk-class+ (js-member +tridash-namespace+ "Thunk")
  "Thunk constructor function name.")

(defconstant +resolve-function+ (js-member +tridash-namespace+ "resolve")
  "Thunk resolve function name.")

(defconstant +node-ref-class+ (js-member +tridash-namespace+ "NodeRef")
  "Class representing a raw node reference object.")

(defconstant +node-interface-class+ (js-member +tridash-namespace+ "Node")
  "Node class type which servers as an interface to the node object.")

(defconstant +module-class+ (js-member +tridash-namespace+ "Module")
  "Tridash module class.")
