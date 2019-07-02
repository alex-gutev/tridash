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

(defconstant +tridash-prefix+ (mkstr +tridash-namespace+ ".")
  "Namespace containing the Tridash runtime library definitions.")

(defconstant +node-class+ (mkstr +tridash-prefix+ "Node")
  "Runtime node class name.")

(defconstant +node-context-class+ (mkstr +tridash-prefix+ "NodeContext")
  "Runtime node context class name.")

(defconstant +end-update-class+ (mkstr +tridash-prefix+ "EndUpdate")
  "EndUpdate class name.")

(defconstant +thunk-class+ (mkstr +tridash-prefix+ "Thunk")
  "Thunk constructor function name.")

(defconstant +resolve-function+ (mkstr +tridash-prefix+ "resolve"))
