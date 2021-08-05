;;;; package.lisp
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

(defpackage :tridash.parser
  (:use :generic-cl
        :alexandria
        :anaphora
        :arrows
        :iterate
        :parse-number
        :optima
        :optima.ppcre)

  (:shadowing-import-from :generic-cl
                          :emptyp
                          :accumulate
                          :multiply)

  (:import-from :lol
                :defmacro!)

  (:export
   :+list-operator+
   :*current-source-file*

   ;; Declaration Types

   :node-declaration
   :node-declaration-location
   :node-declaration-p

   :atom-node
   :atom-node-identifier
   :atom-node-p

   :literal-node
   :literal-node-value
   :literal-node-p

   :functor-node
   :functor-node-operator
   :functor-node-operands
   :functor-node-p

   ;; Parser

   :make-parser
   :add-operator

   ;; Identifiers

   :node-path->name
   :id-symbol

   :symbol-mapping
   :symbol-mappings)

  (:documentation
   "Package containing the parser and lexer."))
