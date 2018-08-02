;;;; package.lisp
;;;;
;;;; Metalink Programming Language.
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

(defpackage :metalink.symbols
  (:use)
  (:export "->" ":" "macro" "op")

  (:documentation
   "The package where symbols, read from source files, are
    interned. The package does not use any other package."))

(defpackage :metalink.parser
  (:use :common-lisp
        :alexandria
        :anaphora
        :iterate
        :graylex
        :metalink.symbols)

  (:export
   :*operator-nodes*

   :+bind-operator+
   :+def-operator+
   :+macro-operator+
   :+op-operator+
   :+list-operator+

   :make-parser
   :add-operator
   :id-symbol

   :left
   :right)

  (:documentation
   "Package containing the parser and lexer."))
