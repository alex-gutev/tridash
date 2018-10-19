;;;; primitives.lisp
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

;;;; Language Primitives.

(in-package :metalink.frontend)

(defparameter *primitive-ops*
  (alist-hash-table
   (list
    (cons (id-symbol "+") '+)
    (cons (id-symbol "-") '-)
    (cons (id-symbol "*") '*)
    (cons (id-symbol "/") '/)

    (cons (id-symbol "<") '<)
    (cons (id-symbol ">") '>)
    (cons (id-symbol "<=") '<=)
    (cons (id-symbol ">=") '>=)
    (cons (id-symbol "=") '=)
    (cons (id-symbol "!=") '!=)

    (cons (id-symbol "and") 'and)
    (cons (id-symbol "or") 'or)
    (cons (id-symbol "not") 'not)

    ;; Types

    (cons (id-symbol "int") '(:type :integer))
    (cons (id-symbol "real") '(:type  :real))))

  "Hash-table of primitive operators where each key is an identifier
   symbol in the METALINK.SYMBOLS package and the corresponding key is
   a symbol in the current package, identifying the primitive.")
