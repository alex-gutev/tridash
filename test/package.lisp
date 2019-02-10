;;;; package.lisp
;;;;
;;;; Tridash Programming Language.
;;;; Copyright (C) 2018-2019  Alexander Gutev
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

(defpackage :tridash.test
  (:use :common-lisp
        :alexandria
        :anaphora
        :cl-arrows
        :iterate
        :optima
        :prove

        :tridash.interface
        :tridash.parser
        :tridash.frontend
        :tridash

        :tridash.backend.js)

  (:shadowing-import-from :prove :fail)

  (:import-from :lol :defmacro!)

  (:import-from
   :tridash.parser

   :*line-term*
   :make-lexer
   :next-token

   :tridash-parse-error)

  (:import-from
   :tridash.frontend

   :+infix-operators+))

(prove:plan nil)
(prove:finalize)
