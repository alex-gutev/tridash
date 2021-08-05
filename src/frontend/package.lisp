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

(defpackage :tridash.ir
  (:use :generic-cl)

  (:documentation
   "Contains the symbols defining the intermediate representation."))

(defpackage :tridash.frontend
  (:use :generic-cl
        :anaphora
        :arrows
        :optima

        :tridash.util
        :tridash.parser)

  (:local-nicknames
   (:ax :alexandria)
   (:ir :tridash.ir)))
