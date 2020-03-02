;;;; package.lisp
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

(defpackage :tridash.backend.wasm
  (:use :generic-cl
        :alexandria
        :anaphora
        :named-readtables
        :optima
        :optima.ppcre
        :iterate
        :cl-ppcre
        :cl-arrows

        :babel
        :babel-encodings

        :tridash.util
        :tridash.parser
        :tridash.interface
        :tridash.frontend
        :tridash.frontend.strictness)

  (:import-from :tridash.frontend.strictness
                :strict-arguments
                :strict-outer-operands)

  (:shadowing-import-from :generic-cl
                          :emptyp
                          :multiply
                          :accumulate)

  (:shadowing-import-from :generic-cl.util
                          :repeat)

  (:import-from :let-over-lambda
                :mkstr
                :symb
                :lol-syntax)

  (:import-from :agutil
                :update-let
                :nlet)

  (:documentation "WebAssembly Backend."))
