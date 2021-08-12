;;;; package.lisp
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

(defpackage :tridash.interface
  (:use :generic-cl
        :alexandria
        :anaphora
        :optima)

  (:shadowing-import-from :generic-cl :emptyp)

  (:export
   ;; Frontend Interface
   :build-nodes-in-file
   :set-file-builder
   :define-file-builder
   :build-files

   ;; Backend Interface
   :compile-nodes

   ;; Errors
   :unknown-file-type)

  (:documentation
   "Package containing the interface to the frontend, backends and
    error reporting."))

(defpackage :tridash
  (:use :generic-cl
        :alexandria
        :anaphora
        :arrows
        :optima
        :optima.ppcre
        :iterate
        :split-sequence
        :cl-ppcre

        :tridash.util
        :tridash.parser
        :tridash.interface
        :tridash.frontend)

  (:shadowing-import-from :generic-cl
                          :emptyp
                          :multiply
                          :accumulate)

  (:documentation
   "Package containing the compiler application."))
