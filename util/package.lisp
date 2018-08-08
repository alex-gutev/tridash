;;;; package.lisp
;;;;
;;;; Metalink Programming Language.
;;;; Copyright (C) 2017, Alexander Gutev
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

(defpackage :metalink.util
  (:use :common-lisp
        :anaphora
        :iterate
        :named-readtables)

  (:import-from :let-over-lambda
                :symb
                :defmacro!
                :lol-syntax)

  (:import-from :alexandria
                :ensure-gethash)

  (:export
   ;; Cut (Reader) Macro
   :cut
   :cut-syntax

   ;; Macros
   :let-if
   :let*-if
   :dohash

   ;; Functions
   :same-length?
   :adjoin-hash
   :in-hash?
   :merge-hash
   :partition)

  (:documentation
   "A collection of utility functions and macros."))
