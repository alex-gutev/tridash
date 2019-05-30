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

(defpackage :tridash.builder.html
  (:use :generic-cl
        :alexandria
        :anaphora
        :cl-arrows
        :iterate
        :optima
        :cl-ppcre
        :named-readtables
        :trivial-gray-streams

        :tridash.util
        :tridash.parser
        :tridash.interface
        :tridash.frontend)

  (:shadowing-import-from :generic-cl
                          :emptyp
                          :accumulate
                          :multiply)

  (:import-from :let-over-lambda
                :mkstr
                :symb
                :lol-syntax)

  (:import-from :tridash.parser
                :lexer
                :make-lexer
                :next-token)

  (:import-from :tridash.frontend

                :+self-node+
                :ensure-node
                :remove-node
                :process-subnode

                :build-meta-node-graphs)

  (:export
   ;; HTML-NODE
   :html-node
   :tag-name
   :html-attribute
   :element-id

   ;; HTML-COMPONENT-NODE
   :html-component-node
   :element-node))
