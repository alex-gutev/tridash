;;;; tridash.asd
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

(asdf:defsystem #:tridash
  :description "Tridash Programming Language"
  :author "Alexander Gutev"
  :license "GPL v3"
  :serial t
  :components ((:module "util"
                        :serial t
                        :components ((:file "package")
                                     (:file "cut")
                                     (:file "macros")
                                     (:file "misc")))
               (:file "package")
               (:file "interface")
               (:file "lexer")
               (:file "parser")
               (:file "operators")
               (:file "conditions")
               (:file "node")
               (:file "meta-node")
               (:file "primitives")
               (:file "node-table")
               (:file "modules")
               (:file "outer-nodes")
               (:file "builder")
               (:file "coalescer")
               (:file "prog-builder")
               (:file "main")

               (:module "builders/html"
                        :serial t
                        :components ((:file "package")
                                     (:file "builder")))

               (:module "backends/javascript"
                        :serial t
                        :components ((:file "package")
                                     (:file "ast")
                                     (:file "print")
                                     (:file "analyze")
                                     (:file "backend")
                                     (:file "functions")
                                     (:file "html"))))

  :depends-on (:graylex
               :anaphora
               :iterate
               :alexandria
               :cl-arrows
               :collectors
               :named-readtables
               :let-over-lambda
               :optima
               :optima.ppcre
               :plump
               :cl-ppcre
               :ppath
               :parse-number
               :cl-yaml
               :cl-fad
               :osicat))
