;;;; metalink.asd
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

(asdf:defsystem #:metalink
  :description "Metalink Programming Language"
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
               (:file "lexer")
               (:file "parser")
               (:file "node")
               (:file "meta-node")
               (:file "primitives")
               (:file "node-table")
               (:file "outer-nodes")
               (:file "two-way")
               (:file "builder")
               (:file "wait-set")
               (:file "coalescer")
               (:file "interface")

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
               :collectors
               :named-readtables
               :let-over-lambda
               :optima
               :plump
               :cl-ppcre))
