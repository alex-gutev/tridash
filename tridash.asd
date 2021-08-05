;;;; tridash.asd
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

(asdf:defsystem #:tridash
  :description "Tridash Programming Language"
  :author "Alexander Gutev"
  :license "GPL v3"
  :components ((:module
                "src"

                :components
                ((:module "util"
                          :serial t
                          :components ((:file "package")
                                       (:file "cut")
                                       (:file "macros")))

                 (:module
                  "parser"
                  :serial t
                  :components
                  ((:file "package")
                   (:file "lexer")
                   (:file "parser")))

                 (:module
                  "frontend"
                  :serial t
                  :components
                  ((:file "package")
                   (:file "ir")
                   (:file "node")))
                 )))

  :depends-on (:anaphora
               :iterate
               :alexandria
               :arrows
               :split-sequence
               :named-readtables
               :let-over-lambda
               :optima
               :optima.ppcre
               :cl-ppcre
               :ppath
               :parse-number
	       :cl-yaclyaml
               :cl-fad
               :unix-opts
               :trivial-gray-streams
               :flexi-streams
               :babel

               :generic-cl
               :generic-cl.util
               :wasm-encoder)

  :in-order-to ((asdf:test-op (asdf:test-op :tridash/test)))

  :build-operation "program-op"
  :build-pathname "tridashc"
  :entry-point "TRIDASH::MAIN")

(asdf:defsystem #:tridash/test
  :description "Units tests for the Tridash."
  :author "Alexander Gutev"
  :license "GPL v3"
  :depends-on (:tridash
               :fiveam
               :cl-interpol)

  :components ((:module
                "test"
                :serial t
                :components
                ((:file "test")
                 (:file "util")
                 (:file "lexer")
                 (:file "parser")
                 (:file "frontend")
                 (:file "coalescer")
                 (:file "constant-folding")
                 (:file "structure-checks")
                 (:file "meta-nodes")
                 (:file "strictness")
                 (:file "macros")
                 (:file "macros-core-module")

                 (:module
                  "builders/html"

                  :components
                  ((:file "builder")))

                 (:module
                  "backends/javascript"

                  :components
                  ((:file "backend")))

                 (:module
                  "backends/wasm"

                  :components
                  ((:file "backend"))))))

  :perform (asdf:test-op :after (op c)
                         (uiop:symbol-call :tridash/test :test-tridash)))
