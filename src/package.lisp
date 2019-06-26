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

(defpackage :tridash.symbols
  (:use)

  (:documentation
   "The package where symbols, read from source files, are
    interned. The package does not use any other package."))

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

(defpackage :tridash.parser
  (:use :generic-cl
        :alexandria
        :anaphora
        :cl-arrows
        :iterate
        :parse-number
        :optima
        :optima.ppcre

        :tridash.interface)

  (:shadowing-import-from :generic-cl
                          :emptyp
                          :accumulate
                          :multiply)

  (:import-from :lol
                :defmacro!)

  (:export
   :+list-operator+
   :*current-source-file*

   :make-parser
   :add-operator

   :node-path->name
   :id-symbol

   :symbol-mapping
   :symbol-mappings)

  (:documentation
   "Package containing the parser and lexer."))

(defpackage :tridash.frontend
  (:use :generic-cl
	:alexandria
	:anaphora
	:iterate
	:named-readtables
	:optima
        :cl-arrows

        :tridash.util
	:tridash.parser
        :tridash.interface)

  (:shadowing-import-from :generic-cl
                          :emptyp
                          :accumulate
                          :multiply)

  (:import-from :let-over-lambda
		:lol-syntax
		:symb
                :mkstr
		:defmacro!)

  (:export
   ;; Operators
   :+subnode-operator+

   ;; Node
   :node
   :name
   :dependencies
   :observers
   :contexts
   :attributes
   :attribute

   ;; Node Contexts
   :node-context
   :operands
   :value-function
   :context

   ;; Predicates
   :node?
   :value?
   :input-node?
   :bool-value

   ;; Intermediate Expressions
   :node-link
   :node-link-node
   :node-link-context
   :node-link-p
   :self

   :functor-expression
   :functor-expression-p
   :functor-expression-meta-node
   :functor-expression-arguments

   :if-expression
   :if-expression-p
   :if-expression-condition
   :if-expression-then
   :if-expression-else

   :object-expression
   :object-expression-p
   :object-expression-entries

   :member-expression
   :member-expression-p
   :member-expression-object
   :member-expression-key

   :catch-expression
   :catch-expression-p
   :catch-expression-main
   :catch-expression-catch

   :fail-expression
   :fail-expression-p
   :fail-expression-type

   :expression-group
   :expression-group-p
   :expression-group-expression
   :expression-group-count
   :expression-group-save

   :meta-node-ref
   :meta-node-ref-p
   :meta-node-ref-node
   :meta-node-ref-outer-nodes

   :walk-expression
   :transform-expression

   ;; Utility Functions
   :observer-list
   :dependency-list

   :observers-count
   :dependencies-count

   ;; Meta-node
   :meta-node
   :external-meta-node

   :operands
   :definition

   :meta-node?
   :external-meta-node?

   ;; Operators
   :+bind-operator+

   ;; module
   :module
   :flat-node-table

   :outer-module
   :depth
   :nodes
   :meta-nodes
   :public-nodes
   :operator-nodes
   :input-nodes

   :add-node
   :add-input

   ;; Module Table
   :module-table
   :modules
   :current-module
   :get-module
   :ensure-module
   :home-module
   :*global-module-table*

   ;; Builder
   :build-parsed-nodes
   :build-node
   :finish-build-graph

   :build-program
   :build-source-file

   ;; Error Conditions
   :semantic-error
   :declaration-stack
   :module-table

   :not-node-error
   :module-node-reference-error

   :non-node-operator-error
   :operator

   :non-existent-node
   :module-name

   :node-exists-error
   :meta-node-name-collision
   :special-operator-name-error

   :non-existent-module
   :alias-clash-error
   :alias-taken-error

   :import-node-error

   :invalid-arguments-error
   :operator
   :arguments
   :expected

   :invalid-value-error
   :thing
   :allowed
   :value

   :global-outer-reference-error
   :special-operator-operand

   :meta-node-target-error
   :module-node-target-error
   :target-node-error

   :ambiguous-context-error
   :ambiguous-meta-node-context
   :node-cycle-error

   :dependency-not-reachable)

  (:documentation
   "Package containing the compiler frontend, which builds the node
    definitions out of the parsed source files."))

(defpackage :tridash
  (:use :generic-cl
        :alexandria
        :anaphora
        :cl-arrows
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
