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

   ;; Declaration Types

   :node-declaration
   :node-declaration-location
   :node-declaration-p

   :atom-node
   :atom-node-identifier
   :atom-node-p

   :literal-node
   :literal-node-value
   :literal-node-p

   :functor-node
   :functor-node-operator
   :functor-node-operands
   :functor-node-p

   ;; Parser

   :make-parser
   :add-operator

   ;; Identifiers

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
		:defmacro!
                :nlet-tail)

  (:import-from :agutil
                :match-state)

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

   :node-ref
   :node-ref-p
   :node-ref-node

   :functor-expression
   :functor-expression-p
   :functor-expression-meta-node
   :functor-expression-arguments

   :if-expression
   :member-expression
   :catch-expression
   :fail-expression

   :object-expression
   :object-expression-p
   :object-expression-entries

   :expression-block
   :expression-block-p
   :expression-block-expression
   :expression-block-count

   :meta-node-ref
   :meta-node-ref-p
   :meta-node-ref-node
   :meta-node-ref-optional
   :meta-node-ref-outer-nodes

   :argument-list
   :argument-list-p
   :argument-list-arguments

   :walk-expression
   :map-expression!

   ;; Utility Functions
   :observer-list
   :dependency-list

   :observers-count
   :dependencies-count

   :unwrap-declaration

   ;; Meta-node
   :meta-node
   :built-meta-node
   :final-meta-node
   :external-meta-node

   :operands
   :definition

   :meta-node?
   :external-meta-node?

   :operand-node-names
   :+optional-argument+
   :+rest-argument+
   :+outer-node-argument+

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
   :finish-build-meta-node

   :build-program
   :build-source-file

   ;; Error Conditions
   :semantic-error
   :declaration-stack
   :module-table

   :not-node-error
   :module-node-reference-error
   :non-node-operator-error
   :non-existent-node-error
   :node-exists-error
   :redefine-special-operator-error
   :non-existent-module-error
   :create-alias-error
   :import-node-error
   :invalid-arguments-error
   :global-outer-reference-error
   :special-operator-reference-error
   :target-node-error
   :ambiguous-context-error
   :ambiguous-meta-node-context-error
   :node-cycle-error
   :dependency-not-reachable-error
   :unsupported-meta-node-error
   :macro-outer-node-error
   :arity-error
   :invalid-operand-list-error
   :compile-meta-node-loop-error

   :module-name
   :node-name
   :source
   :operator
   :arguments
   :expected
   :thing
   :allowed
   :value
   :arity

   ;; Tridash Failure Condition
   :tridash-fail
   :fail-type
   :replace-failure

   :*declaration-stack*
   :*tridash-call-reason*)

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
