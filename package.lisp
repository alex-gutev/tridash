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
  (:use :common-lisp
        :alexandria
        :anaphora
        :optima)

  (:export
   ;; Frontend Interface
   :build-nodes-in-file
   :set-file-builder
   :define-file-builder
   :build-files

   ;; Backend Interface
   :compile-nodes

   ;; Error Reporting
   :error-description)

  (:documentation
   "Package containing the interface to the frontend, backends and
    error reporting."))

(defpackage :tridash.parser
  (:use :common-lisp
        :alexandria
        :anaphora
        :cl-arrows
        :iterate
        :graylex
        :parse-number
        :optima
        :optima.ppcre

        :tridash.interface)

  (:export
   :+list-operator+

   :make-parser
   :add-operator

   :node-path->name
   :id-symbol)

  (:documentation
   "Package containing the parser and lexer."))

(defpackage :tridash.frontend
  (:use :common-lisp
	:alexandria
	:anaphora
	:iterate
	:named-readtables
	:optima
        :cl-arrows

        :tridash.util
	:tridash.parser
        :tridash.interface)

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

   ;; Node Contexts
   :node-context
   :operands
   :value-function
   :wait-set

   :context

   ;; Predicates
   :node?
   :value?
   :input-node?

   ;; Node-link
   :node-link
   :node-link-node
   :node-link-2-way-p
   :node-link-context
   :node-link-p
   :self

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

   ;; Node-table
   :node-table
   :outer-table
   :depth
   :all-nodes
   :nodes
   :meta-nodes
   :operator-nodes

   :add-node

   :input-nodes
   :add-input

   ;; Frontend State
   :module-table
   :modules
   :get-module

   ;; Builder
   :build-graph
   :build-parsed-nodes
   :build-node
   :finish-build-graph

   :build-program

   ;; Wait-set
   :build-wait-sets

   ;; Coalescing Nodes
   :coalesce-nodes

   ;; Error Conditions
   :semantic-error
   :message
   :target-node-error
   :ambiguous-context-error)

  (:documentation
   "Package containing the compiler frontend, which builds the node
    definitions out of the parsed source files."))

(defpackage :tridash
  (:use :common-lisp
        :alexandria
        :anaphora
        :cl-arrows
        :optima
        :iterate

        :tridash.util
        :tridash.parser
        :tridash.interface
        :tridash.frontend)

  (:documentation
   "Package containing the compiler application."))
