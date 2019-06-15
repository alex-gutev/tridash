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

(defpackage :tridash.backend.js.ast
  (:use :generic-cl)

  (:import-from :let-over-lambda
                :mkstr)

  (:export
   ;; AST Nodes

   :js-call
   :js-call-p
   :js-call-operator
   :js-call-operands
   :make-js-call

   :js-new
   :js-new-p
   :js-new-operator
   :js-new-operands

   :js-element
   :js-element-p
   :js-element-object
   :js-element-element

   :js-member
   :js-member-p
   :js-member-object
   :js-member-field
   :js-members

   :js-string
   :js-string-p
   :js-string-string

   :js-array
   :js-array-p
   :js-array-elements

   :js-object
   :js-object-p
   :js-object-fields

   :js-if
   :js-if-p
   :js-if-condition
   :js-if-then
   :js-if-else

   :js-block
   :js-block-p
   :js-block-statements
   :make-js-block

   :js-while
   :js-while-p
   :js-while-condition
   :js-while-body

   :js-function
   :js-function-p
   :js-function-name
   :js-function-arguments
   :js-function-statements

   :js-lambda
   :lexical-block
   :make-lexical-block

   :js-catch
   :js-catch-p
   :js-catch-try
   :js-catch-catch

   :js-return
   :js-return-p
   :js-return-value

   :js-var
   :js-var-p
   :js-var-var
   :js-var-value

   :js-continue
   :js-continue-p

   :js-throw
   :js-throw-p
   :js-throw-expression

   :function-expression?
   :expression
   :expressionp)

  (:documentation "Contains the JavaScript AST Definitions."))

(defpackage :tridash.backend.js
  (:use :generic-cl
        :alexandria
        :anaphora
        :named-readtables
        :optima
        :optima.ppcre
        :iterate
        :cl-ppcre
        :cl-arrows

        :tridash.util
        :tridash.parser
        :tridash.interface
        :tridash.frontend
        :tridash.builder.html

        :tridash.backend.js.ast)

  (:shadowing-import-from :generic-cl
                          :emptyp
                          :multiply
                          :accumulate)

  (:import-from :let-over-lambda
                :mkstr
                :symb
                :lol-syntax)

  (:documentation "JavaScript Backend."))
