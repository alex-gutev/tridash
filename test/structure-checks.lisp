;;;; structure-checks.lisp
;;;;
;;;; Tridash Programming Language.
;;;; Copyright (C) 2019-2021  Alexander Gutev
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

;;;; Unit tests for constant folding

(defpackage :tridash/test.structure-checks
  (:use :generic-cl
        :alexandria
        :arrows

        :fiveam
        :tridash.parser
        :tridash.frontend
        :tridash/test
        :tridash/test.util)

  (:shadowing-import-from :generic-cl
                          :emptyp
                          :multiply
                          :accumulate)

  (:import-from :tridash.parser
                :declaration-parse-error)

  (:import-from :tridash.frontend
                :*node-true*
                :*node-false*))

(in-package :tridash/test.structure-checks)


(def-suite structure-checks
    :description "Test checking of node graph structure."
    :in tridash)

(in-suite structure-checks)


;;; Ambiguous Context Checks

(test ambiguous-context-atom-nodes
  :description "Test checking for ambiguous contexts in atom nodes."

  (with-module-table modules
    (build "a -> d"
           "a -> b; b -> cc; cc -> d"
           "d -> e"

           "/attribute(a, input, 1)")

    (signals ambiguous-context-error (finish-build))))

(test ambiguous-context-atom-nodes-cross-module
  :description "Test checking for ambiguous contexts in atom nodes across modules."

  (with-module-table modules
    (build "/module(m1)"
           "/import(builtin)"
           "a -> d"
           "/attribute(a, input, 1)"

           "/module(m2)"
           "/import(builtin)"
           "/use(m1)"

           "m1.a -> b; b -> cc; cc -> m1.d"
           "m1.d -> e")

    (signals ambiguous-context-error (finish-build))))

(test ambiguous-context-functor-nodes
  :description "Test checking for ambiguous contexts in functor nodes."

  (with-module-table modules
    (build "/external(add, x, y)"
           "add(a, b) -> d"
           "a -> cc; cc -> d"
           "/attribute(a, input, 1)"
           "/attribute(b, input, 1)")

    (signals ambiguous-context-error (finish-build))))

(test ambiguous-context-functor-nodes-cross-module
  :description
  "Test checking for ambiguous contexts in functor nodes across modules."

  (with-module-table modules
    (build "/module(m1)"
           "a; b;"
           "/attribute(a, input, 1)"
           "/attribute(b, input, 1)"

           "/module(m2)"
           "/import(builtin)"
           "/use(m1)"

           "/external(add, x, y)"

           "add(m1.a, m1.b) -> d"
           "m1.a -> cc; cc -> d"
           "d -> e")

    (signals ambiguous-context-error (finish-build))))
