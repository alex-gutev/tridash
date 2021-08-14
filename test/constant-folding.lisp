;;;; constant-folding.lisp
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

(defpackage :tridash/test.constant-folding
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

(in-package :tridash/test.constant-folding)


(def-suite constant-folding
    :description "Test constant node folding."
    :in frontend)

(in-suite constant-folding)


;;; Literal Constants

(test fold-literal
  "Test folding of literal constant"

  (with-module-table modules
    (build "/external(add, x, y)"
           "add(a, b) -> cc"
           "1 -> constant; constant -> b"
           "/attribute(a, input, 1)")

    (let ((table (finish-build)))
      (test-not-nodes table "b" "constant")

      (with-nodes ((add "add") (a "a") (cc "cc")) table
        (has-value-function (a) cc `(,add ,a 1))))))

(test fold-literal-multi-node
  "Test folding of literal constant across multiple node levels of bindings."

  (with-module-table modules
    (build "a -> b; b -> cc"
           "3 -> constant; constant -> b"
           "/attribute(a, input, 1)")

    (let ((table (finish-build)))
      (test-not-nodes table "constant")

      (with-nodes ((a "a") (b "b") (cc "cc")) table
        (test-simple-binding a b)
        (test-simple-binding b cc)

        (test-value-function b (init-context b) 3)))))


;;; Initial Values

(test input-node-initial-value
  "Test binding initial value of input node to literal."

  (with-module-table modules
    (build "a -> b; b -> cc"
           "\"hello\" -> constant; constant -> a"
           "/attribute(a, input, 1)")

    (let ((table (finish-build)))
      (test-not-nodes table "b" "constant")

      (with-nodes ((a "a") (cc "cc")) table
        (test-simple-binding a cc)

        (test-value-function a (init-context a) "hello")))))

(test non-input-node-inital-value
  "Test binding initial value of non-input node."

  (with-module-table modules
    (build "a -> b; b -> cc"

           "\"hello\" -> constant; constant -> a"
           "constant -> cc"

           "/attribute(a, input, 1)")

    (let ((table (finish-build)))
      (test-not-nodes table "b" "constant")

      (with-nodes ((a "a") (cc "cc")) table
        (test-simple-binding a cc)

        (test-value-function a (init-context a) "hello")
        (test-value-function cc (init-context cc) "hello")))))


;;; Functor Node Constants

(test fold-functor-node-constant
  "Test folding of constant functor nodes."

  (with-module-table modules
    (build "/external(add, x, y)"
           "add(a, b) -> cc"

           "1 -> c1; 2 -> c2; c2 -> c3"
           "add(c1, c3) -> b"

           "/attribute(a, input, 1)")

    (let ((table (finish-build)))
      (test-not-nodes table "b" "c1" "c2" "c3")

      (with-nodes ((add "add") (a "a") (cc "cc")) table
        (has-value-function (a) cc `(,add ,a (,add 1 2)))))))

(test fold-functor-node-constant-multi-node
  "Test folding of constant functor nodes with constant functor node dependencies."

  (with-module-table modules
    (build "/external(add, x, y)"
           "a -> b; b -> cc"

           "5 -> c1; 10 -> c2; c2 -> c3"
           "add(c1, add(c2, c3)) -> b"

           "/attribute(a, input, 1)")

    (let ((table (finish-build)))
      (test-not-nodes table "c1" "c2" "c3")

      (with-nodes ((add "add") (a "a") (b "b") (cc "cc")) table
        (test-simple-binding a b)
        (test-simple-binding b cc)

        (test-value-function b (init-context b) `(,add 5 (,add 10 10)))))))


;;; Cross Module Constant Folding

(test cross-module-folding
  "Test constant folding across modules."

  (with-module-table modules
    (build-core-module)
    (build "/module(m1)"
           "/import(builtin)"
           "1 -> constant"
           "constant -> param"

           "/module(m2)"
           "/import(builtin)"
           "/import(core, /use)"
           "/use(m1)"
           "/external(add, x, y)"

           "2 -> param"
           "add(param, m1.param) -> sum"
           "add(a, sum) -> b"
           "b -> cc"

           "/attribute(a, input, 1)")

    (let ((table (finish-build)))
      (test-not-nodes table "constant" "param" "sum" "b")

      (with-nodes ((add "add") (a "a") (cc "cc")) table
        (has-value-function (a) cc `(,add ,a (,add 2 1)))))))


;;; Two-Way Bindings

(test two-way-bindings
  "Test constant folding across two-way bindings."

  (with-module-table modules
    (build "/external(add, a, b)"
           "1 -> x"
           "add(x,y) -> x"
           "add(x,y) -> out"

           "/attribute(y, input, 1)")

    (let ((table (finish-build)))
      (test-not-nodes table "x")

      (with-nodes ((add "add") (y "y") (out "out")) table
        (has-value-function
            (y)
            out

          `(,add 1 ,y))))))
