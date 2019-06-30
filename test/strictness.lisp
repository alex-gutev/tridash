;;;; strictness.lisp
;;;;
;;;; Tridash Programming Language.
;;;; Copyright (C) 2019  Alexander Gutev
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

;;;; Strictness Analysis Tests

(defpackage :tridash.test.strictness
  (:use :generic-cl
        :alexandria
        :cl-arrows
        :optima
        :prove
        :named-readtables

        :tridash.parser
        :tridash.frontend
        :tridash.frontend.strictness

        :tridash.test.util
        :tridash.test.builder)

  (:shadowing-import-from :generic-cl
                          :emptyp
                          :multiply
                          :accumulate)

  (:shadowing-import-from :prove :fail)

  (:import-from :lol
                :defmacro!
                :lol-syntax))

(in-package :tridash.test.strictness)

(in-readtable lol-syntax)


(defmacro! test-strictness ((&rest source) &body tests)
  (let ((syms (map (compose #'gensym #'first) tests)))
   `(with-module-table ,g!modules
      (build-source-file #p"./modules/core.trd" ,g!modules)
      (build ":import(core)" ,@source)

      (with-nodes ,(map #'list syms (map #'first tests))
          (finish-build)

        ,@(map #2`(is (strict-arguments ,a1) ',(second a2)) syms tests)))))

(plan 1)

(subtest "Strictness Analysis of Meta-Node Definitions"
  (subtest "Simple Functors"
    (test-strictness ("1+(n) : n + 1")
      ("1+" (t))))

  (subtest "If Meta-Node"
    (test-strictness ("my-and(a, b) : if(a, b, 0)")
      ("my-and" (t nil))))

  (subtest "And/Or Meta-Nodes"
    (test-strictness ("my-if(test, then, else) : or(and(test, then), else)")
      ("my-if" (t nil nil))))

  (subtest "If and Catch Expressions"
    (test-strictness
        ("f(a, b, c) : { a -> (b -> :context(self, c)); c -> :context(self, c) }")

      ("f" (t nil nil))))

  (subtest "Member Expressions"
    (test-strictness ("f(x) : x.field")
      ("f" (t))))

  (subtest "Higher-Order Meta-Nodes"
    (test-strictness ("apply(f, x, y) : f(x, y)")
      ("apply" (t nil nil))))

  (subtest "Recursive Meta-Nodes"
    (subtest "Factorial"
      (test-strictness ("fact(n) : case(n < 2 : 1, n * fact(n - 1))")
        ("fact" (t))))

    (subtest "Tail-Recursive Factorial"
      (test-strictness ("fact(n, acc) : case(n < 2 : acc, fact(n - 1, n * acc))")
        ("fact" (t t))))

    (subtest "Tail-Recursive Factorial with Helper Meta-Nodes"
      (test-strictness
          ("1-(n) : n - 1"
           "mul(x,y) : x * y"

           "fact(n, acc) : case(n < 2 : acc, fact(1-(n), mul(n, acc)))")
        ("fact" (t t))))

    (subtest "Tail-Recursive Factorial with Nested Meta-Node"
      (test-strictness
          ("fact(n) : { iter(n, acc) : case(n < 2 : acc, iter(n - 1, n * acc)); iter(n,1) }")
        ("fact" (t))))))

(finalize)
