;;;; strictness.lisp
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

;;;; Strictness Analysis Tests

(defpackage :tridash/test.strictness
  (:use :generic-cl
        :alexandria
        :arrows
        :optima
        :named-readtables

        :tridash.parser
        :tridash.frontend
        :tridash.frontend.strictness

        :fiveam
        :tridash/test
        :tridash/test.util)

  (:shadowing-import-from :generic-cl
                          :emptyp
                          :multiply
                          :accumulate)

  (:shadowing-import-from :fiveam :fail)

  (:import-from :lol
                :defmacro!
                :lol-syntax))

(in-package :tridash/test.strictness)

(in-readtable lol-syntax)


(def-suite strictness-analysis
    :description "Test node value strictness analysis."
    :in frontend)

(in-suite strictness-analysis)

(defmacro! test-strictness ((&rest source) &body tests)
  (flet ((make-attr-string (x)
           (format nil "/attribute(~a, removable, 0)" x)))

    (let ((syms (map (compose #'gensym #'first) tests)))
      `(with-module-table ,g!modules
         (build-core-module)
         (build "/import(core)" ,@source
                ,@(map (compose #'make-attr-string #'first) tests))

         (with-nodes ,(map #'list syms (map #'first tests))
             (finish-build)

           ,@(map #2`(is (= ',(second a2) (strict-arguments ,a1))) syms tests))))))


;;; Tests

(test simple-functors
  "Test strictness analysis of simple functor expressions."

  (test-strictness ("1+(n) : n + 1")
    ("1+" (t))))

(test if-meta-node
  "Test strictness analysis of expressions of the builtin if meta-node."

  (test-strictness ("my-and(a, b) : if(a, b, 0)")
    ("my-and" (t nil))))

(test and-or-meta-nodes
  "Test strictness analysis of expressions of the builtin `and` and `or` meta-nodes."

  (test-strictness ("my-if(test, then, else) : or(and(test, then), else)")
    ("my-if" (t nil nil))))

(test if-and-catch-expressions
  "Test strictness analysis of `if` And `catch` internal expression types."

  (test-strictness
      ("f(a, b, c) : { a -> (b -> /context(self, c)); c -> /context(self, c) }")

    ("f" (t nil nil))))

(test member-expressions
  "Test strictness analysis of internal member expression types."

  (test-strictness ("f(x) : x.field")
    ("f" (t))))

(test higher-order-meta-nodes
  "Test strictness analysis of expressions with higher-order meta-nodes."

  (test-strictness ("my-apply(f, x, y) : f(x, y)")
    ("my-apply" (t nil nil))))


;;; Recursive Meta-Node Tests

(test recursive-meta-node-simple
  "Test strictness analysis of simple non-tail recursive meta-node (factorial)."

  (test-strictness ("fact(n) : case(n < 2 : 1, n * fact(n - 1))")
    ("fact" (t))))

(test recursive-meta-node-tail
  "Test strictness analysis of tail recursive meta-node (factorial)."

  (test-strictness ("fact(n, acc) : case(n < 2 : acc, fact(n - 1, n * acc))")
    ("fact" (t t))))

(test recursive-meta-node-tail-helper
  "Test strictness analysis of tail recursive meta-node with helper meta-nodes."

  (test-strictness
      ("1-(n) : n - 1"
       "mul(x,y) : x * y"

       "fact(n, acc) : case(n < 2 : acc, fact(1-(n), mul(n, acc)))")

    ("fact" (t t))))

(test recursive-meta-node-tail-nested
  "Test strictness analysis of nested tail-recursive meta-node."

  (test-strictness
      ("fact(n) : { iter(n, acc) : case(n < 2 : acc, iter(n - 1, n * acc)); iter(n,1) }")
    ("fact" (t))))


;;; Test Meta-Nodes with Optional and Rest Arguments

(test meta-node-optional-arguments
  "Test strictness analysis of meta-node optional arguments."

  (test-strictness ("inc(n, d : 1) : n + d")
    ("inc" (t t)))

  (test-strictness ("f(x, y : 1) : { x -> self.x; y -> self.y }")
    ("f" (nil nil))))

(test meta-node-rest-argument
  "Test strictness analysis of meta-node rest arguments."

  (test-strictness ("f(x, ..(xs)) : { x -> self.x; xs -> self.xs }")
    ("f" (nil nil))))

(test meta-node-rest-arguments
  "Test strictness analysis of recursive meta-node with rest arguments."

  (test-strictness ("add(x, ..(xs)) : if(xs, add(x + head(xs), tail(xs)), x)")
    ("add" (t t))))
