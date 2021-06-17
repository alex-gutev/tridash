;;;; macros-core-module.lisp
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

;;;; Unit tests for core module running in macro system.

(defpackage :tridash/test.macros.core-module
  (:use :generic-cl
        :alexandria
        :anaphora
        :arrows
        :iterate
        :optima
        :named-readtables

        :tridash.parser
        :tridash.frontend

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
                :lol-syntax)

  (:import-from :tridash.frontend
                :tridash->cl-function
                :call-meta-node
                :call-tridash-meta-node
                :call-node

                :thunk
                :resolve
                :resolve%
                :tridash-fail

                :fail-thunk
                :+empty-list+
                :group-rest-args

                :check-arity
                :correct-arity?%
                :fail-arity-error

                :+optional-argument+
                :+rest-argument+)

  (:import-from :tridash/test.macros
                :macros))

(in-package :tridash/test.macros.core-module)

(in-readtable lol-syntax)


;;; Test Suite Definition

(def-suite core-module
    :description "Test core module running in macro system."
    :in macros)

(in-suite core-module)


;;; Test Core Module Utilities

(test core-utility-fails?
  "Test meta-node: fails?."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, fails?)")

    (with-nodes ((fails? "fails?")) modules
      (is-false (bool-value (call-meta-node fails? '(1))))
      (is-true (bool-value (call-meta-node fails? (list (fail-thunk))))))))

(test utility-fail-type?
  "Test meta-node: fail-type?."

  (with-module-table modules
    (build-core-module)

    (with-nodes ((fail-type? "fail-type?")) modules
      (is-false (bool-value (call-meta-node fail-type? '(1 x))))
      (is-false (bool-value (call-meta-node fail-type? (list (fail-thunk 'y) 'x))))
      (is-true (bool-value (call-meta-node fail-type? (list (fail-thunk 'x) 'x)))))))

(test utility-?
  "Test meta-node: ?."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, ?)")

    (with-nodes ((? "?")) modules
      (is-true (bool-value (call-meta-node ? '(1))))
      (is-false (bool-value (call-meta-node ? (list (fail-thunk))))))))

(test utility-!!
  "Test meta-node: !!."

  (with-module-table modules
    (build-core-module)

    (with-nodes ((!! "!!")) modules
      (is-true (bool-value (call-meta-node !! '(1))))
      (signals tridash-fail (call-meta-node !! (list (fail-thunk)))))))

(test utility-!-
  "Test meta-node: !-"

  (with-module-table modules
    (build-core-module)

    (with-nodes ((!- "!-")) modules
      (is (= 2 (call-meta-node !- '(1 2))))
      (signals tridash-fail (call-meta-node !- (list (fail-thunk) 10))))))


;;; Test Lists

(test list-list
  "Test meta-node: list."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, list, +)")

    (with-nodes ((list "list")) modules
      (is (= '(1 2 3) (call-meta-node list '(1 2 3))))

      (is-true (resolve% (call-tridash-meta-node list (list (list 1 2 (fail-thunk))))))
      (is-true (resolve% (call-tridash-meta-node list (list (list 1 (fail-thunk) 2)))))
      (is-true (resolve% (call-tridash-meta-node list (list (list (fail-thunk) 1 2))))))))

(test list-list*
  "Test meta-node: list*."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, list*)")

    (with-nodes ((list* "list*")) modules
      (is (= '(1 2 3 4 5) (call-meta-node list* '(1 2 (3 4 5)))))
      (is (= '(1 2 3) (call-meta-node list* '((1 2 3))))))))

(test list-list!
  "Test meta-node: list!"

  (with-module-table modules
    (build-core-module)
    (build "/import(core, list!)")

    (with-nodes ((list! "list!")) modules
      (is (= '(1 2 3) (call-meta-node list! '(1 2 3))))

      (signals
          tridash-fail
        (resolve% (call-tridash-meta-node list! (list (list 1 2 (fail-thunk))))))

      (signals
          tridash-fail
        (resolve% (call-tridash-meta-node list! (list (list 1 (fail-thunk) 2)))))

      (signals
          tridash-fail
        (resolve% (call-tridash-meta-node list! (list (list (fail-thunk) 1 2))))))))

(test list-nth
  "Test meta-node: nth."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, nth)")

    (with-nodes ((nth "nth")) modules
      (is (= 1 (call-meta-node nth '((1 2 3) 0))))
      (is (= 2 (call-meta-node nth '((1 2 3) 1))))
      (is (= 3 (call-meta-node nth '((1 2 3) 2))))
      (signals tridash-fail (call-meta-node nth '((1 2 3) 3))))))

(test list-append
  "Test meta-node: append."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, append)")

    (with-nodes ((append "append")) modules
      (is (= '(1 2 3 4 5 6) (call-meta-node append '((1 2 3) (4 5 6)))))
      (is (= '(1 2 3) (call-meta-node append (list '(1 2 3) +empty-list+))))
      (is (= '(1 2 3) (call-meta-node append (list +empty-list+ '(1 2 3)))))

      (signals tridash-fail (call-meta-node append (list '(1 2 3) (fail-thunk))))
      (signals tridash-fail (call-meta-node append (list (fail-thunk) '(1 2 3)))))))

(test list-foldl\'
  "Test meta-node: foldl'."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, foldl', /)")

    (with-nodes ((foldl "foldl'") (/ "/")) modules
      ;; Use division '/' as it is non-commutative. This ensures
      ;; that the result is passed in the first argument and list
      ;; element in the second argument.

      (is (= 1 (call-meta-node foldl (list 24 / '(4 3 2)))))
      (is (= 2 (call-meta-node foldl (list 24 / '(4 3)))))
      (is (= 24 (call-meta-node foldl (list 24 / +empty-list+))))

      (signals tridash-fail (call-meta-node foldl (list 24 / (fail-thunk)))))))

(test list-foldl
  "Test meta-node: foldl."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, foldl, /)")

    (with-nodes ((foldl "foldl") (/ "/")) modules
      ;; Use division '/' as it is non-commutative. This ensures
      ;; that the result is passed in the first argument and list
      ;; element in the second argument.

      (is (= 1 (call-meta-node foldl (list / '(24 4 3 2)))))
      (is (= 2 (call-meta-node foldl (list / '(24 4 3)))))
      (is (= 24 (call-meta-node foldl (list / '(24)))))

      (signals tridash-fail (call-meta-node foldl (list / +empty-list+)))
      (signals tridash-fail (call-meta-node foldl (list / (fail-thunk)))))))

(test list-foldr
  "Test meta-node: foldr."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, foldr, list)")

    (with-nodes ((foldr "foldr") (list "list")) modules
      (is (= '(1 (2 (3 (4 5)))) (call-meta-node foldr (list list '(1 2 3 4 5)))))
      (is (= '(1 (2 (3 (4 (5 6))))) (call-meta-node foldr (list list '(1 2 3 4 5) 6))))

      (is (= `(1 (2 (3 (4 (5 ,+empty-list+)))))
             (call-meta-node foldr (list list '(1 2 3 4 5) +empty-list+))))

      (is (= 1 (call-meta-node foldr (list list '(1)))))
      (is (= 1 (call-meta-node foldr (list list +empty-list+ 1))))
      (signals tridash-fail (call-meta-node foldr (list list +empty-list+)))


      (signals tridash-fail (call-meta-node foldr (list list (fail-thunk) 1)))
      (signals tridash-fail (call-meta-node foldr (list list (fail-thunk)))))))

(test list-map
  "Test meta-node: map."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, map, +)"
           "1+(n) : n + 1")

    (with-nodes ((map "map") (1+ "1+")) modules
      (is (= '(2 3 4 5) (call-meta-node map (list 1+ '(1 2 3 4)))))
      (is (= '(2) (call-meta-node map (list 1+ '(1)))))

      (is (= +empty-list+ (call-meta-node map (list 1+ +empty-list+))))
      (signals tridash-fail (call-meta-node map (list 1+ (fail-thunk)))))))

(test list-filter
  "Test meta-node: filter."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, filter, >)"
           ">5(n) : n > 5")

    (with-nodes ((filter "filter") (>5 ">5")) modules
      (is (= '(7 9) (call-meta-node filter (list >5 '(2 7 1 3 9 0)))))
      (is (= +empty-list+ (call-meta-node filter (list >5 '(1 2 3)))))
      (is (= +empty-list+ (call-meta-node filter (list >5 +empty-list+))))

      (signals tridash-fail (call-meta-node filter (list >5 (fail-thunk)))))))

(test list-every?
  "Test meta-node: every?."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, every?)")
    (build "/import(core, >)"
           ">3(n) : n > 3")

    (with-nodes ((>3 ">3") (every? "every?")) modules
      (is-true (bool-value (call-meta-node every? (list >3 '(4 5 6)))))
      (is-false (bool-value (call-meta-node every? (list >3 '(1 2 3 4 5 6)))))
      (is-true (bool-value (call-meta-node every? (list >3 +empty-list+))))

      (signals tridash-fail (bool-value (call-meta-node every? (list >3 (fail-thunk))))))))

(test list-some?
  "Test meta-node: some?."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, some?)")
    (build "/import(core, >)"
           ">3(n) : n > 3")

    (with-nodes ((>3 ">3") (some? "some?")) modules
      (is-true (bool-value (call-meta-node some? (list >3 '(4 5 6)))))
      (is-true (bool-value (call-meta-node some? (list >3 '(1 2 3 4 5 6)))))
      (is-false (bool-value (call-meta-node some? (list >3 '(0 1 2)))))
      (is-false (bool-value (call-meta-node some? (list >3 +empty-list+))))

      (signals tridash-fail (bool-value (call-meta-node some? (list >3 (fail-thunk))))))))

(test list-not-any?
  "Test meta-node: not-any?."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, not-any?)")
    (build "/import(core, >)"
           ">3(n) : n > 3")

    (with-nodes ((>3 ">3") (not-any? "not-any?")) modules
      (is-false (bool-value (call-meta-node not-any? (list >3 '(4 5 6)))))
      (is-false (bool-value (call-meta-node not-any? (list >3 '(1 2 3 4 5 6)))))
      (is-true (bool-value (call-meta-node not-any? (list >3 '(1 2 3)))))
      (is-true (bool-value (call-meta-node not-any? (list >3 +empty-list+))))

      (signals tridash-fail (bool-value (call-meta-node not-any? (list >3 (fail-thunk))))))))

(test list-not-every?
  "Test meta-node: not-every?."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, not-every?)")
    (build "/import(core, >)"
           ">3(n) : n > 3")

    (with-nodes ((>3 ">3") (not-every? "not-every?")) modules
      (is-false (bool-value (call-meta-node not-every? (list >3 '(4 5 6)))))
      (is-true (bool-value (call-meta-node not-every? (list >3 '(1 2 3 4 5 6)))))
      (is-true (bool-value (call-meta-node not-every? (list >3 '(0 1 2)))))
      (is-false (bool-value (call-meta-node not-every? (list >3 +empty-list+))))

      (signals tridash-fail (bool-value (call-meta-node not-every? (list >3 (fail-thunk))))))))


;;; Test Introspection Meta-Nodes

(test introspection-node?
  "Test meta-node: node?."

  (with-module-table modules
    (build-core-module)
    (build "/import(core/introspection, node?, find-node, get-attribute)"
           "/import(core, +)")

    (build "x")
    (with-nodes ((node? "node?") (x "x")) modules
      (is-true (bool-value (call-meta-node node? (list x))))
      (is-false (bool-value (call-meta-node node? '(x)))))))

(test introspection-find-node
  "Test meta-node: find-node."

  (with-module-table modules
    (build-core-module)
    (build "/import(core/introspection, node?, find-node, get-attribute)"
           "/import(core, +)")

    (build "x + y")

    (with-modules ((init :init)) modules
      (let ((tridash.frontend::*functor-module* init)
            (tridash.frontend::*current-module* init))
        (with-nodes ((find-node "find-node")
                     (x "x")
                     (x+y (("/in" "core" "+") "x" "y")))
            modules

          (is (= x (call-meta-node find-node (list (id-symbol "x")))))
          (is (= x+y (call-meta-node find-node (list (name x+y)))))
          (signals tridash-fail (call-meta-node find-node (list (id-symbol "z")))))))))

(test introspection-get-attribute
  "Test meta-node: get-attribute."

  (with-module-table modules
    (build-core-module)
    (build "/import(core/introspection, node?, find-node, get-attribute)"
           "/import(core, +)")

    (build "/attribute(x, my-key, my-value)")
    (with-nodes ((get-attribute "get-attribute") (x "x"))
        modules

      (is (= (id-symbol "my-value")
             (call-meta-node get-attribute (list x (id-symbol "my-key")))))

      (signals tridash-fail
        (call-meta-node get-attribute (list x (id-symbol "not-a-key")))))))


;;; Test Pattern Matching

(test pattern-match-integer
  "Test pattern match integer."

  (with-module-table modules
    (build-core-module)

    (build "/import(core, int)")
    (build "f-int(x) : { x -> int(y); y }")

    (with-nodes ((f-int "f-int")) modules
      (is (= (call-meta-node f-int '(1)) 1))
      (signals tridash-fail (call-meta-node f-int '(2.3)))
      (signals tridash-fail (call-meta-node f-int '("hello"))))))

(test pattern-match-real
  "Test pattern match real."

  (with-module-table modules
    (build-core-module)

    (build "/import(core, real)")
    (build "f-real(x) : { x -> real(y); y }")

    (with-nodes ((f-real "f-real")) modules
      (is (= 1 (call-meta-node f-real '(1))))
      (is (= 2.3 (call-meta-node f-real '(2.3))))
      (signals tridash-fail (call-meta-node f-real '("hello"))))))

(test pattern-match-string
  "Test pattern match string."

  (with-module-table modules
    (build-core-module)

    (build "/import(core, string)")
    (build "f-string(x) : { x -> string(y); y }")

    (with-nodes ((f-string "f-string")) modules
      (is (= "hello" (call-meta-node f-string '("hello"))))
      (signals tridash-fail (call-meta-node f-string '(1)))
      (signals tridash-fail (call-meta-node f-string '(2.3))))))

(test pattern-match-cons
  "Test pattern match cons."

  (with-module-table modules
    (build-core-module)

    (build "/import(core, cons, list)"
           "f-cons(x) : { x -> cons(h,t); list(h,t) }")

    (with-nodes ((f-cons "f-cons")) modules
      (is (= '(1 (2 3)) (call-meta-node f-cons '((1 2 3)))))
      (signals tridash-fail (call-meta-node f-cons '(1)))
      (signals tridash-fail (call-meta-node f-cons '("hello"))))))

(test pattern-match-list
  "Test pattern match list."

  (with-module-table modules
    (build-core-module)

    (build "/import(core, list, +)"
           "f-list(x) : { x -> list(a, b, c); a + b + c }")

    (with-nodes ((f-list "f-list")) modules
      (is (= 6 (call-meta-node f-list '((1 2 3)))))
      (signals tridash-fail (call-meta-node f-list '((1 2))))
      (signals tridash-fail (call-meta-node f-list '((1 2 3 4))))

      (signals tridash-fail (call-meta-node f-list '(1)))
      (signals tridash-fail (call-meta-node f-list '("hello"))))))

(test pattern-match-list*
  "Test pattenr match list*."

  (with-module-table modules
    (build-core-module)

    (build "/import(core, list, list*, +)"
           "f-list*(x) : { x -> list*(a, b, xs); list*(a + b, xs) }")

    (with-nodes ((f-list* "f-list*")) modules
      (is (= '(3 3) (call-meta-node f-list* '((1 2 3)))))
      (is (= '(3) (call-meta-node f-list* '((1 2)))))
      (is (= '(3 3 4) (call-meta-node f-list* '((1 2 3 4)))))

      (signals tridash-fail (call-meta-node f-list* '((1))))
      (signals tridash-fail (call-meta-node f-list* '(1)))
      (signals tridash-fail (call-meta-node f-list* '("hello"))))))

(test pattern-match-and
  "Test pattern match and."

  (with-module-table modules
    (build-core-module)

    (build "/import(core, list, and, +)"
           "f-and(xs) : { xs -> list(a, 1 and b); xs -> list(a and b); a + b }")

    (with-nodes ((f-and "f-and")) modules
      (is (= 3 (call-meta-node f-and '((2 1)))))
      (is (= 4 (call-meta-node f-and '((2)))))

      (signals tridash-fail (call-meta-node f-and '((2 2)))))))

(test pattern-match-or
  "Test pattern match or."

  (with-module-table modules
    (build-core-module)

    (build "/import(core, list, or)"
           "f-or(xs) : { xs -> list(a) or list(1, a); a }")

    (with-nodes ((f-or "f-or")) modules
      (is (= 1 (call-meta-node f-or '((1)))))
      (is (= 2 (call-meta-node f-or '((2)))))
      (is (= 10 (call-meta-node f-or '((1 10)))))

      (signals tridash-fail (call-meta-node f-or '((1 2 3))))
      (signals tridash-fail (call-meta-node f-or '((2 10)))))))

(test pattern-match-not
  "Test pattern match not."

  (with-module-table modules
    (build-core-module)

    (build "/import(core, list, not, and)"
           "f-not(xs) : { xs -> list(1, not 2 and x); x }")

    (with-nodes ((f-not "f-not")) modules
      (is (= 1 (call-meta-node f-not '((1 1)))))
      (is (= 3 (call-meta-node f-not '((1 3)))))

      (signals tridash-fail (call-meta-node f-not '((1 2))))
      (signals tridash-fail (call-meta-node f-not '((2 3)))))))

(test pattern-match-nested-multiple-patterns
  "Test pattern matching of nested and multiple patterns."

  (with-module-table modules
    (build-core-module)

    (build-source-file #p"./test/inputs/macros/pattern-match-nested.trd" modules)

    (with-nodes ((calc "calc")) modules
      (is (= 3 (call-meta-node calc (decls '(!|add| 1 2)))))
      (is (= 2.5 (call-meta-node calc (decls '(!|sub| 3 0.5)))))
      (is (= -5 (call-meta-node calc (decls '(!|neg| 5)))))

      (signals tridash-fail (call-meta-node calc '((1 2 3))))
      (signals tridash-fail (call-meta-node calc (decls '(!|add| 1))))
      (signals tridash-fail (call-meta-node calc (decls '(!|add| "x" "y"))))
      (signals tridash-fail (call-meta-node calc '("x"))))))

(test pattern-match-similar-pattenrs
  "Test pattern matching of similar patterns."

  (with-module-table modules
    (build-core-module)

    (build-source-file #p"./test/inputs/macros/pattern-match-similar.trd" modules)

    (with-nodes ((calc3 "calc3")) modules
      (is (= 3 (call-meta-node calc3 (decls '(!|add| 1 2)))))
      (is (= 2.5 (call-meta-node calc3 (decls '(!|sub| 3 0.5)))))
      (is (= 4 (call-meta-node calc3 (decls '(!|sub| 5)))))

      (signals tridash-fail (call-meta-node calc3 '((1 2 3))))
      (signals tridash-fail (call-meta-node calc3 (decls '(!|add| 1))))
      (signals tridash-fail (call-meta-node calc3 (decls '(!|add| "x" "y"))))
      (signals tridash-fail (call-meta-node calc3 '("x"))))))

(test pattern-match-constant-pattenrs
  "Test pattern matching of constant patterns"

  (with-module-table modules
    (build-core-module)

    (build-source-file #p"./test/inputs/macros/pattern-match-constant.trd" modules)

    (with-nodes ((calc2 "calc2")) modules
      (is (= 3 (call-meta-node calc2 '((0 1 2)))))
      (is (= 2.5 (call-meta-node calc2 '((0.5 3 0.5)))))
      (is (= -5 (call-meta-node calc2 '(("neg" 5)))))
      (is (= -9 (call-meta-node calc2 '((#\- 9)))))

      (signals tridash-fail (call-meta-node calc2 '((1 2 3))))
      (signals tridash-fail (call-meta-node calc2 (decls '(0 1))))
      (signals tridash-fail (call-meta-node calc2 (decls '(0 "x" "y"))))
      (signals tridash-fail (call-meta-node calc2 '("x"))))))


;;; Test Utility Macros

(test utility-macro-case
  "Test case macro."

  (with-module-table modules
    (build-core-module)

    (build "/import(core, case, =, ')"
           "test-case(x) : case{x = 1 : ' a; x = 2 : ' b; '(other)}")

    (with-nodes ((test-case "test-case")) modules
      (is (= (id-symbol "a") (call-meta-node test-case '(1))))
      (is (= (id-symbol "b") (call-meta-node test-case '(2))))
      (is (= (id-symbol "other") (call-meta-node test-case '(3)))))))

(test utility-macro-case-no-default
  "Test case macro without default case."

  (with-module-table modules
    (build-core-module)

    (build "/import(core, case, =, ')"
           "test-case2(x) : case{x = 1 : ' a; x = 2 : '(b)}")

    (with-nodes ((test-case2 "test-case2")) modules
      (is (= (id-symbol "a") (call-meta-node test-case2 '(1))))
      (is (= (id-symbol "b") (call-meta-node test-case2 '(2))))
      (signals tridash-fail (call-meta-node test-case2 '(3))))))

(test utility-macro-!
  "Test ! macro."

  (with-module-table modules
    (build-core-module)

    (build "/import(core, cons, !)"
           "cons!(x, y) : !(cons(x, y))")

    (with-nodes ((cons! "cons!")) modules
      (is (= '(1 2 3) (call-meta-node cons! '(1 (2 3)))))

      (signals tridash-fail
        (resolve% (call-tridash-meta-node cons! (list 1 (fail-thunk)))))

      (signals tridash-fail
        (resolve% (call-tridash-meta-node cons! (list (fail-thunk) 1)))))))


;;; Test String Meta-Nodes

(test string-string->list
  "Test meta-node: string->list."

  (with-module-table modules
    (build-core-module)

    (with-nodes ((string->list "string->list")) modules
      (is (= '(#\h #\e #\l #\l #\o) (call-meta-node string->list '("hello"))))

      (signals tridash-fail (call-meta-node string->list '(1)))
      (signals tridash-fail (call-meta-node string->list '((#\h #\e #\l #\l #\o)))))))

(test string-list->string
  "Test meta-node: list->string."

  (with-module-table modules
    (build-core-module)

    (with-nodes ((list->string "list->string")) modules
      (is (= "hello" (call-meta-node list->string '((#\h #\e #\l #\l #\o)))))
      (is (= "abc123" (call-meta-node list->string '((#\a #\b #\c 1 2 3)))))
      (is (= "abc123" (call-meta-node list->string '(("ab" #\c #\1 23)))))
      (is (= "" (call-meta-node list->string (list +empty-list+))))

      (signals tridash-fail (call-meta-node list->string '("hello")))
      (signals tridash-fail (call-meta-node list->string '(1))))))

(test string-format
  "Test meta-node: format."

  (with-module-table modules
    (build-core-module)

    (with-nodes ((format "format")) modules
      (is (= "hello" (call-meta-node format '("hello"))))
      (is (= "Hello Bob." (call-meta-node format '("Hello %s." "Bob"))))
      (is (= "Hello Bob, welcome to Earth."
             (call-meta-node format '("Hello %s, welcome to %s." "Bob" "Earth"))))

      (is (= "Coverage: 15% of 200."
             (call-meta-node format '("Coverage: %s%% of %s." 15 200))))

      (is (= "abc" (call-meta-node format '("%s" "abc"))))
      (is (= "35%" (call-meta-node format '("%s%%" 35)))))))


;;; Test Functional Programming Utilities

(test functional-apply
  "Test meta-node: apply."

  (with-module-table modules
    (build-core-module)

    (build "/import(core, apply, list, cons, +)"

           "f(x, y) : apply(list, x + y, list(x, y))"
           "g(x, ..(xs)) : apply(list, x, xs)"
           "h(x) : apply(list, list(x))"
           "u(x) : apply(list)"

           "e1(x, y) : apply(list, x, y)"
           "e2(x, y) : apply(list, 1, cons(x, y))"
           "e3(x) : apply(1, list(x))")

    (with-nodes ((f "f") (g "g") (h "h") (u "u")
                 (e1 "e1") (e2 "e2") (e3 "e3"))
        modules

      (is (= '(3 1 2) (call-meta-node f '(1 2))))
      (is (= '(1 2 3 4) (call-meta-node g '(1 2 3 4))))
      (is (= '(3) (call-meta-node h '(3))))

      (handler-bind
          ((tridash-fail
            (lambda (c)
              (when (= (fail-type c) tridash.frontend::+empty-list+)
                (replace-failure nil)))))

        (is (= '() (call-meta-node u '(1)))))

      (signals tridash-fail (call-meta-node e1 '(5 6)))
      (signals tridash-fail (call-meta-node e2 '(7 8)))
      (signals tridash-fail (call-meta-node e3 '(9))))))
