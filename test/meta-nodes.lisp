;;;; meta-nodes.lisp
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

;;;; Unit tests for building of meta-node definition

(defpackage :tridash/test.meta-nodes
  (:use :generic-cl
        :alexandria
        :anaphora
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

(in-package :tridash/test.meta-nodes)


(def-suite meta-nodes
    :description "Test checking of node graph structure."
    :in tridash)

(in-suite meta-nodes)


;;; Simple Functions

(test simple-signle-module
  :description "Test building simple meta-node in single module."

  (with-module-table modules
    (build "/external(add, x, y)"
           "1+(n) : add(n, 1)"
           "/attribute(1+, removable, 0)")

    (let ((table (finish-build)))
      (with-nodes ((add "add") (fn "1+")) table
        (test-meta-node fn ((n "n")) `(,add ,n 1))
        (test-not-nodes (definition fn) '("add" "n" 1))))))

(test simple-multiple-modules
  :description "Test building simple meta-nodes in multiple modules"

  (with-module-table modules
    (build "/module(m1)"
           "/external(add, x, y)"

           "/module(m2)"
           "/import(builtin)"
           "/import(m1, add)"

           "1+(n) : add(n, 1)"
           "/attribute(1+, removable, 0)")

    (let ((table (finish-build)))
      (with-nodes ((add "add") (fn "1+")) table
        (test-meta-node fn ((n "n")) `(,add ,n 1))
        (test-not-nodes (definition fn) '(("/in" "m1" "add") "n" 1))))))


;;; Recursive Functions

;; The following tests also test that node-coalescing and constant
;; folding work within meta-node definitions.

(test recursive-simple
  :description "Test simple recursive meta-nodes."

  (with-module-table modules
    (build-core-module)

    (build "/import(core)"

           "fact(n) : {
                    # Additional nodes to test node coalescing
                    n -> m
                    m -> k

                    m - 1 -> next

                    # Additional nodes to test constant folding
                    2 -> two
                    two -> limit;

                    case(
                      k < limit : 1,
                      k * fact(next)
                    )
                  }"

           "/attribute(fact, removable, 0)")

    (let ((table (finish-build)))
      (with-nodes ((if "if") (- "-") (* "*") (< "<") (fact "fact")) table
        (test-meta-node fact ((n "n")) `(,if (,< ,n 2) 1 (,* ,n (,fact (,- ,n 1)))))
        (test-not-nodes (definition fact)
                        "m" "k" '(("/in" "core" "-") "m" 1) "next"
                        "two" "limit"
                        '#1=(("/in" "core" "<") "k" "limit")
                        '#2=("fact" "next")
                        '#3=(("/in" "core" "*") "k" #2#)
                        '(("/in" "core" "if") #1# 1 #3#))))))

(test recursive-tail
  :description "Test tail-recursive meta-nodes."

  (with-module-table modules
    (build-core-module)

    (build "/import(core)"

           "fact(n) : {
                    n -> m

                    iter(n, acc) : {
                      # Additional nodes to test node coalescing
                      n -> m
                      m -> k

                      m - 1 -> next

                      # Additional nodes to test constant folding
                      2 -> two
                      two -> limit

                      case(
                        k < limit : acc,
                        iter(next, k * acc)
                      )
                    }

                    iter(m, 1)
                  }"

           "/attribute(fact, removable, 0)")

    (let ((table (finish-build)))
      (with-nodes ((if "if") (- "-") (* "*") (< "<") (fact "fact")) table
        (with-nodes ((iter "iter")) (definition fact)
          (test-meta-node fact ((n "n")) `(,iter ,n 1))
          (test-meta-node iter ((n "n") (acc "acc"))
            `(,if (,< ,n 2) ,acc (,iter (,- ,n 1) (,* ,n ,acc))))

          (test-not-nodes (definition fact)
                          "m" '("iter" "m" 1))

          (test-not-nodes (definition iter)
                          "m" "k" '("-" "m" 1) "next"
                          "two" "limit"
                          '#1=(("/in" "core" "<") "k" "limit")
                          '#2=(("/in" "core" "*") "k" "acc")
                          '#3=("iter" "next" #2#)
                          '(("/in" "core" "if") #1# "acc" #3#)))))))

(test recursive-mutual
  :description "Test mutually recursive meta-nodes."

  (with-module-table modules
    (build-core-module)

    (build "/import(core)"
           "fib(n) :
                      case(
                        n > 1 : fib1(n) + fib2(n),
                        1
                      )"
           "fib1(n) : fib(n - 1)"
           "fib2(n) : fib(n - 2)"
           "fib(in) -> out"
           "/attribute(in, input, 1)")

    (let ((table (finish-build)))

      (with-nodes ((if "if") (- "-") (+ "+") (> ">")
                   (fib "fib") (fib1 "fib1") (fib2 "fib2")
                   (in "in") (out "out"))
          table

        (test-meta-node fib ((n "n"))
          `(,if (,> ,n 1) (,+ (,fib1 ,n) (,fib2 ,n)) 1))

        (test-meta-node fib1 ((n "n")) `(,fib (,- ,n 1)))
        (test-meta-node fib2 ((n "n")) `(,fib (,- ,n 2)))

        (test-not-nodes (definition fib)
                        '#1=(("/in" "core" ">") "n" "1")
                        '#2=(("/in" :init "fib1") "n")
                        '#3=(("/in" :init "fib2") "n")
                        '#4=(("/in" "core" "+") #2# #3#)
                        '(("/in" "core" "if") #1# #4# 1))

        (test-not-nodes (definition fib1)
                        '#5=(("/in" "core" "-") "n" "1")
                        '#6=((":in" /init "fib") #5#))

        (test-not-nodes (definition fib2)
                        '#7=(("/in" "core" "-") "n" "2")
                        '#8=(("/in" :init "fib") #7#))

        (has-value-function (in) out `(,fib ,in))))))

(test recursive-mutual-cross-module
  :description "Test mutually recursive meta-nodes across modules."

  (with-module-table modules
    (build-core-module)

    (build "/module(m1)"
           "/import(core)"
           "fib1(n) : { /use(m2); m2.fib(n - 1) }"
           "fib2(n) : { /use(m2); m2.fib(n - 2) }"

           "/module(m2)"
           "/import(core)"
           "/import(m1, fib1)"
           "/use(m1)"

           "fib(n) :
                      case(
                        n > 1 : fib1(n) + m1.fib2(n),
                        1
                      )"

           "fib(in) -> out"
           "/attribute(in, input, 1)")

    (let ((table (finish-build)))
      (with-nodes ((if "if") (- "-") (+ "+") (> ">")
                   (fib1 "fib1") (fib2 "fib2")
                   (fib "fib") (in "in") (out "out"))
          table

        (test-meta-node fib ((n "n"))
          `(,if (,> ,n 1) (,+ (,fib1 ,n) (,fib2 ,n)) 1))

        (test-meta-node fib1 ((n "n")) `(,fib (,- ,n 1)))
        (test-meta-node fib2 ((n "n")) `(,fib (,- ,n 2)))

        (test-not-nodes (definition fib)
                        '#1=(("/in" "core" ">") "n" "1")
                        '#2=(("/in" "m1" "fib1") "n")
                        '#3=(("/in" "m1" "fib2") "n")
                        '#4=(("/in" "core" "+") #2# #3#)
                        '(("/in" "core" "if") #1# #4# 1))

        (test-not-nodes (definition fib1)
                        '#5=(("/in" "core" "-") "n" "1")
                        '#6=(("/in" "m2" "fib") #5#))

        (test-not-nodes (definition fib2)
                        '#7=(("/in" "core" "-") "n" "2")
                        '#8=(("/in" "m2" "fib") #7#))

        (has-value-function (in) out `(,fib ,in))))))


;;; Self Node References

(test self-node-single-context
  :description "Test self node references in a single context."

  (with-module-table modules
    (build "Person(name, surname) : {
                  name -> self.first
                  surname -> self.last
                }"

           "/attribute(Person, removable, 0)")

    (let ((table (finish-build)))
      (with-nodes ((person "Person")) table
        (test-meta-node person ((name "name") (surname "surname"))
          `(:object (,(id-symbol "first") ,name)
                    (,(id-symbol "last") ,surname)))))))

(test self-node-multi-context
  :description "Test self node references in multiple contexts."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"
           "fn(x) : { x < 3 -> (x + 1 -> /context(self, c)); x + 2 -> /context(self, c) }"
           "/attribute(fn, removable, 0)")

    (let ((table (finish-build)))
      (with-nodes ((fn "fn") (< "<") (+ "+")) table
        (->>
         `(:catch
           ,(expression-block
             `(if (,< ,x 3)
                  (,+ ,x 1)
                  (:fail :none)))
           ,(expression-block
             `(,+ ,x 2))
           :none)
         (test-meta-node fn ((x "x"))))))))


;;; Outer Node References

(test outer-node-reference-explicit
  :description "Test explicit outer node references."

  (with-module-table modules
    (build-core-module)

    (build "/import(core)"

           "addx(a) : addy(..(x) + a)"
           "addy(a) : a + ..(y)"

           "x; y"

           "addx(in1) -> out1"
           "addy(in2) -> out2"

           "/attribute(x, input, 1)"
           "/attribute(y, input, 1)"
           "/attribute(in1, input, 1)"
           "/attribute(in2, input, 1)")

    (let ((table (finish-build)))

      (with-nodes ((+ "+")
                   (addx "addx") (addy "addy")
                   (x "x") (y "y")
                   (in1 "in1") (in2 "in2") (out1 "out1") (out2 "out2"))
          table

        (test-meta-node addy ((a "a") (y (outer y)))
          `(,+ ,a ,y))
        (test-meta-node addx ((a "a") (x (outer x)) (dy (outer y)))
          `(,addy (,+ ,x ,a) :outer (,y . ,dy)))

        (has-value-function (in2 (dy y)) out2
          `(,addy ,in2 :outer (,y . ,dy)))

        (has-value-function (in1 (dx x) (dy y)) out1
          `(,addx ,in1 :outer (,x . ,dx) (,y . ,dy)))))))

(test outer-node-reference-implicit
  :description "Test implicit outer node references."

  (with-module-table modules
    (build-core-module)

    (build "/import(core)"

           "addx(a) : addy(x + a)"
           "addy(a) : a + y"

           "x; y"

           "addx(in1) -> out1"
           "addy(in2) -> out2"

           "/attribute(x, input, 1)"
           "/attribute(y, input, 1)"
           "/attribute(in1, input, 1)"
           "/attribute(in2, input, 1)")

    (let ((table (finish-build)))

      (with-nodes ((+ "+")
                   (addx "addx") (addy "addy")
                   (x "x") (y "y")
                   (in1 "in1") (in2 "in2") (out1 "out1") (out2 "out2"))
          table

        (test-meta-node addy ((a "a") (y (outer y)))
          `(,+ ,a ,y))
        (test-meta-node addx ((a "a") (x (outer x)) (dy (outer y)))
          `(,addy (,+ ,x ,a) :outer (,y . ,dy)))

        (has-value-function (in2 (dy y)) out2
          `(,addy ,in2 :outer (,y . ,dy)))

        (has-value-function (in1 (dx x) (dy y)) out1
          `(,addx ,in1 :outer (,x . ,dx) (,y . ,dy)))))))

(test outer-node-reference-cross-module
  :description "Test outer node references across modules."

  (with-module-table modules
    (build-core-module)

    (build "/module(m1)"
           "/import(core)"

           "addx(a) : { /import(m2); addy(x + a) }"

           "y"
           "/attribute(y, input, 1)"

           "/module(m2)"
           "/import(core)"
           "/use(m1)"

           "addy(a) : a + m1.y"

           "x"
           "/export(x)"
           "/export(addy)"

           "m1.addx(in1) -> out1"
           "addy(in2) -> out2"

           "/attribute(x, input, 1)"
           "/attribute(in1, input, 1)"
           "/attribute(in2, input, 1)")

    (let ((table (finish-build)))
      (with-nodes ((+ "+") (addx "addx") (y "y")
                   (addy "addy") (x "x")
                   (in1 "in1") (in2 "in2")
                   (out1 "out1") (out2 "out2"))
          table

        (test-meta-node addy ((a "a") (y (outer y)))
          `(,+ ,a ,y))
        (test-meta-node addx ((a "a") (x (outer x)) (dy (outer y)))
          `(,addy (,+ ,x ,a) :outer (,y . ,dy)))

        (has-value-function (in2 (dy y)) out2
          `(,addy ,in2 :outer (,y . ,dy)))

        (has-value-function (in1 (dx x) (dy y)) out1
          `(,addx ,in1 :outer (,x . ,dx) (,y . ,dy)))))))

(test outer-node-reference-subnodes
  :description "Test references of subnodes of outer-nodes."

  (with-module-table modules
    (build-core-module)

    (build "/module(m1)"
           "/import(core)"

           "addx(a) : { /use(m2); a + m2.dict.x }"

           "/module(m2)"
           "/import(core)"
           "/use(m1)"

           "m1.addx(in2) -> out1"
           "dict.x -> out2"

           "/attribute(in2, input, 1)"

           "/module(m3)"
           "/import(core, ->, .)"
           "/import(m2, dict)"

           "in1 -> dict.x"
           "/attribute(in1, input, 1)")

    (let ((table (finish-build)))
      (with-nodes ((+ "+") (addx "addx")
                   (in1 "in1") (in2 "in2")
                   (dict "dict") (dict.x (:subnode "dict" "x"))
                   (out1 "out1"))
          table

        (test-meta-node addx ((a "a") (x (outer dict.x)))
          `(,+ ,a ,x))

        (has-value-function (in2 (x dict.x)) out1
          `(,addx ,in2 :outer (,dict.x . ,x)))

        (has-value-function (dict.x) dict
          `(:object (,(id-symbol "x") ,dict.x)))

        (test-simple-binding in1 dict.x)))))

(test outer-node-reference-mutual-recursion-explicit
  :description
  "Test explicit outer node references from mutually recursive meta-nodes."

  (with-module-table modules
    (build-core-module)

    (build "/import(core)"

           "addx(a) : addy(..(x) + a)"
           "addy(a) : addx(a + ..(y))"

           "x; y"

           "addx(in1) -> out1"
           "addy(in2) -> out2"

           "/attribute(x, input, 1)"
           "/attribute(y, input, 1)"
           "/attribute(in1, input, 1)"
           "/attribute(in2, input, 1)")

    (let ((table (finish-build)))
      (with-nodes ((+ "+")
                   (addx "addx") (addy "addy")
                   (x "x") (y "y")
                   (in1 "in1") (in2 "in2") (out1 "out1") (out2 "out2"))
          table

        (test-meta-node addy ((a "a") (in-x (outer x)) (in-y (outer y)))
          `(,addx (,+ ,a ,in-y) :outer (,x . ,in-x) (,y . ,in-y)))

        (test-meta-node addx ((a "a") (in-x (outer x)) (in-y (outer y)))
          `(,addy (,+ ,in-x ,a) :outer (,x . ,in-x) (,y . ,in-y)))

        (has-value-function (in1 (dx x) (dy y)) out1
          `(,addx ,in1 :outer (,x . ,dx) (,y . ,dy)))

        (has-value-function (in2 (dx x) (dy y)) out2
          `(,addy ,in2 :outer (,x . ,dx) (,y . ,dy)))))))

(test outer-node-reference-mutual-recursion-implicit
  :description
  "Test implicit outer node references from mutually recursive meta-nodes."

  (with-module-table modules
    (build-core-module)

    (build "/import(core)"

           "addx(a) : addy(x + a)"
           "addy(a) : addx(a + y)"

           "x; y"

           "addx(in1) -> out1"
           "addy(in2) -> out2"

           "/attribute(x, input, 1)"
           "/attribute(y, input, 1)"
           "/attribute(in1, input, 1)"
           "/attribute(in2, input, 1)")

    (let ((table (finish-build)))
      (with-nodes ((+ "+")
                   (addx "addx") (addy "addy")
                   (x "x") (y "y")
                   (in1 "in1") (in2 "in2") (out1 "out1") (out2 "out2"))
          table

        (test-meta-node addy ((a "a") (in-x (outer x)) (in-y (outer y)))
          `(,addx (,+ ,a ,in-y) :outer (,x . ,in-x) (,y . ,in-y)))

        (test-meta-node addx ((a "a") (in-x (outer x)) (in-y (outer y)))
          `(,addy (,+ ,in-x ,a) :outer (,x . ,in-x) (,y . ,in-y)))

        (has-value-function (in1 (dx x) (dy y)) out1
          `(,addx ,in1 :outer (,x . ,dx) (,y . ,dy)))

        (has-value-function (in2 (dx x) (dy y)) out2
          `(,addy ,in2 :outer (,x . ,dx) (,y . ,dy)))))))

(test outer-node-reference-mutual-recursion-cross-module
  :description
  "Test outer-node references in mutually recursive meta-nodes, across modules."

  (with-module-table modules
    (build-core-module)

    (build "/module(m1)"
           "/import(core)"

           "addx(a) : { /import(m2); addy(x + a) }"

           "y"
           "/attribute(y, input, 1)"

           "/module(m2)"
           "/import(core)"
           "/use(m1)"

           "addy(a) : m1.addx(a + m1.y)"

           "x"
           "/export(x)"
           "/export(addy)"

           "m1.addx(in1) -> out1"
           "addy(in2) -> out2"

           "/attribute(x, input, 1)"
           "/attribute(in1, input, 1)"
           "/attribute(in2, input, 1)")

    (let ((table (finish-build)))
      (with-nodes ((+ "+") (addx "addx") (y "y")
                   (addy "addy") (x "x")
                   (in1 "in1") (in2 "in2")
                   (out1 "out1") (out2 "out2"))
          table

        (test-meta-node addy ((a "a") (in-x (outer x)) (in-y (outer y)))
          `(,addx (,+ ,a ,in-y) :outer (,x . ,in-x) (,y . ,in-y)))

        (test-meta-node addx ((a "a") (in-x (outer x)) (in-y (outer y)))
          `(,addy (,+ ,in-x ,a) :outer (,x . ,in-x) (,y . ,in-y)))

        (has-value-function (in1 (dx x) (dy y)) out1
          `(,addx ,in1 :outer (,x . ,dx) (,y . ,dy)))

        (has-value-function (in2 (dx x) (dy y)) out2
          `(,addy ,in2 :outer (,x . ,dx) (,y . ,dy)))))))

(test outer-node-reference-nested-explicit
  :description
  "Test explicit outer-node references from nested meta-nodes."

  (with-module-table modules
    (build-core-module)

    (build "/import(core)"

           "count(end) : {
                      iter(n, acc) : {
                        case (
                          n < ..(end) : iter(n + 1, acc + n),
                          ..(start) + acc
                        )
                      }
                      iter(0, 0)
                    }"

           "start"
           "/attribute(start, input, 1)"

           "count(in) -> out"
           "/attribute(in, input, 1)")

    (let ((table (finish-build)))
      (with-nodes ((+ "+") (< "<") (if "if")
                   (count "count")
                   (start "start")
                   (in "in") (out "out"))
          table

        (with-nodes ((iter "iter") (end "end")) (definition count)

          (test-meta-node count ((in-end "end") (in-start (outer start)))
            `(,iter 0 0 :outer (,start . ,in-start) (,end . ,in-end)))

          (test-meta-node
              iter
              ((n "n") (acc "acc") (in-start (outer start)) (in-end (outer end)))

            `(,if (,< ,n ,in-end)
                  (,iter (,+ ,n 1) (,+ ,acc ,n) :outer (,start . ,in-start) (,end . ,in-end))
                  (,+ ,in-start ,acc))))

        (has-value-function (in (in-start start)) out
          `(,count ,in :outer (,start . ,in-start)))))))

(test outer-node-reference-nested-implicit
  :description
  "Test implicit outer-node references from nested meta-nodes."

  (with-module-table modules
    (build-core-module)

    (build "/import(core)"

           "count(end) : {
                      iter(n, acc) : {
                        case (
                          n < end : iter(n + 1, acc + n),
                          start + acc
                        )
                      }
                      iter(0, 0)
                    }"

           "start"
           "/attribute(start, input, 1)"

           "count(in) -> out"
           "/attribute(in, input, 1)")

    (let ((table (finish-build)))
      (with-nodes ((+ "+") (< "<") (if "if")
                   (count "count")
                   (start "start")
                   (in "in") (out "out"))
          table

        (with-nodes ((iter "iter") (end "end")) (definition count)

          (test-meta-node count ((in-end "end") (in-start (outer start)))
            `(,iter 0 0 :outer (,start . ,in-start) (,end . ,in-end)))

          (test-meta-node
              iter
              ((n "n") (acc "acc") (in-start (outer start)) (in-end (outer end)))

            `(,if (,< ,n ,in-end)
                  (,iter (,+ ,n 1) (,+ ,acc ,n) :outer (,start . ,in-start) (,end . ,in-end))
                  (,+ ,in-start ,acc))))

        (has-value-function (in (in-start start)) out
          `(,count ,in :outer (,start . ,in-start)))))))

(test outer-node-references-nested-cross-module
  :description
  "Test outer-node references from nested meta-nodes across modules."

  (with-module-table modules
    (build-core-module)

    (build "/module(m1)"
           "start; /export(start)"
           "/attribute(start, input, 1)"

           "/module(m2)"
           "/import(core)"
           "/import(m1)"

           "count(end) : {
                      iter(n, acc) : {
                        case (
                          n < ..(end) : iter(n + 1, acc + n),
                          ..(start) + acc
                        )
                      }
                      iter(0, 0)
                    }"

           "count(in) -> out"
           "/attribute(in, input, 1)")

    (let ((table (finish-build)))
      (with-nodes ((start "start")
                   (+ "+") (< "<") (if "if")
                   (count "count")
                   (in "in") (out "out"))
          table

        (with-nodes ((iter "iter") (end "end")) (definition count)

          (test-meta-node count ((in-end "end") (in-start (outer start)))
            `(,iter 0 0 :outer (,start . ,in-start) (,end . ,in-end)))

          (test-meta-node
              iter
              ((n "n") (acc "acc") (in-start (outer start)) (in-end (outer end)))

            `(,if (,< ,n ,in-end)
                  (,iter (,+ ,n 1) (,+ ,acc ,n) :outer (,start . ,in-start) (,end . ,in-end))
                  (,+ ,in-start ,acc))))

        (has-value-function (in (in-start start)) out
          `(,count ,in :outer (,start . ,in-start)))))))

(test outer-node-reference-optional-argument-default-value
  :description
  "Test outer-node reference from default value of meta-node optional argument."

  (with-module-table modules
    (build-core-module)

    (build "/import(core)"
           "dec(x, d : delta) : x - d"

           "fact(n) : case(n < 1 : 1, n * fact(dec(n)))"

           "fact(in) -> out"

           "/attribute(delta, input, 1)"
           "/attribute(in, input, 1)")

    (let ((table (finish-build)))
      (with-nodes ((* "*") (< "<") (if "if")
                   (dec "dec") (fact "fact")
                   (delta "delta") (in "in") (out "out"))
          table

        (test-meta-node fact
            ((n "n") (in-delta (outer delta)))

          `(,if (,< ,n ,1)
                1
                (,* ,n (,fact (,dec ,n ,in-delta) :outer (,delta . ,in-delta)))))

        (has-value-function (in (in-delta delta)) out
          `(,fact ,in :outer (,delta . ,in-delta)))))))

(test outer-node-reference-optional-argument
  :description
  "Test outer-node reference from default value of meta-node optional
   argument, when given value."

  (with-module-table modules
    (build-core-module)

    (build "/import(core)"
           "dec(x, d : delta) : x - d"

           "fact(n) : case(n < 1 : 1, n * fact(dec(n, 1)))"

           "fact(in) -> out"

           "/attribute(delta, input, 1)"
           "/attribute(in, input, 1)")

    (let ((table (finish-build)))
      (with-nodes ((* "*") (< "<") (if "if")
                   (dec "dec") (fact "fact")
                   (in "in") (out "out"))
          table

        (test-meta-node fact ((n "n"))
          `(,if (,< ,n ,1)
                1
                (,* ,n (,fact (,dec ,n 1)))))

        (has-value-function (in) out `(,fact ,in))))))

(test outer-node-reference-constant
  :description
  "Test constant folding of reference outer-nodes."

  (with-module-table modules
    (build "/external(add, a, b)"

           "inc(n) : add(n, delta)"
           "1 -> delta"

           "inc(x) -> y"
           "/attribute(x, input, 1)")

    (let ((table (finish-build)))
      (with-nodes ((add "add") (inc "inc")
                   (x "x") (y "y"))
          table

        (test-not-nodes table "delta")

        (test-meta-node
            inc
            ((n "n"))

          `(,add ,n 1))

        (has-value-function (x) y `(,inc ,x))))))

(test outer-node-reference-constant-functor
  :description
  "Test constant folding of constant functor outer nodes."

  (with-module-table modules
    (build "/external(add, a, b)"

           "inc(n) : add(n, delta)"
           "add(1, 3) -> delta"

           "inc(x) -> y"
           "/attribute(x, input, 1)")

    (let ((table (finish-build)))
      (with-nodes ((add "add") (inc "inc")
                   (x "x") (y "y"))
          table

        (test-not-nodes table "delta" '("add" 1 3))

        (test-meta-node
            inc
            ((n "n"))

          `(,add ,n (,add 1 3)))

        (has-value-function (x) y `(,inc ,x))))))

(test outer-node-reference-functor-of-constant
  :description
  "Test constant folding of outer node which is a functor of constant nodes."

  (with-module-table modules
    (build "/external(add, a, b)"

           "inc(n) : add(n, delta)"
           "add(2, 3) -> const"
           "add(1, const) -> delta"

           "inc(x) -> y"
           "/attribute(x, input, 1)")

    (let ((table (finish-build)))
      (with-nodes ((add "add") (inc "inc")
                   (x "x") (y "y"))
          table

        (test-not-nodes table
                        "delta" "const"
                        '("add" 1 "const")
                        '("add" 2 3))

        (test-meta-node
            inc
            ((n "n"))

          `(,add ,n (,add 1 (,add 2 3))))

        (has-value-function (x) y `(,inc ,x))))))

(test outer-node-reference-constant-in-functor
  :description
  "Test constant folding of functor outer node which is, within meta-node instance."

  (with-module-table modules
    (build "/external(add, a, b)"

           "inc(n) : add(n, delta)"
           "add(2, 3) -> const"
           "add(1, const) -> delta"

           "add(inc(x), z) -> y"

           "/attribute(x, input, 1)"
           "/attribute(z, input, 1)")

    (let ((table (finish-build)))
      (with-nodes ((add "add") (inc "inc")
                   (x "x") (y "y") (z "z"))
          table

        (test-not-nodes table
                        "delta" "const"
                        '("add" 1 "const") '("add" 2 3))

        (test-meta-node
            inc
            ((n "n"))

          `(,add ,n (,add 1 (,add 2 3))))

        (has-value-function (x z) y `(,add (,inc ,x) ,z))))))

(test outer-node-reference-constant-nested
  :description
  "Test constant folding of outer node in nested meta-nodes."

  (with-module-table modules
    (build-source-file #p"./test/inputs/builder/fold-outer-node-nested-1.trd" modules)

    (with-nodes ((add "add") (f "f")
                 (x "x") (y "y") (z "z"))
        (finish-build)

      (with-nodes ((inc "inc")) (definition f)
        (test-meta-node inc ((n "n"))
          `(,add ,n 5))

        (test-meta-node f ((x "x") (y "y"))
          `(,add (,inc ,x) (,inc ,y)))

        (has-value-function (x y) z
          `(,f ,x ,y))))))

(test outer-node-reference-constant-in-nested
  :description
  "Test constant outer node reference from nested meta-node."

  (with-module-table modules
    (build-source-file #p"./test/inputs/builder/fold-outer-node-nested-2.trd" modules)

    (with-nodes ((add "add") (f "f")
                 (x "x") (y "y") (z "z"))
        (finish-build)

      (with-nodes ((inc "inc")) (definition f)
        (test-not-nodes (definition f) "d" '("add" "delta" 1))

        (test-meta-node inc ((n "n"))
          `(,add ,n (,add 1 1)))

        (test-meta-node f ((x "x") (y "y"))
          `(,add (,inc ,x) (,inc ,y)))

        (has-value-function (x y) z
          `(,f ,x ,y))))))

(test outer-node-reference-constant-multi-meta-nodes
  :description
  "Test constant outer-node reference by meta-node used by multiple other meta-nodes."

  (with-module-table modules
    (build "/external(add, a, b)"

           "inc(n) : add(n, delta)"

           "f(x, y) : add(inc(x), inc(y))"
           "g(x, y) : inc(add(x, y))"

           "10 -> delta"

           "f(x, y) -> out1"
           "g(x, y) -> out2"

           "/attribute(x, input, 1)"
           "/attribute(y, input, 1)")

    (with-nodes ((add "add")
                 (inc "inc") (f "f") (g "g")
                 (x "x") (y "y")
                 (out1 "out1") (out2 "out2"))
        (finish-build)

      (test-meta-node inc ((n "n"))
        `(,add ,n 10))

      (test-meta-node f ((x "x") (y "y"))
        `(,add (,inc ,x) (,inc ,y)))

      (test-meta-node g ((x "x") (y "y"))
        `(,inc (,add ,x ,y)))

      (has-value-function (x y) out1
        `(,f ,x ,y))

      (has-value-function (x y) out2
        `(,g ,x ,y)))))

(test outer-node-reference-constant-compiled-meta-node
  :description
  "Test constant outer-node reference by compiled meta-node."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"

           "1 -> delta"
           "inc(n) : n + delta"

           "mac(x) : inc(x)"
           "/attribute(mac, macro, 1)"

           "inc(x) + mac(2) -> y"
           "/attribute(x, input, 1)")

    (with-nodes ((+ "+") (inc "inc")
                 (x "x") (y "y"))
        (finish-build)

      (test-meta-node inc ((n "n"))
        `(,+ ,n 1))

      (has-value-function (x) y
        `(,+ (,inc ,x) 3)))))

(test outer-node-reference-constant-meta-node-reference
  :description
  "Test constant outer-node reference in meta-node reference."

  (with-module-table modules
    (build "/external(add, a, b)"
           "/external(map, f, list)"

           "1 -> delta"
           "inc(n) : add(n, 1)"

           "inc-all(l) : map(inc, l)"

           "inc-all(x) -> y"
           "/attribute(x, input, 1)")

    (with-nodes ((add "add") (map "map")
                 (inc "inc") (inc-all "inc-all")
                 (x "x") (y "y"))
        (finish-build)

      (test-meta-node inc ((n "n"))
        `(,add ,n 1))

      (test-meta-node inc-all ((l "l"))
        `(,map ,(meta-node-ref inc) ,l))

      (has-value-function (x) y
        `(,inc-all ,x)))))


;;; Meta-Node References

(test meta-node-reference
  :description "Test meta-node reference."

  (with-module-table modules
    (build "/external(map, f, list); /external(add, x, y)"
           "1+(x) : add(x,1)"
           "1+-all(list) : map(1+, list)"

           "1+-all(in) -> out"
           "/attribute(in, input, 1)")

    (let ((table (finish-build)))
      (with-nodes ((1+ "1+") (1+-all "1+-all") (map "map")
                   (in "in") (out "out"))
          table

        (test-meta-node
            1+-all
            ((list "list"))

          `(,map ,(meta-node-ref 1+) ,list))

        (has-value-function (in) out `(,1+-all ,in))))))

(test meta-node-reference-outer-node
  :description "Test reference of meta-node with outer-nodes."

  (with-module-table modules
    (build "/external(map, f, list); /external(add, x, y)"
           "x+(n) : add(n, x)"
           "x+-all(list) : map(x+, list)"

           "x+-all(in) -> out"

           "/attribute(in, input, 1)"
           "/attribute(x, input, 1)")

    (let ((table (finish-build)))
      (with-nodes ((x+ "x+") (x+-all "x+-all") (map "map") (add "add")
                   (x "x") (in "in") (out "out"))
          table

        (test-meta-node x+ ((n "n") (x (outer x)))
          `(,add ,n ,x))

        (test-meta-node x+-all ((list "list") (in-x (outer x)))
          `(,map ,(meta-node-ref x+ :outer-nodes `((,x . ,in-x))) ,list))

        (has-value-function (in (in-x x)) out
          `(,x+-all ,in :outer (,x . ,in-x)))))))

(test meta-node-reference-outer-node-nested-1
  :description
  "Test reference of meta-node with outer-nodes from nested meta-node."

  (with-module-table modules
    (build "/external(map, f, list); /external(add, x, y)"
           "x+(n) : add(n, x)"
           "x+-all(list) : { add-x(n) : x+(n); map(add-x, list) }"

           "x+-all(in) -> out"

           "/attribute(in, input, 1)"
           "/attribute(x, input, 1)")

    (let ((table (finish-build)))
      (with-nodes ((x+ "x+") (x+-all "x+-all") (map "map") (add "add")
                   (x "x") (in "in") (out "out"))
          table

        (test-meta-node x+ ((n "n") (x (outer x)))
          `(,add ,n ,x))

        (with-nodes ((add-x "add-x"))
            (definition x+-all)

          (test-meta-node add-x ((n "n") (in-x (outer x)))
            `(,x+ ,n :outer (,x . ,in-x)))

          (test-meta-node x+-all ((list "list") (in-x (outer x)))
            `(,map ,(meta-node-ref add-x :outer-nodes `((,x . ,in-x))) ,list)))

        (has-value-function (in (in-x x)) out
          `(,x+-all ,in :outer (,x . ,in-x)))))))

(test meta-node-reference-outer-node-nested-2
  :description
  "Test reference of meta-node with outer-nodes from nested meta-node."

  (with-module-table modules
    (build "/external(map, f, list); /external(add, x, y)"
           "x+(n) : add(n, ..(x))"
           "x+-all(list) : { add-x(n) : map(x+, n); add-x(list);  }"

           "x+-all(in) -> out"

           "/attribute(in, input, 1)"
           "/attribute(x, input, 1)")

    (let ((table (finish-build)))
      (with-nodes ((x+ "x+") (x+-all "x+-all") (map "map") (add "add")
                   (x "x") (in "in") (out "out"))
          table

        (test-meta-node x+ ((n "n") (x (outer x)))
          `(,add ,n ,x))

        (with-nodes ((add-x "add-x"))
            (definition x+-all)

          (test-meta-node add-x ((n "n") (in-x (outer x)))
            `(,map ,(meta-node-ref x+ :outer-nodes `((,x . ,in-x))) ,n))

          (test-meta-node x+-all ((list "list") (in-x (outer x)))
            `(,add-x ,list :outer (,x . ,in-x))))

        (has-value-function (in (in-x x)) out
          `(,x+-all ,in :outer (,x . ,in-x)))))))

(test meta-node-reference-cycle
  :description "Test cyclic meta-node references."

  (with-module-table modules
    (build "/external(map, f, ..(lists)); /external(add, x, y)"
	   "add-x(xs) : map(add-y, xs, x)"
	   "add-y(xs) : map(add-x, xs, y)"

	   "add-x(in) -> output"

	   "/attribute(in, input, 1)"
	   "/attribute(x, input, 1)"
	   "/attribute(y, input, 1)")

    (let ((table (finish-build modules)))
      (with-nodes ((map "map") (add-x "add-x") (add-y "add-y")
		   (x "x") (y "y") (in "in") (output "output"))
	  table

        (test-meta-node add-x ((xs "xs") (xin (outer x)) (yin (outer y)))
          `(,map
            ,(meta-node-ref add-y :outer-nodes `((,x . ,xin) (,y . ,yin)))
            ,(argument-list (list xs xin))))

        (test-meta-node add-y ((xs "xs") (xin (outer x)) (yin (outer y)))
          `(,map
            ,(meta-node-ref add-x :outer-nodes `((,x . ,xin) (,y . ,yin)))
            ,(argument-list (list xs yin))))

	(let ((x-out x) (y-out y))
	  (has-value-function (in x y) output
            `(,add-x ,in :outer (,x-out . ,x) (,y-out . ,y))))))))

(test meta-node-reference-source
  :description
  "Test meta-node reference in binding source node."

  (with-module-table modules
    (build "/external(add, x, y)"
           "/external(map, f, list)"

           "add-x(n) : add(n, 1)"

           "add-x -> fn"
           "map(fn, in) -> out"

           "/attribute(in, input, 1)")

    (let ((table (finish-build modules)))
      (with-nodes ((add-x "add-x") (map "map")
                   (in "in") (out "out"))
          table

        (has-value-function (in) out `(,map ,(meta-node-ref add-x) ,in))))))

(test meta-node-reference-source-outer-nodes
  :description
  "Test meta-node reference (with outer-nodes) in binding source node."

  (with-module-table modules
    (build "/external(add, x, y)"
           "/external(map, f, list)"

           "add-x(n) : add(n, ..(x))"

           "add-x -> fn"
           "map(fn, in) -> out"

           "/attribute(in, input, 1)"
           "/attribute(x, input, 1)")

    (let ((table (finish-build modules)))
      (with-nodes ((add-x "add-x") (map "map")
                   (x "x") (in "in") (out "out"))
          table

        (has-value-function (in (x-in x)) out
          `(,map ,(meta-node-ref add-x :outer-nodes `((,x . ,x-in))) ,in))))))

(test meta-node-reference-optional-argument-default-constant-value
  :description
  "Test meta-node reference with constant optional argument default value"

  (with-module-table modules
    (build "/external(map, f, list)"
           "/external(+, x, y)"
           "/operator(+, 100)"

           "inc(x, d : 1) : x + d"
           "inc-all(xs) : map(inc, xs)"

           "inc-all(input) -> output"

           "/attribute(input, input, 1)")

    (with-nodes ((map "map") (inc "inc")
                 (inc-all "inc-all")
                 (input "input")
                 (output "output"))
        (finish-build)

      (test-meta-node
          inc-all
          ((xs "xs"))

        `(,map ,(meta-node-ref inc :optional '(1)) ,xs))

      (has-value-function
          (input)
          output

        `(,inc-all ,input)))))

(test meta-node-reference-optional-argument-default-node-value
  :description
  "Test meta-node reference with node optional argument default value"

  (with-module-table modules
    (build "/external(map, f, list)"
           "/external(+, x, y)"
           "/operator(+, 100)"

           "inc(x, d : delta) : x + d"
           "inc-all(xs) : map(inc, xs)"

           "inc-all(input) -> output"

           "/attribute(input, input, 1)"
           "/attribute(delta, input, 1)")

    (with-nodes ((map "map") (inc "inc")
                 (inc-all "inc-all")
                 (delta "delta") (input "input")
                 (output "output"))
        (finish-build)

      (test-meta-node inc-all ((xs "xs") (delta (outer delta)))
        `(,map ,(meta-node-ref inc :optional (list delta)) ,xs))

      (has-value-function (input (in-delta delta)) output
        `(,inc-all ,input :outer (,delta . ,in-delta))))))


;;; Node Auto Creation

(test node-auto-create-target-operand
  :description
  "Test auto-creation of nodes as operands in functor in binding target."

  (with-module-table modules
    (build "/external(int, x)"
           "/attribute(int, target-node, int)"
           "f(x) : { x -> int(y); y }"
           "/attribute(f, removable, 0)")

    (let ((table (finish-build)))
      (with-nodes ((f "f") (int "int"))
          table

        (test-meta-node f ((x "x")) `(,int ,x))))))

(test node-auto-create-atom-declaration
  :description
  "Test auto-creation of nodes in atom node declarations."

  (with-module-table modules
    (build "/external(add,a,b)"
           "f(x) : { z; add(x,1) -> z; z; }"
           "/attribute(f, removable, 0)")

    (with-nodes ((f "f") (add "add")) (finish-build)
      (test-meta-node f ((x "x"))
        `(,add ,x 1)))))

(test node-auto-create-error-operands
  :description
  "Test error raised when unknown node referenced in functor operands in binding source."

  (with-module-table modules
    (build "/external(add, x, y)"
           "f(x) : add(x, y, z)"
           "/attribute(f, removable, 0)")

    (signals non-existent-node-error (finish-build))))

(test node-auto-create-error-binding-source
  :description
  "Test error raised when unknown node referenced in binding source."

  (with-module-table modules
    (build "f(x) : z -> self"
           "/attribute(f, removable, 0)")

    (signals non-existent-node-error (finish-build))))

(test node-auto-create-error-object-nodes
  :description
  "Test error raised when subnode of unknown node referenced."

  (with-module-table modules
    (build "f(x) : n.z"
           "/attribute(f, removable, 0)")

    (signals non-existent-node-error (finish-build))))


;;; Cyclic References

(test cyclic-references
  :description
  "Test cyclic references in meta-nodes."

  (with-module-table modules
    (build-source-file "./test/inputs/builder/cyclic-references.trd" modules)

    (with-nodes ((f "f") (cons "cons")) (finish-build)
      (test-meta-node f ((a "a") (b "b"))
        (aprog1 (expression-block nil :count 2)
          (setf (expression-block-expression it)
                `(,cons ,a (,cons ,b ,(cyclic-reference it)))))))))


;;; Structure Checking

(test ambiguous-context-check-simple
  :description "Test checking for ambiguous contexts in simple operands."

  (with-module-table modules
    (build "f(a) : {
                      a -> d
                      a -> b
                      b -> c
                      c -> d
                      d -> e
                      e
                    }"
           "/attribute(f, removable, 0)")

    (signals ambiguous-context-error (finish-build))))

(test ambiguous-context-check-self-node
  :description "Test checking for ambiguous contexts in self nodes."

  (with-module-table modules
    (build "f(a, b) : {
                      a -> self
                      b -> self
                    }"
           "/attribute(f, removable, 0)")

    (signals ambiguous-context-error (finish-build))))

(test ambiguous-context-check-functor
  :description "Test checking for ambiguous contexts in functor nodes."

  (with-module-table modules
    (build "/external(add, x, y)"

           "f(a, b) : {
                      add(a, b) -> d
                      a -> c; c -> d
                      d
                    }"
           "/attribute(f, removable, 0)")

    (signals ambiguous-context-error (finish-build))))

(test ambiguous-context-check-functor-self
  :descritpion "Test checking for ambiguous contexts in functor nodes bound to `self`."

  (with-module-table modules
    (build "/external(add, x, y)"

           "f(a, b) : {
                      a -> self
                      b -> self
                      add(a, b) -> self
                    }"
           "/attribute(f, removable, 0)")

    (signals ambiguous-context-error (finish-build))))

(test mutliple-contexts-multiple-operands
  :description
  "Test checking for multiple contexts activated by multiple operands."

  (with-module-table modules
    (build "f(a, b) : { a -> c; b -> c; c }")

    (signals ambiguous-context-error (finish-build))))

(test node-states-in-meta-node
  :description
  "Test that using node states within a meta-node results in an error."

  (with-module-table modules
    (build-core-module)
    (build-source-file "./test/inputs/builder/states-in-meta-node.trd" modules)
    (signals ambiguous-context-error (finish-build))))


;;; Constant Functions

(test constant-meta-node-ignore-arguments
  :description "Test constant meta-nodes with ignored arguments."

  (with-module-table modules
    (build "f(x) : 1"
           "/attribute(f, removable, 0)")

    (let ((table (finish-build)))
      (with-nodes ((f "f")) table
        (test-value-function f (car (first (contexts f))) 1)))))


;;; Errors

(test error-meta-node-without-function
  :description
  "Test that a meta-node without a function results in an error."

  (with-module-table modules
    (build "f(x) : x -> z"

           "f(in) -> out"
           "/attribute(in, input, 1)")

    (signals meta-node-no-function-error (finish-build))))
