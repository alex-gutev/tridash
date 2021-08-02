;;;; coalescer.lisp
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

;;;; Unit tests for node coalescing

(defpackage :tridash/test.coalescer
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
                :*node-false*)

  (:local-nicknames (:ir :tridash.ir)))

(in-package :tridash/test.coalescer)


(def-suite coalescer
    :description "Test coalescing of nodes"
    :in frontend)

(in-suite coalescer)


;;; Atom Nodes

(test atom-node-coalesce-one-way-binding
  "Test node coalescing in one-way bindings between atom nodes."

  (with-module-table modules
    (build "a -> b; b -> cc; cc -> d"
	   "/attribute(a, input, 1)")

    (let ((table (finish-build)))
      (test-not-nodes table "b" "cc")

      (with-nodes ((a "a") (d "d")) table
	(test-simple-binding a d)))))

(test atom-node-coalesce-one-way-binding-coalescable-attribute
  "Test node coalescing of atom nodes, in one-way bindings, with coalescable attribute."

  (with-module-table modules
    (build "a -> b; b -> cc; cc -> d"
           "/attribute(a, input, 1)"
           "/attribute(b, coalescable, 0)")

    (let ((table (finish-build)))
      (test-not-nodes table "cc")

      (with-nodes ((a "a") (b "b") (d "d")) table
	(test-simple-binding a b)
	(test-simple-binding b d)))))

(test atom-node-coalesce-two-way-binding
  "Test node coalescing in two-way bindings between atom nodes."

  (with-module-table modules
    (build "a -> b; b -> cc; cc -> d"
           "d -> cc; cc -> b; b -> a"
           "/attribute(a, input, 1)")

    (let ((table (finish-build)))
      (test-not-nodes table "b" "cc")

      (with-nodes ((a "a") (d "d")) table
	(test-simple-binding a d)))))

(test atom-node-coalesce-one-way-binding-coalescable-attribute
  "Test node coalescing of atom nodes, in two-way bindings, with coalescable attribute."

  (with-module-table modules
    (build "a -> b; b -> cc; cc -> d"
           "d -> cc; cc -> b; b -> a"
           "/attribute(a, input, 1)"
           "/attribute(b, input, 1)")

    (let ((table (finish-build)))
      (test-not-nodes table "cc")

      (with-nodes ((a "a") (b "b") (d "d")) table
        (test-simple-binding a b)
        (test-simple-binding b a)
	(test-simple-binding b d)))))

(test atom-node-coalesce-multiple-observers
  "Test node coalescing of atom nodes with multiple observer nodes."

  (with-module-table modules
    (build "a -> b; b -> cc; b -> d; cc -> e; d -> f"
           "/attribute(a, input, 1)")

    (let ((table (finish-build)))
      (test-not-nodes table "cc" "d")

      (with-nodes ((a "a") (b "b") (e "e") (f "f")) table
        (test-simple-binding a b)
        (test-simple-binding b e)
        (test-simple-binding b f)))))


;;; Functor Nodes

(test functor-node-coalesce-simple
  "Test coalescing of basic functor nodes."

  (with-module-table modules
    (build "/external(+, x, y); /operator(+, 50, left)"
           "a + b + cc + d -> output"
           "/attribute(a, input, 1)"
           "/attribute(b, input, 1)"
           "/attribute(cc, input, 1)"
           "/attribute(d, input, 1)")

    (let ((table (finish-build)))
      (test-not-nodes table
                      '("+" "a" "b")
                      '("+" ("+" "a" "b") "cc")
                      '("+" ("+" ("+" "a" "b") "cc") "d"))

      (with-nodes ((+ "+") (a "a") (b "b") (cc "cc") (d "d") (output "output")) table
        (has-value-function (a b cc d) output `(,+ (,+ (,+ ,a ,b) ,cc) ,d))))))

(test functor-node-coalesce-coalescable-attribute
  "Test coalescing of functor nodes with coalescable attributes."

  (with-module-table modules
    (build "/external(+, x, y); /operator(+, 50, left)"
           "a + b + cc + d -> output"
           "/attribute(a, input, 1)"
           "/attribute(b, input, 1)"
           "/attribute(cc, input, 1)"
           "/attribute(d, input, 1)"
           "/attribute(a + b, coalescable, 0)")

    (let ((table (finish-build)))
      (test-not-nodes table
                      '("+" ("+" "a" "b") "cc")
                      '("+" ("+" ("+" "a" "b") "cc") "d"))

      (with-nodes ((+ "+") (a "a") (b "b") (cc "cc") (d "d")
                   (a+b ("+" "a" "b"))
                   (output "output"))
          table

        (has-value-function (a b) a+b `(,+ ,a ,b))
        (has-value-function (a+b cc d) output `(,+ (,+ ,a+b ,cc) ,d))))))

(test functor-node-coalesce-multiple-observers
  "Test coalescing of functor nodes with multiple observers."

  (with-module-table modules
    (build "/external(+, x, y); /operator(+, 50, left)"
           "a + b + cc + d -> out1"
           "a + b -> out2"
           "/attribute(a, input, 1)"
           "/attribute(b, input, 1)"
           "/attribute(cc, input, 1)"
           "/attribute(d, input, 1)")

    (let ((table (finish-build)))
      (test-not-nodes table
                      '("+" ("+" "a" "b") "cc")
                      '("+" ("+" ("+" "a" "b") "cc") "d"))

      (with-nodes ((+ "+") (a "a") (b "b") (cc "cc") (d "d")
                   (a+b ("+" "a" "b"))
                   (out1 "out1") (out2 "out2"))
          table

        (has-value-function (a+b cc d) out1 `(,+ (,+ ,a+b ,cc) ,d))

        (test-simple-binding a+b out2)
        (has-value-function (a b) a+b `(,+ ,a ,b))))))

(test functor-node-coalesce-non-input
  "Test coalescing of functor nodes of non input nodes."

  (with-module-table modules
    (build "/external(add, x, y)"
           "a -> b; b -> cc; b -> d; add(cc,d) -> e"
           "/attribute(a, input, 1)")

    (let ((table (finish-build)))
      (test-not-nodes table "b" "cc" "d")

      (with-nodes ((add "add") (a "a") (e "e")) table
        (has-value-function (a) e (list add a a))))))


;;; Subnodes

(test subnode-coalesce
  "Test coalescing of subnodes."

  (with-module-table modules
    (build "/external(parse, x); /external(not, x)"
           "parse(in1) -> p"

           "not(p.fail) -> (p.value -> a)"
           "p.fail -> (in2 -> b)"

           "/attribute(in1, input, 1)"
           "/attribute(in2, input, 1)")

    (let ((table (finish-build)))
      (test-not-nodes table
                      '("parse" "in1")
                      '(:subnode "p" "value")
                      '("not" (:subnode "p" "fail"))
                      '(:bind (:subnode "p" "value") "a")
                      '(:bind "in2" "b"))

      (with-nodes ((in1 "in1") (in2 "in2") (p "p")
                   (p.fail (:subnode "p" "fail"))
                   (a "a") (b "b")

                   (parse "parse") (not "not"))
          table

        (has-value-function
            (p p.fail) a
          `(if (,not ,p.fail) (:member ,p ,(id-symbol "value")) (:fail :none)))

        (has-value-function
            (in2 p.fail) b
          `(if ,p.fail ,in2 (:fail :none)))

        (has-value-function (in1) p `(,parse ,in1))
        (is (length (contexts p)) 1 "Node p has a single context.")))))


;;; Cycles

(test cycle-coalesce-non-coalescable
  "Test coalescing of cycle of non-coalescable nodes."

  (with-module-table modules
    (build "/external(cons, a, b)"
           "cons(a, y) -> x"
           "cons(b, x) -> y"

           "x -> output"

           "/attribute(a, input, 1)"
           "/attribute(b, input, 1)"
           "/attribute(x, coalescable, 0)"
           "/attribute(y, coalescable, 0)")

    (let ((table (finish-build)))
      (test-not-nodes
       table
       '("cons" "a" "y")
       '("cons" "b" "x"))

      (with-nodes ((cons "cons")
                   (a "a") (b "b")
                   (x "x") (y "y")
                   (output "output"))
          table

        (has-value-function (a y) x
          `(,cons ,a ,y))

        (has-value-function (b x) y
          `(,cons ,b ,x))

        (test-binding x output)))))

(test cycle-coalesce-coalescable
  "Test coalescing of cycle of coalescable nodes."

  (with-module-table modules
    (build "/external(cons, a, b)"
           "cons(a, y) -> x"
           "cons(b, x) -> y"

           "x -> output"

           "/attribute(a, input, 1)"
           "/attribute(b, input, 1)")

    (let ((table (finish-build)))
      (test-not-nodes
       table
       "x" "y"
       '("cons" "a" "y")
       '("cons" "b" "x"))

      (with-nodes ((cons "cons")
                   (a "a") (b "b")
                   (output "output"))
          table

        (let ((block (ir:expression-block nil :count 2)))
          (has-value-function (a b) output
            (progn
              (setf (ir:expression block)
                    `(,cons ,a (,cons ,b ,(ir:cyclic-reference block))))

              block)))))))


;;; Removing Unreachable Nodes

(test remove-unreachable
  "Test removal of unreachable nodes."

  (with-module-table modules
    (build "a -> b; b -> cc;"
           "e -> f"                     ; Unreachable Nodes
           "/attribute(a, input, 1)")

    (let ((table (finish-build)))
      (test-not-nodes table "b" "e" "f")

      (with-nodes ((a "a") (cc "cc")) table
        (test-simple-binding a cc)))))

(test remove-unreachable-non-removable
  "Test removable attribute, when removing unreachable nodes."

  (with-module-table modules
    (build "some-special-node"
           "/attribute(some-special-node, removable, 0)")

    (let ((table (finish-build)))
      (test-nodes table "some-special-node"))))

(test unreachable-dependency-errors
  "Test that unreachable dependency error is signalled."

  (with-module-table modules
    (build "/external(add, x, y)"
           "a -> b; add(b, d) -> output"
           "cc -> d"                    ; Unreachable Nodes
           "/attribute(a, input, 1)")

    (signals dependency-not-reachable-error (finish-build))))


;;; Removing Unused Meta-Nodes

(test unused-meta-node-remove-all
  "Test removing of all unreachable meta-nodes in a module."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"
           "f(x) : x"
           "x -> y"
           "/attribute(x, input, 1)")

    (let ((table (finish-build)))
      (is (length (meta-nodes table)) 0))))

(test unused-meta-node-keep-used
  "Test that used meta-nodes are kept, with remainder removed."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, +)"
           "1+(x) : x + 1"

           "1+(x) -> y"
           "/attribute(x, input, 1)")

    (let ((table (finish-build)))
      (with-nodes ((1+ "1+") (x "x") (y "y")) table
        (has-value-function (x) y `(,1+ ,x))
        (is (length (meta-nodes table)) 2)))))

(test unsued-meta-node-keep-referenced
  "Test that meta-nodes, referenced as values, are kept."

  ;; Additionally this also tests that nodes used by used
  ;; meta-nodes are not removed.

  (with-module-table modules
    (build-core-module)
    (build "/import(core, +, map)"
           "1+(x) : x + 1"
           "f(z) : 1+(z)"

           "map(f, a) -> b"
           "/attribute(a, input, 1)")

    (let ((table (finish-build)))
      (with-nodes ((1+ "1+") (f "f") (map "map")
                   (a "a") (b "b"))
          table

        (has-value-function
            (a)
            b
          `(,map ,(ir:meta-node-ref f) ,a))

        (test-meta-node f ((z "z")) `(,1+ ,z))))))


;;; Cross-Module Bindings

(test cross-module-bindings
  "Test coalescing of cross-module bindings."

  (with-module-table modules
    (build "/module(m1)"
           "/import(builtin)"

           "a -> b; b -> cc; cc -> d"
           "/attribute(a, input, 1)")

    (build "/module(m2)"
           "/import(builtin)"
           "/use(m1)"

           "m1.b -> a; b -> cc; cc -> d"
           "/attribute(b, input, 1)")

    (let ((table (finish-build)))
      (with-modules ((m1 "m1") (m2 "m2")) modules
        (test-not-nodes table "cc")

        (with-nodes ((m1.a "a") (m1.b "b") (m1.d "d")) m1
          (test-simple-binding m1.a m1.b)
          (test-simple-binding m1.b m1.d)

          (with-nodes ((m2.a "a") (m2.b "b") (m2.d "d")) m2
            (test-simple-binding m1.b m2.a)
            (test-simple-binding m2.b m2.d)))))))

(test cross-module-bindings-functor-node
  "Test coalescing of cross-module bindings of functor nodes."

  (with-module-table modules
    (build "/module(m1)"
           "/import(builtin)"

           "in -> a; in -> b"
           "/attribute(in, input, 1)")

    (build "/module(m2)"
           "/import(builtin)"
           "/use(m1)"

           "/external(+, x, y); /operator(+, 50, left)"
           "m1.a + m1.b + cc -> output"
           "/attribute(cc, input, 1)")

    (let ((table (finish-build)))
      (with-modules ((m1 "m1") (m2 "m2")) modules
        (test-not-nodes
         table
         "a" "b"
         '(("/in" "m2" "+")
           (("/in" "m2" "+") ("/in" "m1" "a") ("/in" "m1" "b"))
           ("/in" "m2" "cc")))

        (with-nodes ((in "in")) m1
          (with-nodes ((+ "+") (cc "cc") (output "output")) m2
            (has-value-function (in cc) output `(,+ (,+ ,in ,in) ,cc))))))))


;;; Common Sub-Expressions

(test common-sub-expressions
  "Test that common sub-expressions are extracted in a single EXPRESSION-BLOCK."

  (with-module-table modules
    (build "/external(parse, x); /external(NaN?, x)"

           "parse(in) -> p"
           "p -> self.value"
           "NaN?(p) -> self.fail"
           "self -> out"

           "/attribute(in, input, 1)")

    (let ((table (finish-build)))
      (test-not-nodes table
                      '("parse" "in")
                      "p"
                      '("NaN?" "p")
                      "self"
                      '(:subnode "self" "value")
                      '(:subnode "self" "fail"))

      (with-nodes ((in "in") (out "out")
                   (parse "parse") (nan? "NaN?"))
          table

        (has-value-function
            (in) out
          `(:object
            (,(id-symbol "fail")
              (,nan? ,(ir:expression-block `(,parse ,in) :count 2)))

            (,(id-symbol "value")
              ,(ir:expression-block `(,parse ,in) :count 2))))))))

(test common-sub-expressions-conditional-bindings
  "Test extraction of common sub-expressions in conditional bindings."

  (with-module-table modules
    (build "/external(add, x, y); /external(even?, x)"

           "add(a, b) -> cc"
           "even?(add(a, 1)) -> (add(a, 1) -> b)"

           "add(cc, d) -> out"

           "/attribute(a, input, 1)"
           "/attribute(d, input, 1)")

    (let ((table (finish-build)))
      (test-not-nodes table
                      '("add" "a" 1)
                      '("add" "a" "b")
                      "b"
                      "cc"
                      '("even?" ("add" "a" 1))
                      '(:bind ("add" "a" 1) "b")
                      '("add" "cc" "d"))

      (with-nodes ((a "a") (d "d") (out "out")
                   (add "add") (even? "even?"))
          table

        (has-value-function
            (a d) out
          `(,add (,add ,a ,(ir:expression-block `(if (,even? (,add ,a ,1)) (,add ,a 1) (:fail :none)))) ,d))))))


;;; Explicit Contexts

(test explicit-context-common-operands
  "Test coalescing of nodes with multiple explicit contexts with common operands."

  (with-module-table modules
    (build-core-module)
    (build "/import(core);"
           "a < 3 -> ((a + b) -> /context(output, ctx))"
           "a > 4 -> ((a - b) -> /context(output, ctx))"
           "b -> /context(output, ctx)"

           "/attribute(a, input, 1)"
           "/attribute(b, input, 1)")

    (let ((table (finish-build)))
      (with-nodes ((+ "+") (- "-") (< "<") (> ">")
                   (a "a") (b "b") (output "output"))
          table

        (has-value-function
            (a b)
            output

          `(:catch
            (:catch
             ,(ir:expression-block
               `(if (,< ,a 3) (,+ ,a ,b) (:fail :none)))
             ,(ir:expression-block
               `(if (,> ,a 4) (,- ,a ,b) (:fail :none)))
             :none)
            ,b
            :none))))))

(test explicit-context-common-operands-non-coalescable
  "Test coalescing of nodes with multiple explicit contexts with non-coalescable operands."

  (with-module-table modules
    (build-core-module)
    (build "/import(core);"
           "a + b -> cc"
           "a < 3 -> (cc -> /context(output, ctx1))"
           "b -> /context(output, ctx1)"

           "/attribute(a, input, 1)"
           "/attribute(b, input, 1)"
           "/attribute(cc, coalescable, 0)")

    (let ((table (finish-build)))
      (with-nodes ((< "<")
                   (a "a") (b "b") (cc "cc")
                   (output "output"))
          table

        (has-value-function
            (a b cc)
            output

          `(:catch
            ,(ir:expression-block
              `(if (,< ,a 3) ,cc (:fail :none)))
            ,b
            :none))))))

(test explicit-context-common-operands-no-default
  "Test coalescing of nodes without default context, with common operands among other contexts."

  (with-module-table modules
    (build-core-module)
    (build "/import(core);"
           "a < 3 -> ((a + b) -> /context(output, cc))"
           "b < 4 -> (b -> /context(output, cc))"

           "/attribute(a, input, 1)"
           "/attribute(b, input, 1)")

    (let ((table (finish-build)))
      (with-nodes ((+ "+") (< "<")
                   (a "a") (b "b") (output "output"))
          table

        (has-value-function
            (a b)
            output

          `(:catch
            ,(ir:expression-block
              `(if (,< ,a 3) (,+ ,a ,b) (:fail :none)))
            ,(ir:expression-block
              `(if (,< ,b 4) ,b (:fail :none)))
            :none))))))

(test explicit-context-single-common-ancestor
  "Test coalescing of a node with explicit context activated by single common ancestor."

  (with-module-table modules
    (build-core-module)
    (build "/import(core);"
           "a < 3 -> ((a + b) -> cc)"
           "cc -> /context(output, ctx)"
           "b -> /context(output, ctx)"

           "/attribute(a, input, 1)"
           "/attribute(b, input, 1)")

    (let ((table (finish-build)))
      (with-nodes ((+ "+") (< "<")
                   (a "a") (b "b") (output "output"))
          table

        (has-value-function
            (a b)
            output

          `(:catch
            ,(ir:expression-block
              `(if (,< ,a 3) (,+ ,a ,b) (:fail :none)))
            ,b
            :none))))))

(test explicit-context-single-common-ancestor-no-default
  "Test coalescing of a node with explicit context activated by single
  common ancestor, without default context."

  (with-module-table modules
    (build-core-module)
    (build "/import(core);"
           "a < 3 -> ((a + b) -> cc)"
           "cc -> /context(output, ctx)"
           "b < 4 -> (b -> /context(output, ctx))"

           "/attribute(a, input, 1)"
           "/attribute(b, input, 1)")

    (let ((table (finish-build)))
      (with-nodes ((+ "+") (< "<")
                   (a "a") (b "b") (output "output"))
          table

        (has-value-function
            (a b)
            output

          `(:catch
            ,(ir:expression-block
              `(if (,< ,a 3) (,+ ,a ,b) (:fail :none)))
            ,(ir:expression-block
              `(if (,< ,b 4) ,b (:fail :none)))
            :none))))))

(test explicit-context-creation-order
  "Test explicit context creation order."

  (with-module-table modules
    (build-core-module)
    (build "/import(core);"
           "a + b -> /context(output, c1)"
           "b -> /context(output, c1)"
           "a < 3 -> ((a + b) -> /context(output, c1))"

           "/attribute(a, input, 1)"
           "/attribute(b, input, 1)")

    (let ((table (finish-build)))
      (with-nodes ((+ "+") (< "<")
                   (a "a") (b "b") (output "output"))
          table

        (has-value-function
            (a b)
            output

          `(:catch
            (if (,< ,a 3) (,+ ,a ,b) (:fail :none))
            ,b
            :none))))))


;;; Node States

(test node-state-from-any-state
  "Test coalescing of nodes with states, with from any state transition."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, not)"
           "not on -> /state(on, toggle)"
           "start -> on"

           "the-state -> /state(on)"

           "/attribute(start, input, 1)"
           "/attribute(the-state, input, 1)")

    (with-nodes ((on "on")
                 (toggle-state-on ("/state" "on" "toggle"))
                 (the-state "the-state")
                 (start "start")

                 (if "if") (not "not"))

        (finish-build modules)

      (let ((equal (get :symbol-equal tridash.frontend::*core-meta-nodes*))
            (previous (get :previous-value tridash.frontend::*core-meta-nodes*)))

        (has-value-function (the-state (toggle toggle-state-on))
            on

          `(,if (,equal ,the-state ,(id-symbol "toggle"))
                (,previous ,(ir:node-ref toggle-state-on))
                (,previous ,(ir:node-ref on))))

        (test-binding start on)


        (has-value-function (on)
            toggle-state-on
          `(,not ,on))))))

(test node-state-specific-state
  "Test coalescing of nodes with specific state transition."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, not)"
           "not on -> /state(on, default, toggle)"
           "start -> on"

           "the-state -> /state(on)"

           "/attribute(start, input, 1)"
           "/attribute(the-state, input, 1)")

    (with-nodes ((on "on")
                 (state-on ("/state" "on"))
                 (toggle-state-on ("/state" "on" "default" "toggle"))
                 (the-state "the-state")
                 (start "start")

                 (if "if") (not "not"))

        (finish-build modules)

      (let ((equal (get :symbol-equal tridash.frontend::*core-meta-nodes*))
            (previous (get :previous-value tridash.frontend::*core-meta-nodes*)))

        (has-value-function ((state state-on) (toggle toggle-state-on))
            on

          `(,if
            (,if (,equal (,previous ,(ir:node-ref state-on)) ,(id-symbol "default"))
                 (,equal ,state ,(id-symbol "toggle"))
                 ,(ir:node-ref *node-false*))

            (,previous ,(ir:node-ref toggle-state-on))
            (,previous ,(ir:node-ref on))))

        (test-binding start on)


        (has-value-function (on)
            toggle-state-on
          `(,not ,on))

        (test-binding the-state state-on)))))
