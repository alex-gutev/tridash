;;;; frontend.lisp
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

;;;; Unit tests which test that the frontend node definitions are
;;;; built correctly.

(defpackage :tridash/test.frontend
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

(in-package :tridash/test.frontend)


(def-suite builder
    :description "Test building of frontend node definitions."
    :in frontend)

(in-suite builder)


;;; Atom Nodes and Bindings

(test atom-nodes
  "Test building of atom nodes."

  (with-module-table modules
    (build "a;b;cc")

    (test-nodes modules "a" "b" "cc")))

(test bindings
  "Test building of bindings between nodes."

  (with-module-table modules
    (build "a -> b; cc -> b")

    (with-nodes ((a "a") (b "b") (cc "cc")) modules
      (test-simple-binding a b :context a)
      (test-simple-binding cc b :context cc))))

(test binding-bindings
  "Test building binding to a binding (conditional bindings)"

  (with-module-table modules
    (build "a -> (b -> cc)")

    (with-nodes ((a "a") (b "b") (cc "cc")
                 (b->cc (:bind "b" "cc")))
        modules

      (test-simple-binding a b->cc :context a)

      (let ((*strict-test* nil))
	(has-value-function
	    ((b :context b) (b->cc :context b))
	    cc

	  `(if ,b->cc ,b (:fail :none)))))))


;;; Functor Nodes

(test functor-nodes-meta-node-operator
  "Test building functor nodes with meta-node operator."

  (with-module-table modules
    (build-core-module)
    (build "/import(core); a + b -> output; int(x) -> z")

    (with-nodes ((+ "+") (a "a") (b "b")
                 (a+b (("/in" "core" "+") "a" "b")) (output "output"))
        modules

      (test-simple-binding a+b output :context a+b)
      (test-node-function a+b + + a b))

    (with-nodes ((int "int") (x "x") (z "z") (int-x (("/in" "core" "int") "x"))) modules
      (test-simple-binding int-x z :context int-x)

      (test-node-function int-x int int x))))

(test functor-nodes-node-operator
  "Test building functor nodes with node operator."

  (with-module-table modules
    (build "in1 -> fn"
           "fn(in2) -> output")

    (with-nodes ((in1 "in1") (in2 "in2")
                 (fn "fn") (output "output")
                 (fn-in2 ("fn" "in2")))
        modules

      (test-simple-binding in1 fn)
      (test-simple-binding fn-in2 output)

      (has-value-function
          (fn in2)
          fn-in2

        `(,fn ,in2)))))

(test functor-nodes-non-node-operator
  "Test that error is signalled when building functor nodes with non-node operators."

  (test-error non-node-operator-error "1(x, y)")
  (test-error non-node-operator-error " \"hello\"(1, x, 2) ")
  (test-error non-existent-node-error "z(x, y)"))


;;; Functor Nodes as Binding Target

(test target-functor-no-target-node
  "Test building binding target functor node with no target node."

  (test-error target-node-error "/external(add, x, y); a -> add(b, cc)"))

(test target-functor-with-target-node
  "Test building binding target functor node with target node."

  (with-module-table modules
    (build "/external(add, x, y)"
           "/external(reverse-add, sum)"

           "/attribute(add, target-node, reverse-add)"
           "input -> add(a, b)")

    (with-nodes ((add "add") (reverse-add "reverse-add")
                 (input "input") (a "a") (b "b")
                 (add-a-b ("add" "a" "b")))
        modules

      (test-node-function add-a-b add add a b)

      (test-node-function a add-a-b reverse-add add-a-b)
      (test-node-function b add-a-b reverse-add add-a-b)

      (test-simple-binding input add-a-b))))

(test target-functor-cross-module-target-node
  "Test building binding target functor node with target node in different module."

  (with-module-table modules
    (build "/module(m1)"

           "/external(add, x, y)"
           "/external(reverse-add, sum)"
           "/attribute(add, target-node, reverse-add)"

           "/module(m2)"
           "/import(builtin)"
           "/import(m1, add)"

           "input -> add(a, b)")

    (with-nodes ((add "add") (reverse-add "reverse-add")
                 (input "input") (a "a") (b "b")
                 (add-a-b (("/in" "m1" "add") ("/in" "m2" "a") ("/in" "m2" "b"))))
        modules

      (test-node-function add-a-b add add a b)

      (test-node-function a add-a-b reverse-add add-a-b)
      (test-node-function b add-a-b reverse-add add-a-b)

      (test-simple-binding input add-a-b))))

(test target-functor-with-functor-arguments
  "Test building binding target functor node with functor nodes as arguments."

  (test-error target-node-error
              "/external(add, x, y)"
              "/external(reverse-add, sum)"
              "/external(f, x)"

              "/attribute(add, target-node, reverse-add)"
              "input -> add(f(a), b)"))

(test with-target-node-functor-arguments
  "Test building a functor with a `target-node` attribute and a functor argument."

  (with-module-table modules
    (build "/external(add, x, y)"
           "/external(f, x)"
           "/external(reverse-add, sum)"

           "/attribute(add, target-node, reverse-add)"
           "add(f(a), b)")

    (with-nodes ((add "add") (f "f")
                 (a "a") (b "b")
                 (f-a ("f" "a"))
                 (add-a-b ("add" ("f" "a") "b")))
        modules

      (test-node-function add-a-b add add f-a b)

      (test-node-function f-a f f a)

      (is (= 1 (length (contexts f-a))))
      (is (= 0 (length (contexts b)))))))


;;; Explicit Contexts

(test explicit-context-in-target
  "Test explicit contexts in binding target."

  (with-module-table modules
    (build-core-module)
    (build "/import(core);"
           "a < 3 -> ((a + b) -> /context(output, ctx))"
           "a < 4 -> ((a - b) -> /context(output, ctx))"
           "b -> /context(output, ctx)")

    (with-nodes ((b "b") (output "output")

                 (cond1 (:bind (("/in" "core" "+") "a" "b") "output"))
                 (cond2 (:bind (("/in" "core" "-") "a" "b") "output"))

                 (a+b (("/in" "core" "+") "a" "b"))
                 (a-b (("/in" "core" "-") "a" "b")))
        modules

      (let ((ctx (id-symbol "ctx"))
            (*strict-test* nil))

        (has-value-function
            ((cond1 :context ctx) (a+b :context ctx)
             (cond2 :context ctx) (a-b :context ctx)
             (b :context ctx))

            output

          `(:catch
            (:catch
             (if ,cond1 ,a+b (:fail :none))
             (if ,cond2 ,a-b (:fail :none))
             :none)
            ,b
            :none))))))

(test explicit-context-literal-binding-target
  "Test explicit context in literal binding target."

  (with-module-table modules
    (build-core-module)
    (build "/import(core);"
           "a < 3 -> ((a + b) -> /context(output, ctx))"
           "a < 4 -> ((a - b) -> /context(output, ctx))"
           "1 -> /context(output, ctx)")

    (with-nodes ((output "output")

                 (cond1 (:bind (("/in" "core" "+") "a" "b") "output"))
                 (cond2 (:bind (("/in" "core" "-") "a" "b") "output"))

                 (a+b (("/in" "core" "+") "a" "b"))
                 (a-b (("/in" "core" "-") "a" "b")))
        modules

      (let ((ctx (id-symbol "ctx"))
            (*strict-test* nil))

        (has-value-function
            ((cond1 :context ctx) (a+b :context ctx)
             (cond2 :context ctx) (a-b :context ctx))

            output

          `(:catch
            (:catch
             (if ,cond1 ,a+b (:fail :none))
             (if ,cond2 ,a-b (:fail :none))
             :none)
            1
            :none))))))

(test explicit-context-with-test
  "Test explicit context with test function."

  (with-module-table modules
    (build-core-module)
    (build "/import(core);"
           "test-type(x) : x = 1"

           "a < 3 -> ((a + b) -> /context(output, ctx, test-type))"
           "a < 4 -> ((a - b) -> /context(output, ctx, test-type))"
           "b -> /context(output, ctx)")

    (with-nodes ((b "b") (output "output") (test-type "test-type")

                 (cond1 (:bind (("/in" "core" "+") "a" "b") "output"))
                 (cond2 (:bind (("/in" "core" "-") "a" "b") "output"))

                 (a+b (("/in" "core" "+") "a" "b"))
                 (a-b (("/in" "core" "-") "a" "b")))
        modules

      (let ((ctx (id-symbol "ctx"))
            (*strict-test* nil))

        (has-value-function
            ((cond1 :context ctx) (a+b :context ctx)
             (cond2 :context ctx) (a-b :context ctx)
             (b :context ctx))

            output

          `(:catch
            (:catch
             (if ,cond1 ,a+b (:fail :none))
             (if ,cond2 ,a-b (:fail :none))
             ,(meta-node-ref test-type))
            ,b
            :none))))))

(test explicit-context-literal-binding-with-test
  "Test explicit context with test function in literal binding."

  (with-module-table modules
    (build-core-module)
    (build "/import(core);"
           "test-type(x) : x = 1"

           "a < 3 -> ((a + b) -> /context(output, ctx))"
           "1 -> /context(output, ctx, test-type)"
           "a < 4 -> ((a - b) -> /context(output, ctx))")

    (with-nodes ((output "output") (test-type "test-type")

                 (cond1 (:bind (("/in" "core" "+") "a" "b") "output"))
                 (cond2 (:bind (("/in" "core" "-") "a" "b") "output"))

                 (a+b (("/in" "core" "+") "a" "b"))
                 (a-b (("/in" "core" "-") "a" "b")))
        modules

      (let ((ctx (id-symbol "ctx"))
            (*strict-test* nil))

        (has-value-function
            ((cond1 :context ctx) (a+b :context ctx)
             (cond2 :context ctx) (a-b :context ctx))

            output

          `(:catch
            (:catch
             (if ,cond1 ,a+b (:fail :none))
             1
             ,(meta-node-ref test-type))

            (if ,cond2 ,a-b (:fail :none))
            :none))))))

(test explicit-context-in-source
  "Test that explicit context has no effect in binding source."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"
           "/context(a, ctx) -> b"
           "/context(b, z) + cc -> output")

    (with-nodes ((a "a") (b "b") (cc "cc") (output "output")
                 (b+cc (("/in" "core" "+") "b" "cc"))
                 (+ "+"))
        modules

      (test-simple-binding a b :context a)
      (test-simple-binding b+cc output :context b+cc)

      (test-node-function b+cc + + b cc))))

(test explicit-context-in-source-subnode
  "Test that explicit context has no effect in subnode in binding source."

  ;; The /context operator has no effect when a subnode is
  ;; referenced, as the subnode implicitly creates an :OBJECT
  ;; context.

  (with-module-table modules
    (build "/context(a, c1).field -> out")

    (with-nodes ((a "a") (out "out")
                 (a.field (:subnode "a" "field")))
        modules

      (test-simple-binding a.field out)

      (has-value-function (a) a.field `(:member ,a ,(id-symbol "field")))

      (has-value-function
          ((a.field :context :object))
          a

        `(:object (,(id-symbol "field") ,a.field))))))

(test explicit-context-in-target-subnode
  "Test that explicit context has no effect in subnode in binding target."

  ;; The /context operator has no effect when a subnode is
  ;; referenced, as the subnode implicitly creates an :OBJECT
  ;; context.

  (with-module-table modules
    (build "in -> /context(a, c1).field")

    (with-nodes ((in "in") (a "a")
                 (a.field (:subnode "a" "field")))
        modules

      (test-simple-binding in a.field)

      (has-value-function (a) a.field `(:member ,a ,(id-symbol "field")))

      (has-value-function
          ((a.field :context :object))
          a

        `(:object (,(id-symbol "field") ,a.field))))))


;;; Special Operators

(test special-operator-/operator
  "Test registering infix operators."

  (flet ((test-op (position id operators prec assoc)
           (let ((op (get (list position (id-symbol id)) operators)))
             (is-true op "~a operator ~a not registered." position id)

             (is (= (length op) 2)
                 "Expected list of length 2 for ~a operator ~a.~%Got:~%~a"
                 position id op)

             (is (= (first op) prec)
                 "Precedence for operator ~a not ~a. Got: ~a."
                 id prec (first op))

             (is (= (second op) assoc)
                 "Associativity for operator ~a not ~a. Got: ~a."
                 id assoc (second op)))))

    (with-module-table modules
      (build "/operator(+, 50, left); /operator(-, 70); /operator(*, 100, right)")

      (with-slots (operator-nodes) (current-module modules)
        (test-op :infix "+" operator-nodes 50 :left)
        (test-op :infix "-" operator-nodes 70 :right)
        (test-op :infix "*" operator-nodes 100 :right)))))

(test special-operator-errors-/operator
  "Test invalid us of `/operator` operator."

  (test-error invalid-arguments-error "/operator()")
  (test-error invalid-arguments-error "/operator(*)")
  (test-error invalid-arguments-error "/operator(*,*)")
  (test-error invalid-arguments-error "/operator(*,9,x)")
  (test-error invalid-arguments-error "/operator(\"*\", 10, left)")

  (test-top-level-only "/operator(*, 10, left)"))

(test special-operator-/attribute
  "Test setting of node attributes."

  (with-module-table modules
    ;; Build Nodes
    (build "add(a, b) : +(a, b); node1; node2")

    ;; Build Attribute Declarations
    (build "/attribute(node1, coalescable, 0)")
    (build "/attribute(add, \"public-name\", \"sum\")")
    (build "/attribute(node2, input, 1)")

    (with-nodes ((add "add") (node1 "node1") (node2 "node2")) modules
      (is (= 0 (attribute :coalescable node1)))
      (is (= "sum" (attribute :public-name add)))
      (is (= 1 (attribute :input node2)))

      (is (input-node? node2))
      (is (memberp node2 (input-nodes (current-module modules)))))))

(test special-operator-errors-/attribute
  "Test invalid use of `/attribute` operator."

  (test-error invalid-arguments-error "/attribute()")
  (test-error invalid-arguments-error "node; /attribute(node, attribute)")
  (test-error invalid-arguments-error "node; /attribute(node, 1, 2)")
  (test-error invalid-arguments-error "/attribute(1, attribute, value)")

  (test-top-level-only "/attribute(node, input, 1)" "node"))


;;; Meta-Node Definitions

(test meta-node-simple-definition
  "Test basic meta-node definitions."

  (with-module-table modules
    (build-core-module)
    (build "/import(core); add(x,y) : x + y; add(a,b)")

    (with-nodes ((add "add") (a "a") (b "b") (add-ab ("add" "a" "b"))) modules
      (is (typep add 'meta-node))
      (is (= (decls '!\x '!\y) (operands add)))
      (is (decl= (decls '(!+ !\x !\y)) (definition add)))

      (test-node-function add-ab add add a b))))

(test meta-node-optional-and-rest-arguments
  "Test meta-node definition with optional and rest arguments."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"
           "f(a, b : 1, c : x, :(d), ..(rest)) : a + b + c + d + rest")

    (with-nodes ((f "f") (x "x")) modules
      (is (typep f 'meta-node))
      (is (= (decls '!\a
                    '(!\: !\b 1)
                    (list '!\: '!\c x)
                    '(!\: !\d :none)
                    '(!.. !|rest|))

             (operands f))))))

(test meta-node-definition-error-syntax
  "Test syntax errors in meta-node definition."

  (test-error invalid-arguments-error "x : y")
  (test-error semantic-error "{x; y} : z")
  (test-error invalid-arguments-error "{w; x}(y) : z")
  (test-error invalid-arguments-error ":(x)")
  (test-error invalid-arguments-error ":()")
  (test-error invalid-arguments-error ":(x,y,z)")

  (test-top-level-only "(g(x,y) : f(x,y))"))

(test meta-node-definition-error-invalid-operand-list
  "Test error raised when meta-node operand list, in definition, is invalid."

  (test-error invalid-operand-list-error "f(x, g(h)) : x")
  (test-error invalid-operand-list-error "f(1, x, 3) : x")

  (test-error invalid-operand-list-error "f(x, y : 3, z) : x")
  (test-error invalid-operand-list-error "f(x, ..(y), ..(z)) : x")
  (test-error invalid-operand-list-error "f(x, ..(y), z) : x")
  (test-error invalid-operand-list-error "f(x, ..(y), z : 3) : x")

  (test-error invalid-operand-list-error "f(x, :(g(y), 3)) : x")
  (test-error invalid-operand-list-error "f(x, ..(g(y))) : x"))

(test meta-node-definition-error-redefine-special-operator
  "Test error raised on attempts to redefine a special operator."

  (test-error redefine-special-operator-error "/context(x) : x")
  (test-error redefine-special-operator-error "/external(x) : x")
  (test-error redefine-special-operator-error "/operator(a, b) : f(b, a)")
  (test-error redefine-special-operator-error "/attribute(m, n) : f(m,n)")
  (test-error redefine-special-operator-error "/module(m) : m")
  (test-error redefine-special-operator-error "/import(x) : x")
  (test-error redefine-special-operator-error "/use(z) : z")
  (test-error redefine-special-operator-error "/export(y) : h(y)")
  (test-error redefine-special-operator-error "/in(x, y) : add(x, y)"))

(test meta-node-definition-error-node-exists
  "Test error raised on attempt to redefine node as meta-node."

  (with-module-table modules
    (build "a;b")
    (signals node-exists-error (build "a(x,y) : add(x,y)")))

  (with-module-table modules
    (build "f(x) : x")
    (signals node-exists-error (build "f(x,y) : +(x, y)"))))


;;; External Meta-Node Definitions

(test external-meta-node-simple-definition
  "Test basic definition of external meta-node."

  (with-module-table modules
    (build "/external(add, x, y); /external(sub, x, y); add(a,b); sub(a,b)")

    (with-nodes ((add "add") (sub "sub")
                 (a "a") (b "b")
                 (add-ab ("add" "a" "b")) (sub-ab ("sub" "a" "b")))
        modules

      (is (typep add 'external-meta-node))
      (is (typep sub 'external-meta-node))

      (is (= (decls '!\x '!\y) (operands add)))
      (is (= (decls '!\x '!\y) (operands sub)))

      (test-node-function add-ab add add a b)
      (test-node-function sub-ab sub sub a b))))

(test external-meta-node-optional-and-rest-arguments
  "Test external meta-node definition with optional and rest arguments."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"
           "/external(f, a, b : 1, c : x, :(d), ..(rest))")

    (with-nodes ((f "f") (x "x")) modules
      (is (typep f 'external-meta-node))
      (is (= (decls '!\a
                    '(!\: !\b 1)
                    (list '!\: '!\c x)
                    '(!\: !\d :none)
                    '(!.. !|rest|))

             (operands f))))))

(test external-meta-node-definition-error-syntax
  "Test syntax error in external meta-node definition."

  (test-top-level-only "/external(y)")

  (test-error invalid-arguments-error "/external(g(h))")
  (test-error semantic-error "/external({x; y})")
  (test-error invalid-arguments-error "/external({w; x}(y) : z)")
  (test-error invalid-arguments-error "/external()"))

(test external-meta-node-definition-error-invalid-operand-list
  "Test error raised when meta-node operand list, in definition, is invalid."

  (test-error invalid-operand-list-error "/external(f, x, g(h))")
  (test-error invalid-operand-list-error "/external(f, 1, x, 3) : x")

  (test-error invalid-operand-list-error "/external(f, x, y : 3, z)")
  (test-error invalid-operand-list-error "/external(f, x, ..(y), ..(z))")
  (test-error invalid-operand-list-error "/external(f, x, ..(y), z)")
  (test-error invalid-operand-list-error "/external(f, x, ..(y), z : 3)")

  (test-error invalid-operand-list-error "/external(f, x, :(g(y), 3))")
  (test-error invalid-operand-list-error "/external(f, x, ..(g(y)))"))

(test external-meta-node-definition-error-redefine-special-operator
  "Test error raised on attempts to redefine a special operator with external meta-node."

  (test-error redefine-special-operator-error "/external(/context)")
  (test-error redefine-special-operator-error "/external(/external)")
  (test-error redefine-special-operator-error "/external(/operator)")
  (test-error redefine-special-operator-error "/external(/attribute)")
  (test-error redefine-special-operator-error "/external(/module)")
  (test-error redefine-special-operator-error "/external(/import)")
  (test-error redefine-special-operator-error "/external(/use)")
  (test-error redefine-special-operator-error "/external(/export)")
  (test-error redefine-special-operator-error "/external(/in)"))

(test external-meta-node-error-node-exists
  "Test error raised on attempt to redefine node as external meta-node."

  (with-module-table modules
    (build "a;b")
    (signals node-exists-error (build "/external(a)")))

  (with-module-table modules
    (build "f(x) : x")
    (signals node-exists-error (build "/external(f)"))))


;;; Meta-Node Arity Checking

(test meta-node-arity-check-required-arguments
  "Test arity checks in meta-node required arguments."

  (test-error arity-error
              "/import(core)"
              "add(x, y) : x + y"
              "add(a) -> b")

  (test-error arity-error
              "/import(core)"
              "1+(x) : x + 1"
              "1+(a,b) -> cc"))

(test meta-node-arity-check-optional-arguments
  "Test arity checks in meta-node optional arguments."

  (test-error arity-error
              "/import(core)"
              "add(x, y, z : 1) : x + y + z"
              "add(a) -> b")

  (test-error arity-error
              "/import(core)"
              "1+(x, y : 1) : x + y"
              "1+(a,b,cc) -> d"))

(test meta-node-arity-check-optional-arguments-required-only
  "Test arity checks in meta-node with optional arguments, but only required given."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"
           "1+(x, delta : 1) : x + delta"

           "1+(a) -> b")

    (with-nodes ((1+ "1+")
                 (a "a") (b "b")
                 (1+a ("1+" "a")))
        modules

      (has-value-function (a) 1+a `(,1+ ,a 1))
      (test-simple-binding 1+a b))))

(test meta-node-arity-check-optional-arguments-required-and-optional
  "Test arity checks in meta-node with both required and optional arguments given."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"
           "1+(x, delta : 1) : x + delta"

           "1+(a, 3) -> b")

    (with-nodes ((1+ "1+")
                 (a "a") (b "b")
                 (1+a ("1+" "a" 3)))
        modules

      (has-value-function (a) 1+a `(,1+ ,a 3))
      (test-simple-binding 1+a b))))

(test meta-node-arity-check-optional-arguments-node-default-value
  "Test arity checks in meta-node with node default values."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"
           "1+(x, d : delta) : x + d"

           "1+(a) -> b")

    (with-nodes ((1+ "1+")
                 (a "a") (b "b") (delta "delta")
                 (1+a ("1+" "a")))
        modules

      (has-value-function (a delta) 1+a `(,1+ ,a ,delta))
      (test-simple-binding 1+a b))))

(test meta-node-arity-check-optional-arguments-functor-node-default-value
  "Test arity checks in meta-node with functor node default values."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"
           "1+(x, d : delta1 + delta2) : x + d"

           "1+(a) -> b")

    (with-nodes ((1+ "1+") (+ "+")
                 (a "a") (b "b")
                 (delta1 "delta1") (delta2 "delta2")
                 (d1+d2 (("/in" "core" "+") "delta1" "delta2"))
                 (1+a ("1+" "a")))
        modules

      (test-node-function d1+d2 + + delta1 delta2)
      (has-value-function (a d1+d2) 1+a `(,1+ ,a ,d1+d2))

      (test-simple-binding 1+a b))))

(test meta-node-arity-check-rest-arguments
  "Test arity checks in meta-node with rest arguments."

  (test-error arity-error
              "/import(core); add(x, y, ..(xs)) : x + y + xs; add(a) -> b"))

(test meta-node-arity-check-rest-arguments-minimum
  "Test arity checks when minimum number of arguments given to meta-node."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"
           "add(x, ..(xs)) : x + xs"
           "add(a) -> b")

    (with-nodes ((add "add")
                 (a "a") (b "b")
                 (add-a ("add" "a")))
        modules

      (has-value-function (a) add-a `(,add ,a ,(argument-list nil)))
      (test-simple-binding add-a b))))

(test meta-node-arity-check-rest-arguments-more-than-operands
  "Test arity checks on meta-node with rest arguments, and more arguments than operands."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"
           "add(x, ..(xs)) : x + xs"
           "add(a,b,cc) -> x")

    (with-nodes ((add "add")
                 (a "a") (b "b") (cc "cc") (x "x")
                 (add-abc ("add" "a" "b" "cc")))
        modules

      (has-value-function
          (a b cc)
          add-abc

        `(,add ,a ,(argument-list (list b cc))))
      (test-simple-binding add-abc x))))

(test meta-node-arity-check-rest-arguments-zero
  "Test arity checks on meta-node with rest arguments and zero arguments given."

  (with-module-table modules
    (build "f(..(xs)) : xs"
           "f() -> a")

    (with-nodes ((f "f") (ff ("f")) (a "a"))
        modules

      (test-value-function ff f (list f (argument-list nil)))
      (test-simple-binding ff a))))


;;; External Meta-Node Arity Checking

(test external-meta-node-arity-check-required-arguments
  "Test arity checks in external meta-node required arguments."

  (test-error arity-error
              "/external(add, x, y)"
              "add(a) -> b")

  (test-error arity-error
              "/external(1+, x)"
              "1+(a,b) -> cc"))

(test external-meta-node-arity-check-optional-arguments
  "Test arity checks in external meta-node optional arguments."

  (test-error arity-error
              "/external(add, x, y, z : 1)"
              "add(a) -> b")

  (test-error arity-error
              "/external(1+, x, y : 1)"
              "1+(a,b,cc) -> d"))

(test external-meta-node-arity-check-optional-arguments-required-only
  "Test arity checks in external meta-node with optional arguments, but only required given."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"
           "/external(1+, x, delta : 1)"

           "1+(a) -> b")

    (with-nodes ((1+ "1+")
                 (a "a") (b "b")
                 (1+a ("1+" "a")))
        modules

      (has-value-function (a) 1+a `(,1+ ,a 1))
      (test-simple-binding 1+a b))))

(test external-meta-node-arity-check-optional-arguments-required-and-optional
  "Test arity checks in external meta-node with both required and optional arguments given."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"
           "/external(1+, x, delta : 1)"

           "1+(a, 3) -> b")

    (with-nodes ((1+ "1+")
                 (a "a") (b "b")
                 (1+a ("1+" "a" 3)))
        modules

      (has-value-function (a) 1+a `(,1+ ,a 3))
      (test-simple-binding 1+a b))))

(test external-meta-node-arity-check-optional-arguments-node-default-value
  "Test arity checks in external meta-node with node default values."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"
           "/external(1+, x, d : delta)"

           "1+(a) -> b")

    (with-nodes ((1+ "1+")
                 (a "a") (b "b") (delta "delta")
                 (1+a ("1+" "a")))
        modules

      (has-value-function (a delta) 1+a `(,1+ ,a ,delta))
      (test-simple-binding 1+a b))))

(test external-meta-node-arity-check-optional-arguments-functor-node-default-value
  "Test arity checks in external meta-node with functor node default values."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"
           "/external(1+, x, d : delta1 + delta2)"

           "1+(a) -> b")

    (with-nodes ((1+ "1+") (+ "+")
                 (a "a") (b "b")
                 (delta1 "delta1") (delta2 "delta2")
                 (d1+d2 (("/in" "core" "+") "delta1" "delta2"))
                 (1+a ("1+" "a")))
        modules

      (test-node-function d1+d2 + + delta1 delta2)
      (has-value-function (a d1+d2) 1+a `(,1+ ,a ,d1+d2))

      (test-simple-binding 1+a b))))

(test external-meta-node-arity-check-rest-arguments
  "Test arity checks in external meta-node with rest arguments."

  (test-error arity-error
              "/import(core)"
              "/external(add, x, y, ..(xs))"
              "add(a) -> b"))

(test external-meta-node-arity-check-rest-arguments-minimum
  "Test arity checks when minimum number of arguments given to external meta-node."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"
           "/external(add, x, ..(xs))"
           "add(a) -> b")

    (with-nodes ((add "add")
                 (a "a") (b "b")
                 (add-a ("add" "a")))
        modules

      (has-value-function (a) add-a `(,add ,a ,(argument-list nil)))
      (test-simple-binding add-a b))))

(test external-meta-node-arity-check-rest-arguments-more-than-operands
  "Test arity checks on external meta-node with rest arguments, and more arguments than operands."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"
           "/external(add, x, ..(xs))"
           "add(a,b,cc) -> x")

    (with-nodes ((add "add")
                 (a "a") (b "b") (cc "cc") (x "x")
                 (add-abc ("add" "a" "b" "cc")))
        modules

      (has-value-function
          (a b cc)
          add-abc

        `(,add ,a ,(argument-list (list b cc))))
      (test-simple-binding add-abc x))))

(test external-meta-node-arity-check-rest-arguments-zero
  "Test arity checks on external meta-node with rest arguments and zero arguments given."

  (with-module-table modules
    (build "/external(f, ..(xs))"
           "f() -> a")

    (with-nodes ((f "f") (ff ("f")) (a "a"))
        modules

      (test-value-function ff f (list f (argument-list nil)))
      (test-simple-binding ff a))))


;;; Meta-Node References

(test meta-node-reference-in-operand
  "Test referencing meta-node as value in operand."

  (with-module-table modules
    (build "/external(map, f, list)"
           "/external(add, x, y)"
           "map(add, input) -> output")

    (with-nodes ((map "map") (add "add")
                 (input "input") (output "output")
                 (map-input ("map" "add" "input")))
        modules

      (has-value-function
          (input)
          map-input

        `(,map ,(meta-node-ref add) ,input))

      (test-simple-binding map-input output))))

(test meta-node-reference-default-arguments
  "Test referencing meta-node with default argument."

  (with-module-table modules
    (build "/external(map, f, list)"
           "/external(inc, x, d : delta)"

           "map(inc, input) -> output")

    (with-nodes ((map "map") (inc "inc")
                 (input "input") (output "output") (delta "delta")
                 (map-input ("map" "inc" "input")))
        modules

      (has-value-function
          (input delta)
          map-input

        `(,map ,(meta-node-ref inc :optional (list delta)) ,input))

      (test-simple-binding map-input output))))

(test meta-node-reference-binding-source
  "Test meta-node reference in binding source."

  (with-module-table modules
    (build "/external(map, f, list)"
           "/external(add, x, y)"
           "add -> fn"
           "map(fn, input) -> output")

    (with-nodes ((map "map") (add "add") (fn "fn")
                 (input "input") (output "output")
                 (map-input ("map" "fn" "input")))
        modules

      (test-value-function fn (init-context fn) (meta-node-ref add))

      (has-value-function
          (input fn)
          map-input

        `(,map ,fn ,input))

      (test-simple-binding map-input output))))

(test meta-node-reference-binding-target
  "Test meta-node reference in binding target."

  (test-error target-node-error "/external(add); x -> add"))


;;; Subnodes

(defmacro with-subnode-test (&body body)
  `(labels ((test-object-fn (node &rest fields)
              (-<>
               (list* :object (map (curry #'make-field node) fields))
               (test-value-function node :object <>)))

            (make-field (node field)
              (destructuring-bind (field dep) field
                (list (id-symbol field) (test-binding dep node :context :object))))

            (test-member-fn (node field dep)
              (->>
               `(:member ,(test-binding dep node :context dep) ,(id-symbol field))
               (test-value-function node dep))))

     ,@body))

(test subnode-of-node
  "Test binding to subnode of node."

  (with-subnode-test
    (with-module-table modules
      (build "x -> a.first; y -> a.last; a.last -> y")

      (with-nodes ((x "x") (y "y") (a "a")
                   (a.first (:subnode "a" "first")) (a.last (:subnode "a" "last")))
          modules

        (test-object-fn a (list "first" a.first) (list "last" a.last))
        (test-member-fn a.first "first" a)
        (test-member-fn a.last "last" a)

        (test-simple-binding x a.first :context x)
        (test-simple-binding y a.last :context y)
        (test-simple-binding a.last y :context a.last)))))

(test subnode-of-meta-node
  "Test binding to subnode of meta-node `self` node."

  (with-subnode-test
    (with-module-table modules
      (build "Person(first, last) : { first -> self.first; last -> self.last }"
             "Person(first, last).first -> name")

      (with-nodes ((person-fn "Person")
                   (first "first") (last "last")
                   (person ("Person" "first" "last"))
                   (person.first (:subnode ("Person" "first" "last") "first"))
                   (name "name"))
          modules

        (test-node-function person person-fn person-fn first last)
        (test-member-fn person.first "first" person)
        (test-simple-binding person.first name :context person.first)))))

(test subnode-cross-module
  "Test binding to subnode of node in another module."

  (with-subnode-test
    (with-module-table modules
      (build "/module(mod1)"
             "/import(builtin)"
             "x -> a.first"

             "/module(mod2)"
             "/import(builtin)"
             "/use(mod1)"
             "y -> mod1.a.second")

      (with-modules ((mod1 "mod1") (mod2 "mod2")) modules
        (with-nodes ((a "a")
                     (x "x")
                     (a.first (:subnode "a" "first"))
                     (a.second (:subnode "a" "second")))
            mod1

          (with-nodes ((y "y")) mod2
            (test-object-fn a (list "first" a.first) (list "second" a.second))
            (test-member-fn a.first "first" a)
            (test-member-fn a.second "second" a)

            (test-simple-binding x a.first :context x)
            (test-simple-binding y a.second :context y)))))))


;;; Node States

(test node-state-any-state
  "Test node state bindings from any state."

  (with-module-table modules
    (build "/external(not, x)"
           "not(on) -> /state(on, toggle)"
           "0 -> on"

           "the-state -> /state(on)")

    (with-nodes ((on "on")
                 (state-on ("/state" "on"))
                 (toggle-state-on ("/state" "on" "toggle"))
                 (not-on ("not" "on"))
                 (the-state "the-state")
                 (if "if"))
        modules

      (let ((equal (get :symbol-equal tridash.frontend::*core-meta-nodes*))
            (previous (get :previous-value tridash.frontend::*core-meta-nodes*)))

        (has-value-function (state-on (toggle toggle-state-on))
            on

          `(,if (,equal ,state-on ,(id-symbol "toggle"))
                (,previous ,(node-ref toggle-state-on))
                (,previous ,(node-ref on))))

        (test-value-function on :init 0)

        (test-binding not-on toggle-state-on)
        (test-binding the-state state-on)))))

(test node-state-specific-state
  "Test node state bindings with specific state transition."

  (with-module-table modules
    (build "/external(not, x)"
           "not(on) -> /state(on, default, toggle)"
           "False -> on"

           "the-state -> /state(on)")

    (with-nodes ((on "on")
                 (state-on ("/state" "on"))
                 (toggle-state-on ("/state" "on" "default" "toggle"))
                 (not-on ("not" "on"))
                 (the-state "the-state")
                 (if "if")

                 (false "False"))
        modules

      (let ((equal (get :symbol-equal tridash.frontend::*core-meta-nodes*))
            (previous (get :previous-value tridash.frontend::*core-meta-nodes*)))

        (has-value-function ((state state-on) (toggle toggle-state-on))
            on

          `(,if
            (,if (,equal (,previous ,(node-ref state-on)) ,(id-symbol "default"))
                 (,equal ,state ,(id-symbol "toggle"))
                 ,(node-ref *node-false*))

            (,previous ,(node-ref toggle-state-on))
            (,previous ,(node-ref on))))

        (test-simple-binding false on)

        (test-binding not-on toggle-state-on)
        (test-binding the-state state-on)))))


;;; Modules

(defmacro with-modules-test (&body body)
  `(with-module-table modules
      (build "x;y"
             "/module(my-mod); a; b"
             "/module(my-mod); /import(builtin); +(x,y) : add(x,y); /operator(+, 50, left)"
             "/module(my-mod); /export(a,+)")

      ,@body))

(defmacro with-module-nodes (&body body)
  `(with-modules ((init :init) (my-mod "my-mod"))
       modules

     (with-nodes ((my-mod.a "a") (my-mod.b "b") (+ "+")) my-mod
       (declare (ignorable my-mod.a my-mod.b +))
       ,@body)))

(test modules-test
  "Test the creation of modules and the separation of nodes."

  (with-modules-test
    (with-module-nodes
      (test-nodes init "x" "y"))))

(test modules-operator-/use
  "Test `/use` operator."

  (with-modules-test
    (with-module-nodes
      (build "/module(mod2)"
             "/import(builtin)"
             "/use(my-mod)"

             "my-mod.a -> a"
             "my-mod.+(n,m)")

      (with-modules ((mod2 "mod2")) modules
        (with-nodes ((a "a") (n "n") (m "m"))
            mod2

          (with-nodes ((n+m (("/in" "my-mod" "+") ("/in" "mod2" "n") ("/in" "mod2" "m"))))
              init
            (test-simple-binding my-mod.a a :context my-mod.a)
            (test-node-function n+m + + n m)))))))

(test modules-operator-/use-as
  "Test `/use-as` operator."

  (with-modules-test
    (with-module-nodes
      (build "/module(mod3)"
             "/import(builtin)"
             "/use-as(my-mod, m)"

             "m.a -> a"
             "m.+(j,k)")

      (with-modules ((mod3 "mod3")) modules
        (with-nodes ((a "a") (j "j") (k "k"))
            mod3

          (with-nodes ((j+k (("/in" "my-mod" "+") ("/in" "mod3" "j") ("/in" "mod3" "k"))))
              init

            (test-simple-binding my-mod.a a :context my-mod.a)
            (test-node-function j+k + + j k)))))))

(test modules-operator-/import-long-form
  "Test long form of `/import` operator."

  (with-modules-test
    (with-module-nodes
      (build "/module(mod4); /import(my-mod, +); a + b")

      (with-modules ((mod4 "mod4")) modules
        (with-nodes ((a "a") (b "b"))
            mod4

          (with-nodes ((a+b (("/in" "my-mod" "+") ("/in" "mod4" "a") ("/in" "mod4" "b"))))
              init

            (test-node-function a+b + + a b)

            (is (not (eq a my-mod.a)))
            (is (not (eq b my-mod.b)))))))))

(test modules-operator-/import-short-form
  "Test short form of `/import` operator."

  (with-modules-test
    (with-module-nodes
      (build "/module(mod5); /import(my-mod); a + b")

      (with-modules ((mod5 "mod5")) modules
        (with-nodes ((a "a") (b "b"))
            mod5

          (with-nodes ((a+b (("/in" "my-mod" "+") ("/in" "my-mod" "a") ("/in" "mod5" "b"))))
              init

            (test-node-function a+b + + a b)

            (is (eq a my-mod.a))
            (is (not (eq b my-mod.b)))))))))

(test modules-operator-/in
  "Test `/in` operator."

  (with-modules-test
    (with-module-nodes
      (build "/module(mod6); /in(my-mod,+)(/in(my-mod, a), b)")

      (with-modules ((mod6 "mod6")) modules
        (with-nodes ((b "b"))
            mod6

          (with-nodes ((a+b (("/in" "my-mod" "+") ("/in" "my-mod" "a") ("/in" "mod6" "b"))))
              init

            (test-node-function a+b + + my-mod.a b)

            (is (not (eq b my-mod.b)))))))))

(test cross-module-conditional-bindings
  "Test cross-module conditional bindings."

  (with-modules-test
    (with-module-nodes
      (build "/module(mod7)"
             "/import(builtin)"
             "/import(my-mod, a, b)"

             "a -> (b -> cc)")

      (with-modules ((mod7 "mod7")) modules
        (with-nodes ((a "a") (b "b") (cc "cc"))
            mod7

          (with-nodes ((b->cc (:bind ("/in" "my-mod" "b") ("/in" "mod7" "cc"))))
              init

            (test-simple-binding a b->cc :context a)

            (let ((*strict-test* nil))
	      (has-value-function
	          ((b :context b) (b->cc :context b))
	          cc

	        `(if ,b->cc ,b (:fail :none))))))))))

(test module-error-semantics
  "Test semantic error conditions when using modules."

  (with-module-table modules
    (build "x;y"
           "/module(my-mod); a; b"
           "/module(my-mod); /import(builtin); +(x,y) : add(x,y); /operator(+, 50, left)"
           "/module(my-mod); /export(a,+)")

    (build "/module(mod2)"
           "/import(builtin)"
           "/use(my-mod)"

           "my-mod.a -> a"
           "my-mod.+(n,m)")

    (build "/module(mod3)"
           "/import(builtin)"
           "/use-as(my-mod, m)"

           "m.a -> a"
           "m.+(j,k)")

    (build "/module(mod4); /import(my-mod, +); a + b")
    (build "/module(mod5); /import(my-mod); a + b")
    (build "/module(mod6); /in(my-mod,+)(/in(my-mod, a), b)")

    (build "/module(mod7)"
           "/import(builtin)"
           "/import(my-mod, a, b)"

           "a -> (b -> cc)")

    (signals non-existent-node-error (build "/module(mod2); +(j,k)"))
    (signals declaration-parse-error (build "/module(mod2); j + k"))
    (signals non-existent-node-error (build "/module(mod2); my-mod.z"))

    (signals non-existent-node-error (build "/module(mod3); +(j,k)"))
    (signals declaration-parse-error (build "/module(mod3); j + k"))
    (signals non-existent-node-error (build "/module(mod3); m.z"))

    (signals non-existent-node-error
      (build "/module(mod4); /import(builtin); my-mod.+(a,b)"))
    (signals non-existent-node-error (build "/module(mod4); /in(my-mod, z)"))))

(test non-existent-module-errors
  "Test that using non-existent modules signals an error."

  (test-error non-existent-module-error "/use(no-such-module)")
  (test-error non-existent-module-error "/use-as(no-such-module, m)")
  (test-error non-existent-module-error "/import(no-such-module)")
  (test-error non-existent-module-error "/import(no-such-module, node)")
  (test-error non-existent-module-error "/in(no-such-module, x)")
  (test-error non-existent-node-error "/export(no-such-node)")
  (test-error non-existent-node-error "x; /export(x, no-such-node)"))

(test module-error-syntax-/module
  "Test syntax errors with `/module` operator."

  (test-error invalid-arguments-error "/module()")
  (test-error invalid-arguments-error "/module(a, b, c)")
  (test-error invalid-arguments-error "/module(1, 2, 3)")
  (test-error invalid-arguments-error "/module(1)")

  (test-top-level-only "/module(m)"))

(test module-error-syntax-/use
  "Test syntax errors with `/use` operator."

  (test-error invalid-arguments-error "/use(1,2,3)")
  (test-top-level-only "/use(m1)" "/module(m1)" "/module(m2); /import(builtin)"))

(test module-error-syntax-/use-as
  "Test syntax errors with `/user-as` operator."

  (test-error invalid-arguments-error "/use-as()")
  (test-error invalid-arguments-error "/use-as(m)")
  (test-error invalid-arguments-error "/use-as(1)")
  (test-error invalid-arguments-error "/use-as(1,2)")
  (test-error invalid-arguments-error
              "/module(m1); /module(m2); /use-as(m1, m, x, y)")

  (test-top-level-only "/use-as(mod, m)" "/module(mod)" "/module(m1); /import(builtin)"))

(test module-error-syntax-/import
  "Test syntax errors with `/import` operator."

  (test-error invalid-arguments-error "/import()")
  (test-error invalid-arguments-error "/import(1)")

  (test-top-level-only "/import(mod)" "/module(mod)" "/module(m1); /import(builtin)")
  (test-top-level-only "/import(mod, x)" "/module(mod); x" "/module(m1); /import(builtin)"))

(test module-error-syntax-/export
  "Test syntax errors with `/export` operator."

  (test-error semantic-error "/export(1, 2, 3)")
  (test-top-level-only "/export(x)" "x"))

(test module-error-syntax-/in
  "Test syntax errors with `/in` operator."

  (test-error invalid-arguments-error "/in()")
  (test-error invalid-arguments-error "/in(x)")
  (test-error invalid-arguments-error
              "/module(m1); x; /module(m2); /in(m1, x, y)")
  (test-error invalid-arguments-error "/in(1, x)"))

(test module-error-pseudo-node-reference
  "Test that referencing module pseudo nodes results in an error."

  (test-error module-node-reference-error
              "/module(m1)"
              "/module(m2)"
              "/use(m1)"
              "/external(add, x, y)"
              "add(m1, x)")

  (test-error module-node-reference-error
              "/module(m1)"
              "/module(m2)"
              "/import(builtin)"
              "/use(m1)"
              "m1 -> x")
  (test-error target-node-error
              "/module(m1)"
              "/module(m2)"
              "/import(builtin)"
              "/use(m1)"
              "x -> m1"))
