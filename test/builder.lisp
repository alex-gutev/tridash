;;;; builder.lisp
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

;;;; Unit tests for the graph builder

(in-package :tridash.test)

(cl-interpol:enable-interpol-syntax)

(plan nil)

(defun node-id (name)
  "Converts a string/list node identifier to an identifier symbol."

  (typecase name
    (cons (mapcar #'node-id name))
    (symbol name)
    (otherwise (id-symbol name))))

(defun test/get-node (name node-table)
  "Retrieves the node with name NAME from NODE-TABLE and checks that
   its NAME slot matches NAME."

  (diag (format nil "Check node ~a" name))

  (let ((id (node-id name)))
    (aprog1 (gethash id (all-nodes node-table))
      (is-type it 'node "Is Node")
      (is (name it) id "Node Name"))))

(defun ensure-node-table (thing)
  "If THING is a `MODULE-TABLE' return the node table bound to the
   NODE-TABLE slot. Otherwise if THING is a `NODE-TABLE' return it."

  (etypecase thing
    (module-table (node-table thing))
    (node-table thing)))


(defun test-dependency (dep node)
  "Tests that DEP is a dependency of NODE and returns the
   `NODE-LINK'."

  (aprog1 (gethash dep (dependencies node))
    (ok it (format nil "~a is a dependency of ~a" dep node))))


(defmacro! with-nodes ((&rest nodes) o!modules &body body)
  "Binds the nodes to variables. Each element of NODES is of the
   form (VAR NAME) where VAR is the variable to which the node is
   bound and NAME designates the node's name."

  (flet ((make-binding (node)
           (destructuring-bind (var name) node
             `(,var (test/get-node ',(node-id name) ,g!node-table)))))
    `(let ((,g!node-table (ensure-node-table ,g!modules)))
       (let ,(mapcar #'make-binding nodes)
         ,@body))))

(defmacro! with-modules ((&rest modules) o!module-table &body body)
  "Binds module `NODE-TABLE's to variables. Each element of MODULES is
   of the form (VAR NAME) where VAR is the variable to which the
   module is bound and NAME designates the module's name."

  (flet ((make-binding (module)
           (destructuring-bind (var name) module
             `(,var (aprog1 (gethash ',(node-id name) (modules ,g!module-table))
                      (is-type it 'node-table ,(format nil "~a is a module" name)))))))
    `(let ,(mapcar #'make-binding modules)
       ,@body)))

(defmacro! with-dependencies ((&rest deps) o!node &body body)
  "Tests that NODE has the dependencies in DEPS and binds the
   corresponding `NODE-LINK' objects to variables. Each element of
   DEPS is either a symbol naming a variable which evaluates to the
   dependency NODE or is of the form (VAR NODE) where NODE is a form
   that evaluates to the dependency node and VAR is the variable to
   which the `NODE-LINK' object is bound."

  (flet ((make-binding (dep)
           (ematch dep
             ((list var dep)
              (list var `(test-dependency ,dep ,g!node)))

             ((type symbol)
              (list dep `(test-dependency ,dep ,g!node))))))
    `(let ,(mapcar #'make-binding deps)
       ,@body)))


(labels ((build-nodes (string modules)
	   (with-input-from-string (in string)
	     (build-parsed-nodes (make-parser in) modules)))

	 (test-nodes (modules &rest nodes)
	   (mapc (rcurry #'test/get-node (ensure-node-table modules)) nodes))

	 (test-binding (src target &optional (context src))
	   "Tests that a binding between node SRC and TARGET has been
            established in the context CONTEXT."

	   (diag (format nil "Test binding ~a -> ~a" (name src) (name target)))

	   (let ((link (gethash target (observers src))))
	     (is-type link 'node-link "Node link created")
	     (is link (gethash src (dependencies target))
		 (format nil "Node link added to dependencies of ~a" (name target)))

	     (is (node-link-node link) src "Node link points to correct node")
	     (is (node-link-context link) context "Link context is correct")

	     (let ((id context)
		   (context (gethash context (contexts target))))
	       (is-type context 'node-context (format nil "Node context ~a created" id))
	       (is (gethash src (operands context)) link
		   (format nil "~a added to context operands" (name src))))

	     link))

	 (test-simple-binding (src target &optional (context src))
	   "Tests that a binding between node SRC and TARGET has been
            established and that the value function of TARGET (in
            context CONTEXT) is the `NODE-LINK' itself."

	   (->>
	    (test-binding src target context)
	    (test-value-function target context)))

	 (value-fn-equal (a b)
	   "Value function equality comparison. Returns true if the
            value function A is equivalent to the value function B."

	   (multiple-value-match (values a b)
	     (((cons a as) (cons b bs))
	      (and (value-fn-equal a b)
		   (value-fn-equal as bs)))

	     (((node-link- (node node-a) (context context-a))
	       (node-link- (node node-b) (context context-b)))

	      (and (eq node-a node-b)
		   (eq context-a context-b)))

	     ((_ _) (equal a b))))

	 (test-value-function (node context fn &key (test #'value-fn-equal))
	   "Tests that the context CONTEXT of node NODE has value
            function FN."

	   (diag (format nil "Test value function of ~a in context ~a" (name node) context))

	   (let ((context (gethash context (contexts node))))
	     (is (value-function context) fn :test test)))

	 (test-node-function (node context fn &rest operands)
	   "Tests the node NODE has a context CONTEXT with the value
            function being FN applied to operands OPERANDS."

	   (->>
	    (list* fn (mapcar (rcurry #'test-binding node context) operands))
	    (test-value-function node context)))

	 (test-error (str &optional (error 'semantic-error))
	   "Tests that building the source STR results in a
            `SEMANTIC-ERROR'."

	   (is-error (build-nodes str (make-instance 'module-table)) error
		     (format nil "`~a` raises an error of type `~s'" str error)))

	 (test-top-level-only (decl &rest code)
	   "Tests that an error is raised if the declaration DECL
            appears in a non-top-level position. Each element in CODE
            is prepended (separated by ';') to DECL before performing
            the tests."

	   (test-error (format nil "~{~a; ~}~a -> a" code decl))
	   (test-error (format nil "~{~a; ~}a -> ~a" code decl))
	   (test-error (format nil "~{~a; ~}f(x) : x; f(~a)" code decl)))

	 (test-not-nodes (modules &rest ids)
	   "Tests that there are no nodes with identifiers IDS in
            MODULES."

	   (mapc (curry #'test-not-node modules) ids))

	 (test-not-node (modules id)
	   "Tests that there is no node with identifier ID in
            MODULES."

	   (ok (null (gethash (node-id id) (all-nodes (ensure-node-table modules))))
	       (format nil "~a is not a node" id))))

  (subtest "Test Node Builder"
    (subtest "Simple Atom Nodes"
      (let ((modules (make-instance 'module-table)))
        (build-nodes "a;b;c" modules)

        (test-nodes modules "a" "b" "c")))

    (subtest "Bindings"
      (let ((modules (make-instance 'module-table)))
        (build-nodes "a -> b; c -> b" modules)

        (with-nodes ((a "a") (b "b") (c "c")) modules
          (test-simple-binding a b)
          (test-simple-binding c b)))

      (let ((modules (make-instance 'module-table)))
        (build-nodes "a -> (b -> c)" modules)

        (with-nodes ((a "a") (b "b") (c "c") (b->c ("->" "b" "c"))) modules
          (test-simple-binding a b->c a)

          (->>
           `(if ,(test-binding b->c c b)
                ,(test-binding b c)
                ,(node-link :self))
           (test-value-function c b)))))

    (subtest "Functor Nodes"
      (let ((modules (make-instance 'module-table)))
        (build-source-file #p"./modules/core.trd" modules)
        (build-nodes "a + b -> output; int(x) -> z" modules)

        (with-nodes ((fn "+") (a "a") (b "b")
                     (a+b ("+" "a" "b")) (output "output"))
            modules

          (test-binding a+b output)
          (test-node-function a+b fn fn a b))

        (with-nodes ((fn "int") (x "x") (z "z") (int-x ("int" "x"))) modules
          (test-binding int-x z)
          (test-node-function int-x fn fn x)
          (test-node-function x int-x fn int-x))))

    (subtest "Special Operators"
      (subtest ":op Operator - Registering Infix Operators"
        (flet ((test-op (id operators prec assoc)
                 (diag (format nil "Test infix operator ~a" id))

                 (let ((op (gethash (id-symbol id) operators)))
                   (ok op "Operator registered")
                   (is (length op) 2 "Operator details added")
                   (is (first op) prec "Operator precedence")
                   (is (second op) assoc "Operator associativity"))))

          (let ((modules (make-instance 'module-table)))
            (build-nodes ":op(+, 50, left); :op(-, 70); :op(*, 100, right)" modules)

            (with-slots (operator-nodes) (node-table modules)

              (test-op "+" operator-nodes 50 :left)
              (test-op "-" operator-nodes 70 :right)
              (test-op "*" operator-nodes 100 :right))))

        (subtest "Errors"
          (test-error ":op()")
          (test-error ":op(*)")
          (test-error ":op(*,*)")
          (test-error ":op(*,9,x)")
          (test-error ":op(\"*\", 10, left)")

          (test-top-level-only ":op(*, 10, left)")))

      (subtest ":attribute Operator - Node Attributes"
        (let ((modules (make-instance 'module-table)))
          ;; Build Nodes
          (build-nodes "add(a, b) : +(a, b); node1; node2" modules)

          ;; Build Attribute Declarations
          (build-nodes ":attribute(node1, no-coalesce, 1)" modules)
          (build-nodes ":attribute(add, \"public-name\", \"sum\")" modules)
          (build-nodes ":attribute(node2, input, 1)" modules)

          (with-nodes ((add "add") (node1 "node1") (node2 "node2")) modules
            (is (attribute :no-coalesce node1) 1)
            (is (attribute :public-name add) "sum")
            (is (attribute :input node2) 1)

            (ok (input-node? node2) "(INPUT-NODE? node2)")
            (ok (member node2 (input-nodes (node-table modules))) "node2 in input-nodes")))

        (subtest "Errors"
          (test-error ":attribute()")
          (test-error "node; :attribute(node, attribute)")
          (test-error "node; :attribute(node, 1, 2)")
          (test-error ":attribute(1, attribute, value)")
          (test-error ":attribute(non-existant-node, public-name, \"node\")")

          (test-top-level-only ":attribute(node, input, 1)" "node"))))

    (subtest "Meta-Nodes"
      (subtest "Meta-Node Definitions"
        (let ((modules (make-instance 'module-table)))
          (build-source-file #p"./modules/core.trd" modules)
          (build-nodes "add(x,y) : x + y; add(a,b)" modules)

          (with-nodes ((add "add") (a "a") (b "b") (add-ab ("add" "a" "b"))) modules
            (is-type add 'meta-node)
            (is (operands add) (decls '!\x '!\y))
            (is (definition add) (decls '(!+ !\x !\y)))

            (test-node-function add-ab add add a b)))

        (subtest "Errors"
          (test-error "x : y")
          (test-error "{x; y} : z")
          (test-error "{w; x}(y) : z")
          (test-error ":(x)")
          (test-error ":()")
          (test-error ":(x,y,z)")

          (test-top-level-only "(g(x,y) : f(x,y))")

          (diag "Redefining Special Operators")
          (test-error "->(x, y) : fn(x, y)")
          (test-error ":(x, y) : z")
          (test-error ":extern(x) : x")
          (test-error ":op(a, b) : f(b, a)")
          (test-error "..(x,z) : g(x, z)")
          (test-error ".(a) : h(a)")
          (test-error ":attribute(m, n) : f(m,n)")
          (test-error ":module(m) : m")
          (test-error ":import(x) : x")
          (test-error ":use(z) : z")
          (test-error ":export(y) : h(y)")
          (test-error ":in(x, y) : add(x, y)")

          (let ((modules (make-instance 'module-table)))
            ;; Test node name collisions with meta-nodes
            (build-nodes "a;b" modules)
            (is-error (build-nodes #1="a(x,y) : add(x,y)" modules) 'semantic-error #1#)

            (with-slots (node-table) modules
              (build-nodes "f(x) : x" modules)

              (with-nodes ((f "f")) modules
                (is-type f 'meta-node)
                (is (operands f) (decls '!\x))
                (is (definition f) (decls '!\x)))

              (build-nodes "f(x, y) : +(x, y)" modules)

              ;; Test meta-node redefinitions
              (with-nodes ((f "f")) modules
                (is-type f 'meta-node)
                (is (operands f) (decls '!\x '!\y))
                (is (definition f) (decls '(!+ !\x !\y))))

              ;; Test name collisions with atom nodes
              (is-error (build-nodes "f" modules) 'semantic-error "f")))))

      (subtest "External Meta-Node Definitions"
        (let ((modules (make-instance 'module-table)))
          (build-nodes ":extern(add, sub); add(a,b); sub(a,b)" modules)

          (with-nodes ((add "add") (sub "sub")
                       (a "a") (b "b")
                       (add-ab ("add" "a" "b")) (sub-ab ("sub" "a" "b")))
              modules

            (is-type add 'external-meta-node)
            (is-type sub 'external-meta-node)

            (is (definition add) nil)
            (is (definition sub) nil)

            (test-node-function add-ab add add a b)
            (test-node-function sub-ab sub sub a b)))

        (subtest "Errors"

          (test-top-level-only ":extern(y)"))))

    (subtest "Subnodes"
      (labels ((test-object-fn (node &rest fields)
                 (-<>
                  (list* :object (mapcar (curry #'make-field node) fields))
                  (test-value-function node :object <> :test (rcurry #'set-equal :test #'value-fn-equal))))

               (make-field (node field)
                 (destructuring-bind (field dep) field
                   (list (id-symbol field) (test-binding dep node :object))))

               (test-member-fn (node field dep)
                 (->>
                  `(:member ,(test-binding dep node) ,(id-symbol field))
                  (test-value-function node dep))))

        (let ((modules (make-instance 'module-table)))
          (build-nodes "x -> a.first; y -> a.last; a.last -> y" modules)

          (with-nodes ((x "x") (y "y") (a "a")
                       (a.first ("." "a" "first")) (a.last ("." "a" "last")))
              modules

            (test-object-fn a (list "first" a.first) (list "last" a.last))
            (test-member-fn a.first "first" a)
            (test-member-fn a.last "last" a)

            (test-simple-binding x a.first)
            (test-simple-binding y a.last)
            (test-simple-binding a.last y)))

        (let ((modules (make-instance 'module-table)))
          (build-nodes "Person(first, last) : { first -> self.first; last -> self.last }" modules)
          (build-nodes "Person(first, last).first -> name" modules)

          (with-nodes ((person-fn "Person")
                       (first "first") (last "last")
                       (person ("Person" "first" "last"))
                       (person.first ("." ("Person" "first" "last") "first"))
                       (name "name"))
              modules

            (test-node-function person person-fn person-fn first last)
            (test-member-fn person.first "first" person)
            (test-simple-binding person.first name)))))

    (subtest "Modules"
      (let ((modules (make-instance 'module-table)))
        (build-nodes "x;y" modules)
        (build-nodes ":module(my-mod); a; b" modules)
        (build-nodes ":module(my-mod); +(x,y) : add(x,y); :op(+, 50, left)" modules)
        (build-nodes ":module(my-mod); :export(a,+)" modules)

        (with-modules ((init :init) (my-mod "my-mod"))
            modules
          (test-nodes init "x" "y")

          (with-nodes ((my-mod.a "a") (my-mod.b "b") (+ "+")) my-mod
            (subtest "Test :use operator"
              (build-nodes ":module(mod2); :use(my-mod); my-mod.a -> a; my-mod.+(n,m)" modules)

              (with-modules ((mod2 "mod2")) modules
                (with-nodes ((a "a") (n "n") (m "m") (n+m (("." "my-mod" "+") "n" "m"))) mod2
                  (test-simple-binding my-mod.a a)
                  (test-node-function n+m + + n m))))

            (subtest "Test :alias operator"
              (build-nodes ":module(mod3); :alias(my-mod, m); m.a -> a; m.+(j,k)" modules)

              (with-modules ((mod3 "mod3")) modules
                (with-nodes ((a "a") (j "j") (k "k") (j+k (("." "m" "+") "j" "k"))) mod3
                  (test-simple-binding my-mod.a a)
                  (test-node-function j+k + + j k))))

            (subtest "Test :import operator with arguments"
              (build-nodes ":module(mod4); :import(my-mod, +); a + b" modules)

              (with-modules ((mod4 "mod4")) modules
                (with-nodes ((a "a") (b "b") (a+b ("+" "a" "b"))) mod4
                  (test-node-function a+b + + a b)

                  (isnt a my-mod.a :test #'eq)
                  (isnt b my-mod.b :test #'eq))))

            (subtest "Test :import operator without arguments"
              (build-nodes ":module(mod5); :import(my-mod); a + b" modules)

              (with-modules ((mod5 "mod5")) modules
                (with-nodes ((a "a") (b "b") (a+b ("+" "a" "b"))) mod5
                  (test-node-function a+b + + a b)

                  (is a my-mod.a :test #'eq)
                  (isnt b my-mod.b :test #'eq))))

            (subtest "Test :in operator"
              (build-nodes ":module(mod6); :in(my-mod,+)(:in(my-mod, a), b)" modules)

              (with-modules ((mod6 "mod6")) modules
                (with-nodes ((b "b") (a+b ((":in" "my-mod" "+") (":in" "my-mod" "a") "b"))) mod6
                  (test-node-function a+b + + my-mod.a b)

                  (isnt b my-mod.b :test #'eq))))))

        (subtest "Errors"
          (subtest "Module Semantics"
            (is-error (build-nodes ":module(mod2); +(j,k)" modules) 'semantic-error)
            (is-error (build-nodes ":module(mod2); j + k" modules) 'tridash-parse-error)
            (is-error (build-nodes ":module(mod2); my-mod.z" modules) 'semantic-error)

            (is-error (build-nodes ":module(mod3); +(j,k)" modules) 'semantic-error)
            (is-error (build-nodes ":module(mod3); j + k" modules) 'tridash-parse-error)
            (is-error (build-nodes ":module(mod3); m.z" modules) 'semantic-error)

            (is-error (build-nodes ":module(mod4); my-mod.+(a,b)" modules) 'semantic-error)
            (is-error (build-nodes ":module(mod4); :in(my-mod, z)" modules) 'semantic-error)

            (test-error ":use(no-such-module)")
            (test-error ":alias(no-such-module, m)")
            (test-error ":import(no-such-module)")
            (test-error ":import(no-such-module, node)")
            (test-error ":in(no-such-module, x)")
            (test-error ":export(no-such-node)")
            (test-error "x; :export(x, no-such-node)"))

          (subtest ":module Operator Syntax"
            (test-error ":module()")
            (test-error ":module(a, b, c)")
            (test-error ":module(1, 2, 3)")
            (test-error ":module(1)")

            (test-top-level-only ":module(m)"))

          (subtest ":use Operator Syntax"
            (test-error ":use(1,2,3)")
            (test-top-level-only ":use(m1)" ":module(m1)" ":module(m2)"))

          (subtest ":alias Operator Syntax"
            (test-error ":alias()")
            (test-error ":alias(m)")
            (test-error ":alias(1)")
            (test-error ":alias(1,2)")
            (test-error ":module(m1); :module(m2); :alias(m1, m, x, y)")

            (test-top-level-only ":alias(mod, m)" ":module(mod)" ":module(m1)"))

          (subtest ":import Operator Syntax"
            (test-error ":import()")
            (test-error ":import(1)")

            (test-top-level-only ":import(mod)" ":module(mod)" ":module(m1)")
            (test-top-level-only ":import(mod, x)" ":module(mod); x" ":module(m1)"))

          (subtest ":export Operator Syntax"
            (test-error ":export(1, 2, 3)")

            (test-top-level-only ":export(x)" "x"))

          (subtest ":in Operator Syntax"
            (test-error ":in()")
            (test-error ":in(x)")
            (test-error ":module(m1); x; :module(m2); :in(m1,x, y)")
            (test-error ":in(1, x)"))))))

  (macrolet
      ((has-value-function ((&rest deps) node function)
         "Tests that the value function of NODE is equal to
          FUNCTION.

          DEPS is a list of the dependency nodes which are passed to
          WITH-DEPENDENCIES.

          FUNCTION is evaluated as if in the body of the
          WITH-DEPENDENCIES form with DEPS bound to the dependency
          `NODE-LINK' objects.

          The value function of the context of the `NODE-LINK',
          corresponding to the first dependency in DEPS, is tested
          that it is equal to FUNCTION."

         (with-gensyms (context)
           (once-only (node)
             `(with-dependencies ,deps ,node
                (let ((,context (node-link-context ,(ensure-car (first deps)))))
                  (test-value-function ,node ,context ,function)))))))

    (subtest "Test Node Coalescer"
      (subtest "Simple Nodes"
        (let ((modules (make-instance 'module-table)))
          (diag "One-Way Bindings")
	  (build-nodes "a -> b; b -> c; c -> d" modules)
	  (build-nodes ":attribute(a, input, 1)" modules)
	  (finish-build-graph modules)

	  (test-not-nodes modules "b" "c")

	  (with-nodes ((a "a") (d "d")) modules
            (has-value-function (a) d a)))

        (let ((modules (make-instance 'module-table)))
          (diag "Two-Way Bindings")
          (build-nodes "a -> b; b -> c; c -> d" modules)
          (build-nodes "d -> c; c -> b; b -> a" modules)
          (build-nodes ":attribute(a, input, 1)" modules)
          (finish-build-graph modules)

          (test-not-nodes modules "b" "c")

	  (with-nodes ((a "a") (d "d")) modules
            (has-value-function (a) d a)))

        (let ((modules (make-instance 'module-table)))
          (diag "Multiple Observers")
          (build-nodes "a -> b; b -> c; b -> d; c -> e; d -> f" modules)
          (build-nodes ":attribute(a, input, 1)" modules)
          (finish-build-graph modules)

          (test-not-nodes modules "c" "d")

          (with-nodes ((a "a") (b "b") (e "e") (f "f")) modules
            (has-value-function (a) b a)
            (has-value-function (b) e b)
            (has-value-function (b) f b))))

      (subtest "Functor Nodes"
        (let ((modules (make-instance 'module-table)))
          (diag "Simple Functor Nodes")

          (build-nodes ":extern(+); :op(+, 50, left)" modules)
          (build-nodes "a + b + c + d -> output" modules)
          (build-nodes ":attribute(a, input, 1)" modules)
          (build-nodes ":attribute(b, input, 1)" modules)
          (build-nodes ":attribute(c, input, 1)" modules)
          (build-nodes ":attribute(d, input, 1)" modules)
          (finish-build-graph modules)

          (test-not-nodes modules
                          '("+" "a" "b")
                          '("+" ("+" "a" "b") "c")
                          '("+" ("+" ("+" "a" "b") "c") "d"))

          (with-nodes ((+ "+") (a "a") (b "b") (c "c") (d "d") (output "output")) modules
            (has-value-function (a b c d) output `(,+ (,+ (,+ ,a ,b) ,c) ,d))))

        (subtest "Multiple Observers"
          (let ((modules (make-instance 'module-table)))
            (build-nodes ":extern(+); :op(+, 50, left)" modules)
            (build-nodes "a + b + c + d -> out1" modules)
            (build-nodes "a + b -> out2" modules)
            (build-nodes ":attribute(a, input, 1)" modules)
            (build-nodes ":attribute(b, input, 1)" modules)
            (build-nodes ":attribute(c, input, 1)" modules)
            (build-nodes ":attribute(d, input, 1)" modules)
            (finish-build-graph modules)

            (test-not-nodes modules
                            '("+" ("+" "a" "b") "c")
                            '("+" ("+" ("+" "a" "b") "c") "d"))

            (with-nodes ((+ "+") (a "a") (b "b") (c "c") (d "d")
                         (a+b ("+" "a" "b"))
                         (out1 "out1") (out2 "out2"))
                modules

              (has-value-function (a+b c d) out1 `(,+ (,+ ,a+b ,c) ,d))

              (has-value-function (a+b) out2 a+b)
              (has-value-function (a b) a+b `(,+ ,a ,b))))

          (let ((modules (make-instance 'module-table)))
            (build-nodes ":extern(add)" modules)
            (build-nodes "a -> b; b -> c; b -> d; add(c,d) -> e" modules)
            (build-nodes ":attribute(a, input, 1)" modules)
            (finish-build-graph modules)

            (test-not-nodes modules "b" "c" "d")

            (with-nodes ((add "add") (a "a") (e "e")) modules
              (has-value-function (a) e (list add a a))))))

      (subtest "Removing Unreachable Nodes"
        (let ((modules (make-instance 'module-table)))
          (build-nodes "a -> b; b -> c;" modules)
          (build-nodes "e -> f" modules) ; Unreachable Nodes
          (build-nodes ":attribute(a, input, 1)" modules)
          (finish-build-graph modules)

          (test-not-nodes modules "b" "e" "f")

          (with-nodes ((a "a") (c "c")) modules
            (has-value-function (a) c a)))

        (subtest "Unreachable Dependency Errors"
          (let ((modules (make-instance 'module-table)))
            (build-nodes ":extern(add)" modules)
            (build-nodes "a -> b; add(b, d) -> output" modules)
            (build-nodes "c -> d" modules) ; Unreachable Nodes
            (build-nodes ":attribute(a, input, 1)" modules)
            (is-error (finish-build-graph modules) 'semantic-error))))

      (subtest "Cross-Module Bindings"
        (subtest "Simple Bindings"
          (let ((modules (make-instance 'module-table)))
            (build-nodes ":module(m1); a -> b; b -> c; c -> d" modules)
            (build-nodes ":attribute(a, input, 1)" modules)

            (build-nodes ":module(m2); :use(m1); m1.b -> a; b -> c; c -> d" modules)
            (build-nodes ":attribute(b, input, 1)" modules)

            (finish-build-graph modules)

            (with-modules ((m1 "m1") (m2 "m2")) modules
              (test-not-nodes m1 "c")
              (test-not-nodes m2 "c")

              (with-nodes ((m1.a "a") (m1.b "b") (m1.d "d")) m1
                (has-value-function (m1.a) m1.b m1.a)
                (has-value-function (m1.b) m1.d m1.b)

                (with-nodes ((m2.a "a") (m2.b "b") (m2.d "d")) m2
                  (has-value-function (m1.b) m2.a m1.b)
                  (has-value-function (m2.b) m2.d m2.b))))))))))

(finalize)

(cl-interpol:disable-interpol-syntax)
