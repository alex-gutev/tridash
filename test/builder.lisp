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

(defpackage :tridash.test.builder
  (:use :cl
        :alexandria
        :anaphora
        :cl-arrows
        :iterate
        :optima
        :prove

        :tridash.parser
        :tridash.frontend

        :tridash.test.util)

  (:shadowing-import-from :prove :fail)

  (:import-from :lol :defmacro!)

  (:import-from :tridash.parser
                :tridash-parse-error))


(in-package :tridash.test.builder)

(cl-interpol:enable-interpol-syntax)

(plan nil)


;;;; Building From Strings

(defun build-nodes (string modules)
  "Builds the nodes parsed from STRING into the `MODULE-TABLE'
   MODULES."

  (with-input-from-string (in string)
    (build-parsed-nodes (make-parser in) modules)))


;;;; Getting a Node-Table

(defun ensure-node-table (thing)
  "If THING is a `MODULE-TABLE' return the node table bound to the
   NODE-TABLE slot. Otherwise if THING is a `NODE-TABLE' return it."

  (etypecase thing
    (module-table (node-table thing))
    (node-table thing)))


;;;; Test Functions

;;; Test for the existence of nodes

(defun test/get-node (name node-table)
  "Retrieves the node with name NAME from NODE-TABLE and checks that
   its NAME slot matches NAME."

  (let ((id (node-id name)))
    (aprog1 (gethash id (all-nodes node-table))
      (subtest (format nil "Test Node: ~a" name)
        (is-type it 'node (format nil "~a is a node" name))
        (is (name it) id (format nil "Node name = ~a" name))))))

(defun test-nodes (modules &rest ids)
  "Tests that each identifier in IDS names a node in MODULES."

  (mapc (rcurry #'test/get-node (ensure-node-table modules)) ids))


(defun test-not-node (modules id)
  "Tests that there is no node with identifier ID in MODULES."

  (ok (null (gethash (node-id id) (all-nodes (ensure-node-table modules))))
      (format nil "~a is not a node" id)))

(defun test-not-nodes (modules &rest ids)
  "Tests that there are no nodes with identifiers IDS in MODULES."

  (mapc (curry #'test-not-node modules) ids))


;;; Test Bindings

(defun test-dependency (dep node)
  "Tests that DEP is a dependency of NODE and returns the
   `NODE-LINK'."

  (aprog1 (gethash dep (dependencies node))
    (ok it (format nil "~a is a dependency of ~a" dep node))))

(defun test-binding (src target &optional (context src))
  "Tests that a binding between node SRC and TARGET has been
   established in the context CONTEXT."

  (let ((link (gethash target (observers src))))
    (subtest (format nil "Test binding ~a -> ~a" (name src) (name target))
      (is-type link 'node-link "Node link created")
      (is link (gethash src (dependencies target))
	  (format nil "Node link added to dependencies of ~a" (name target)))

      (is (node-link-node link) src "Node link points to correct node")
      (is (node-link-context link) context "Link context is correct")

      (let ((id context)
	    (context (gethash context (contexts target))))
        (is-type context 'node-context (format nil "Node context ~a created" id))
        (is (gethash src (operands context)) link
	    (format nil "~a added to context operands" (name src)))))

    link))

(defun test-simple-binding (src target &optional (context src))
  "Tests that a binding between node SRC and TARGET has been
   established and that the value function of TARGET (in context
   CONTEXT) is the `NODE-LINK' itself."

  (->>
   (test-binding src target context)
   (test-value-function target context)))


;;; Test Value Functions

(defun value-fn-equal (a b)
  "Value function equality comparison. Returns true if the value
   function A is equivalent to the value function B."

  (multiple-value-match (values a b)
    (((cons a as) (cons b bs))
     (and (value-fn-equal a b)
	  (value-fn-equal as bs)))

    (((node-link- (node node-a) (context context-a))
      (node-link- (node node-b) (context context-b)))

     (and (eq node-a node-b)
	  (eq context-a context-b)))

    ((_ _) (equal a b))))

(defun test-value-function (node context fn &key (test #'value-fn-equal))
  "Tests that the context CONTEXT of node NODE has value function FN."

  (diag (format nil "Test value function of ~a in context ~a" (name node) context))

  (let ((context (gethash context (contexts node))))
    (is (value-function context) fn :test test)))

(defun test-node-function (node context fn &rest operands)
  "Tests that node NODE has a context CONTEXT with value function FN
   applied to operands OPERANDS."

  (->>
   (list* fn (mapcar (rcurry #'test-binding node context) operands))
   (test-value-function node context)))

(defun init-context (node)
  "Checks whether node has an init context and returns its identifier."

  (aprog1
      (iter (for (id context) in-hashtable (contexts node))
            (finding id such-that
                     (and (not (eq id :input))
                          (zerop (hash-table-count (operands context))))))
    (ok it (format nil "~a has an :INIT context" node))))


;;; Test Errors

(defun test-error (str &optional (error 'semantic-error))
  "Tests that building the source STR results in a `SEMANTIC-ERROR'."

  (is-error (build-nodes str (make-instance 'module-table)) error
	    (format nil "`~a` raises an error of type `~s'" str error)))

(defun test-top-level-only (decl &rest code)
  "Tests that an error is raised if the declaration DECL appears in a
   non-top-level position. Each element in CODE is
   prepended (separated by ';') to DECL before performing the tests."

  (test-error (format nil "~{~a; ~}~a -> a" code decl))
  (test-error (format nil "~{~a; ~}a -> ~a" code decl))
  (test-error (format nil "~{~a; ~}f(x) : x; f(~a)" code decl)))


;;;; Utility Macros

(defmacro! with-module-table (var &body body)
  "Creates a new `MODULE-TABLE' and binds it to VAR. The binding is
   visible to the forms in BODY.

   Two lexical function definitions are visible to the forms in BODY:
   BUILD and FINISH-BUILD. BUILD builds each source string, passed as
   an argument, into the module-table VAR. FINISH-BUILD calls
   FINISH-BUILD-GRAPH on the module-table VAR."

  `(let ((,var (make-instance 'module-table)))
     (flet ((build (&rest ,g!strings)
              (mapc (rcurry #'build-nodes ,var) ,g!strings))

            (finish-build (&optional (,g!module-table ,var))
              (finish-build-graph ,g!module-table)))
      ,@body)))

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

(defmacro! has-value-function ((&rest deps) o!node function)
  "Tests that the value function of NODE is equal to FUNCTION.

   DEPS is a list of the dependency nodes which are passed to
   WITH-DEPENDENCIES.

   FUNCTION is evaluated as if in the body of the WITH-DEPENDENCIES
   form with DEPS bound to the dependency `NODE-LINK' objects.

   The value function of the context of the `NODE-LINK', corresponding
   to the first dependency in DEPS, is tested that it is equal to
   FUNCTION."

  `(with-dependencies ,deps ,g!node
     (let ((,g!context (node-link-context ,(ensure-car (first deps)))))
       (test-value-function ,g!node ,g!context ,function))))


;;;; Tests

(deftest builder
 (subtest "Test Node Builder"
   (subtest "Simple Atom Nodes"
     (with-module-table modules
       (build "a;b;c")

       (test-nodes modules "a" "b" "c")))

   (subtest "Bindings"
     (with-module-table modules
       (build "a -> b; c -> b")

       (with-nodes ((a "a") (b "b") (c "c")) modules
         (test-simple-binding a b)
         (test-simple-binding c b)))

     (with-module-table modules
       (build "a -> (b -> c)")

       (with-nodes ((a "a") (b "b") (c "c") (b->c ("->" "b" "c"))) modules
         (test-simple-binding a b->c a)

         (->>
          `(if ,(test-binding b->c c b)
               ,(test-binding b c)
               ,(node-link :self))
          (test-value-function c b)))))

   (subtest "Functor Nodes"
     (with-module-table modules
       (build-source-file #p"./modules/core.trd" modules)
       (build "a + b -> output; int(x) -> z")

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

         (with-module-table modules
           (build ":op(+, 50, left); :op(-, 70); :op(*, 100, right)")

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
       (with-module-table modules
         ;; Build Nodes
         (build "add(a, b) : +(a, b); node1; node2")

         ;; Build Attribute Declarations
         (build ":attribute(node1, no-coalesce, 1)")
         (build ":attribute(add, \"public-name\", \"sum\")")
         (build ":attribute(node2, input, 1)")

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
       (with-module-table modules
         (build-source-file #p"./modules/core.trd" modules)
         (build "add(x,y) : x + y; add(a,b)")

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

         (with-module-table modules
           ;; Test node name collisions with meta-nodes
           (build "a;b")
           (is-error (build #1="a(x,y) : add(x,y)") 'semantic-error #1#)

           (with-slots (node-table) modules
             (build "f(x) : x")

             (with-nodes ((f "f")) modules
               (is-type f 'meta-node)
               (is (operands f) (decls '!\x))
               (is (definition f) (decls '!\x)))

             (build "f(x, y) : +(x, y)")

             ;; Test meta-node redefinitions
             (with-nodes ((f "f")) modules
               (is-type f 'meta-node)
               (is (operands f) (decls '!\x '!\y))
               (is (definition f) (decls '(!+ !\x !\y))))

             ;; Test name collisions with atom nodes
             (is-error (build "f") 'semantic-error "f")))))

     (subtest "External Meta-Node Definitions"
       (with-module-table modules
         (build ":extern(add, sub); add(a,b); sub(a,b)")

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

       (with-module-table modules
         (build "x -> a.first; y -> a.last; a.last -> y")

         (with-nodes ((x "x") (y "y") (a "a")
                      (a.first ("." "a" "first")) (a.last ("." "a" "last")))
             modules

           (test-object-fn a (list "first" a.first) (list "last" a.last))
           (test-member-fn a.first "first" a)
           (test-member-fn a.last "last" a)

           (test-simple-binding x a.first)
           (test-simple-binding y a.last)
           (test-simple-binding a.last y)))

       (with-module-table modules
         (build "Person(first, last) : { first -> self.first; last -> self.last }"
                "Person(first, last).first -> name")

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
     (with-module-table modules
       (build "x;y"
              ":module(my-mod); a; b"
              ":module(my-mod); +(x,y) : add(x,y); :op(+, 50, left)"
              ":module(my-mod); :export(a,+)")

       (with-modules ((init :init) (my-mod "my-mod"))
           modules
         (test-nodes init "x" "y")

         (with-nodes ((my-mod.a "a") (my-mod.b "b") (+ "+")) my-mod
           (subtest "Test :use operator"
             (build ":module(mod2); :use(my-mod); my-mod.a -> a; my-mod.+(n,m)")

             (with-modules ((mod2 "mod2")) modules
               (with-nodes ((a "a") (n "n") (m "m") (n+m (("." "my-mod" "+") "n" "m"))) mod2
                 (test-simple-binding my-mod.a a)
                 (test-node-function n+m + + n m))))

           (subtest "Test :alias operator"
             (build ":module(mod3); :alias(my-mod, m); m.a -> a; m.+(j,k)")

             (with-modules ((mod3 "mod3")) modules
               (with-nodes ((a "a") (j "j") (k "k") (j+k (("." "m" "+") "j" "k"))) mod3
                 (test-simple-binding my-mod.a a)
                 (test-node-function j+k + + j k))))

           (subtest "Test :import operator with arguments"
             (build ":module(mod4); :import(my-mod, +); a + b")

             (with-modules ((mod4 "mod4")) modules
               (with-nodes ((a "a") (b "b") (a+b ("+" "a" "b"))) mod4
                 (test-node-function a+b + + a b)

                 (isnt a my-mod.a :test #'eq)
                 (isnt b my-mod.b :test #'eq))))

           (subtest "Test :import operator without arguments"
             (build ":module(mod5); :import(my-mod); a + b")

             (with-modules ((mod5 "mod5")) modules
               (with-nodes ((a "a") (b "b") (a+b ("+" "a" "b"))) mod5
                 (test-node-function a+b + + a b)

                 (is a my-mod.a :test #'eq)
                 (isnt b my-mod.b :test #'eq))))

           (subtest "Test :in operator"
             (build ":module(mod6); :in(my-mod,+)(:in(my-mod, a), b)")

             (with-modules ((mod6 "mod6")) modules
               (with-nodes ((b "b") (a+b ((":in" "my-mod" "+") (":in" "my-mod" "a") "b"))) mod6
                 (test-node-function a+b + + my-mod.a b)

                 (isnt b my-mod.b :test #'eq))))))

       (subtest "Errors"
         (subtest "Module Semantics"
           (is-error (build ":module(mod2); +(j,k)") 'semantic-error)
           (is-error (build ":module(mod2); j + k") 'tridash-parse-error)
           (is-error (build ":module(mod2); my-mod.z") 'semantic-error)

           (is-error (build ":module(mod3); +(j,k)") 'semantic-error)
           (is-error (build ":module(mod3); j + k") 'tridash-parse-error)
           (is-error (build ":module(mod3); m.z") 'semantic-error)

           (is-error (build ":module(mod4); my-mod.+(a,b)") 'semantic-error)
           (is-error (build ":module(mod4); :in(my-mod, z)") 'semantic-error)

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
           (test-error ":in(1, x)")))))))

(run-test 'builder)


(deftest coalescer
 (subtest "Test Node Coalescer"
   (subtest "Simple Nodes"
     (subtest "One-Way Bindings"
       (with-module-table modules
	 (build "a -> b; b -> c; c -> d"
	        ":attribute(a, input, 1)")

	 (finish-build)

	 (test-not-nodes modules "b" "c")

	 (with-nodes ((a "a") (d "d")) modules
           (has-value-function (a) d a)))

       (subtest "NO-COALESCE Attribute"
         (with-module-table modules
           (build "a -> b; b -> c; c -> d"
                  ":attribute(a, input, 1)"
                  ":attribute(b, no-coalesce, 1)")

           (finish-build)

           (test-not-nodes modules "c")

           (with-nodes ((a "a") (b "b") (d "d")) modules
             (has-value-function (a) b a)
             (has-value-function (b) d b)))))

     (subtest "Two-Way Bindings"
       (with-module-table modules
         (build "a -> b; b -> c; c -> d"
                "d -> c; c -> b; b -> a"
                ":attribute(a, input, 1)")

         (finish-build)

         (test-not-nodes modules "b" "c")

	 (with-nodes ((a "a") (d "d")) modules
           (has-value-function (a) d a)))

       (subtest "NO-COALESCE Attribute"
         (with-module-table modules
           (build "a -> b; b -> c; c -> d"
                  "d -> c; c -> b; b -> a"
                  ":attribute(a, input, 1)"
                  ":attribute(b, input, 1)")

           (finish-build)

           (test-not-nodes modules "c")

	   (with-nodes ((a "a") (b "b") (d "d")) modules
             (test-simple-binding a b)
             (test-simple-binding b a)

             (has-value-function (b) d b)))))

     (subtest "Multiple Observers"
       (with-module-table modules
         (build "a -> b; b -> c; b -> d; c -> e; d -> f"
                ":attribute(a, input, 1)")

         (finish-build)

         (test-not-nodes modules "c" "d")

         (with-nodes ((a "a") (b "b") (e "e") (f "f")) modules
           (has-value-function (a) b a)
           (has-value-function (b) e b)
           (has-value-function (b) f b)))))

   (subtest "Functor Nodes"
     (subtest "Simple Functor Nodes"
       (with-module-table modules
         (build ":extern(+); :op(+, 50, left)"
                "a + b + c + d -> output"
                ":attribute(a, input, 1)"
                ":attribute(b, input, 1)"
                ":attribute(c, input, 1)"
                ":attribute(d, input, 1)")

         (finish-build)

         (test-not-nodes modules
                         '("+" "a" "b")
                         '("+" ("+" "a" "b") "c")
                         '("+" ("+" ("+" "a" "b") "c") "d"))

         (with-nodes ((+ "+") (a "a") (b "b") (c "c") (d "d") (output "output")) modules
           (has-value-function (a b c d) output `(,+ (,+ (,+ ,a ,b) ,c) ,d))))

       (subtest "NO-COALESCE Attribute"
         (with-module-table modules
           (build ":extern(+); :op(+, 50, left)"
                  "a + b + c + d -> output"
                  ":attribute(a, input, 1)"
                  ":attribute(b, input, 1)"
                  ":attribute(c, input, 1)"
                  ":attribute(d, input, 1)"
                  ":attribute(a + b, no-coalesce, 1)")

           (finish-build)

           (test-not-nodes modules
                           '("+" ("+" "a" "b") "c")
                           '("+" ("+" ("+" "a" "b") "c") "d"))

           (with-nodes ((+ "+") (a "a") (b "b") (c "c") (d "d")
                        (a+b ("+" "a" "b"))
                        (output "output"))
               modules

             (has-value-function (a b) a+b `(,+ ,a ,b))
             (has-value-function (a+b c d) output `(,+ (,+ ,a+b ,c) ,d))))))

     (subtest "Multiple Observers"
       (with-module-table modules
         (build ":extern(+); :op(+, 50, left)"
                "a + b + c + d -> out1"
                "a + b -> out2"
                ":attribute(a, input, 1)"
                ":attribute(b, input, 1)"
                ":attribute(c, input, 1)"
                ":attribute(d, input, 1)")

         (finish-build)

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

       (with-module-table modules
         (build ":extern(add)"
                "a -> b; b -> c; b -> d; add(c,d) -> e"
                ":attribute(a, input, 1)")
         (finish-build)

         (test-not-nodes modules "b" "c" "d")

         (with-nodes ((add "add") (a "a") (e "e")) modules
           (has-value-function (a) e (list add a a))))))

   (subtest "Removing Unreachable Nodes"
     (with-module-table modules
       (build "a -> b; b -> c;"
              "e -> f" ; Unreachable Nodes
              ":attribute(a, input, 1)")

       (finish-build)

       (test-not-nodes modules "b" "e" "f")

       (with-nodes ((a "a") (c "c")) modules
         (has-value-function (a) c a)))

     (subtest "NO-REMOVE Attribute"
       (with-module-table modules
         (build "some-special-node"
                ":attribute(some-special-node, no-remove, 1)")

         (finish-build)

         (test-nodes modules "some-special-node")))

     (subtest "Unreachable Dependency Errors"
       (with-module-table modules
         (build ":extern(add)"
                "a -> b; add(b, d) -> output"
                "c -> d" ; Unreachable Nodes
                ":attribute(a, input, 1)")

         (is-error (finish-build) 'semantic-error))))

   (subtest "Cross-Module Bindings"
     (subtest "Simple Bindings"
       (with-module-table modules
         (build ":module(m1); a -> b; b -> c; c -> d"
                ":attribute(a, input, 1)")

         (build ":module(m2); :use(m1); m1.b -> a; b -> c; c -> d"
                ":attribute(b, input, 1)")

         (finish-build)

         (with-modules ((m1 "m1") (m2 "m2")) modules
           (test-not-nodes m1 "c")
           (test-not-nodes m2 "c")

           (with-nodes ((m1.a "a") (m1.b "b") (m1.d "d")) m1
             (has-value-function (m1.a) m1.b m1.a)
             (has-value-function (m1.b) m1.d m1.b)

             (with-nodes ((m2.a "a") (m2.b "b") (m2.d "d")) m2
               (has-value-function (m1.b) m2.a m1.b)
               (has-value-function (m2.b) m2.d m2.b))))))

     (subtest "Functor Nodes"
       (with-module-table modules
         (build ":module(m1); in -> a; in -> b"
                ":attribute(in, input, 1)")

         (build ":module(m2); :use(m1);"
                ":extern(+); :op(+, 50, left)"
                "m1.a + m1.b + c -> output"
                ":attribute(c, input, 1)")

         (finish-build)

         (with-modules ((m1 "m1") (m2 "m2")) modules
           (test-not-nodes m1 "a" "b")
           (test-not-nodes m2 '("+" ("+" ("." "m1" "a") ("." "m1" "b")) "c"))

           (with-nodes ((in "in")) m1
             (with-nodes ((+ "+") (c "c") (output "output")) m2
               (has-value-function (in c) output `(,+ (,+ ,in ,in) ,c))))))))))

(run-test 'coalescer)


(deftest constant-folding
  (subtest "Test Constant Folding"
    (subtest "Literal Constant Values"
      (with-module-table modules
        (build ":extern(add)"
               "add(a, b) -> c"
               "1 -> constant; constant -> b"
               ":attribute(a, input, 1)")

        (finish-build)

        (test-not-nodes modules "b" "constant")

        (with-nodes ((add "add") (a "a") (c "c")) modules
          (has-value-function (a) c `(,add ,a 1))))

      (with-module-table modules
        (build "a -> b; b -> c"
               "3 -> constant; constant -> b"
               ":attribute(a, input, 1)")
        (finish-build)

        (test-not-nodes modules "constant")

        (with-nodes ((a "a") (b "b") (c "c")) modules
          (test-simple-binding a b)
          (test-simple-binding b c)

          (test-value-function b (init-context b) 3)))

      (with-module-table modules
        (build "a -> b; b -> c"
               "\"hello\" -> constant; constant -> a"
               ":attribute(a, input, 1)")
        (finish-build)

        (test-not-nodes modules "b" "constant")

        (with-nodes ((a "a") (c "c")) modules
          (has-value-function (a) c a)

          (test-value-function a (init-context a) "hello"))))

    (subtest "Constant Functor Nodes"
      (with-module-table modules
        (build ":extern(add)"
               "add(a, b) -> c"

               "1 -> c1; 2 -> c2; c2 -> c3"
               "add(c1, c3) -> b"

               ":attribute(a, input, 1)")

        (finish-build)

        (test-not-nodes modules "b" "c1" "c2" "c3")

        (with-nodes ((add "add") (a "a") (c "c")) modules
          (has-value-function (a) c `(,add ,a (,add 1 2)))))

      (with-module-table modules
        (build ":extern(add)"
               "a -> b; b -> c"

               "5 -> c1; 10 -> c2; c2 -> c3"
               "add(c1, add(c2, c3)) -> b"

               ":attribute(a, input, 1)")

        (finish-build)

        (test-not-nodes modules "c1" "c2" "c3")

        (with-nodes ((add "add") (a "a") (b "b") (c "c")) modules
          (test-simple-binding a b)
          (test-simple-binding b c)

          (test-value-function b (init-context b) `(,add 5 (,add 10 10)))))

      (with-module-table modules
        (build "a -> b; b -> c"

               "\"hello\" -> constant; constant -> a"
               "constant -> c"

               ":attribute(a, input, 1)")

        (finish-build)

        (test-not-nodes modules "b" "constant")

        (with-nodes ((a "a") (c "c")) modules
          (has-value-function (a) c a)

          (test-value-function a (init-context a) "hello")
          (test-value-function c (init-context c) "hello"))))

    (subtest "Cross-Module Constant Folding"
      (with-module-table modules
        (build ":module(m1)"
               "1 -> constant"
               "constant -> param"

               ":module(m2)"
               ":use(m1)"
               ":extern(add)"

               "2 -> param"
               "add(param, m1.param) -> sum"
               "add(a, sum) -> b"
               "b -> c"

               ":attribute(a, input, 1)")

        (finish-build)

        (with-modules ((m1 "m1") (m2 "m2")) modules
          (test-not-nodes m1 "constant" "param")
          (test-not-nodes m2 "param" "sum" "b")

          (with-nodes ((add "add") (a "a") (c "c")) m2
            (has-value-function (a) c `(,add ,a (,add 2 1))))))

      ;; Switch module names to test that constant folding is not
      ;; dependent on the way the modules are ordered
      (with-module-table modules
        (build ":module(m2)"
               "1 -> constant"
               "constant -> param"

               ":module(m1)"
               ":use(m2)"
               ":extern(add)"

               "2 -> param"
               "add(param, m2.param) -> sum"
               "add(a, sum) -> b"
               "b -> c"

               ":attribute(a, input, 1)")

        (finish-build)

        (with-modules ((m1 "m1") (m2 "m2")) modules
          (test-not-nodes m2 "constant" "param")
          (test-not-nodes m1 "param" "sum" "b")

          (with-nodes ((add "add") (a "a") (c "c")) m1
            (has-value-function (a) c `(,add ,a (,add 2 1)))))))))

(run-test 'constant-folding)


(deftest structure-checks
  (subtest "Test Structure Checking"
    (subtest "Cycle Checks"
      (subtest "Simple Bindings"
        (with-module-table modules
          (build "a -> b"
                 "b -> c"

                 "c -> d"
                 "c -> out2"

                 "d -> out1"
                 "d -> b"

                 ":attribute(a, input, 1)")

          (is-error (finish-build) 'node-cycle-error))

        (subtest "Cross-Module Bindings"
          (with-module-table modules
            (build ":module(m1)"
                   "a -> b"
                   "b -> c"

                   ":attribute(a, input, 1)"

                   ":module(m2)"
                   ":use(m1)"

                   "m1.c -> d"
                   "m1.c -> out2"

                   "d -> out1"
                   "d -> m1.b")

            (is-error (finish-build) 'node-cycle-error))

          ;; Switch module names to test a possibly different module
          ;; order.
          (with-module-table modules
            (build ":module(m2)"
                   "a -> b"
                   "b -> c"

                   ":attribute(a, input, 1)"

                   ":module(m1)"
                   ":use(m2)"

                   "m2.c -> d"
                   "m2.c -> out2"

                   "d -> out1"
                   "d -> m2.b")

            (is-error (finish-build) 'node-cycle-error))))

      (subtest "Functional Bindings"
        (with-module-table modules
          (build ":extern(add)"
                 "add(a, b) -> c"

                 "c -> out1"
                 "c -> d"

                 "d -> out2"
                 "d -> a"

                 ":attribute(a, input, 1)"
                 ":attribute(b, input, 1)")

          (is-error (finish-build) 'node-cycle-error))

        (subtest "Cross-Module Bindings"
          (with-module-table modules
            (build ":module(m1)"
                   "a; c"
                   ":attribute(a, input, 1)"

                   ":module(m2)"
                   ":use(m1)"

                   ":extern(add)"
                   "add(m1.a, b) -> m1.c"

                   "m1.c -> out1"
                   "m1.c -> d"

                   "d -> out2"
                   "d -> m1.a"

                   ":attribute(b, input, 1)")

            (is-error (finish-build) 'node-cycle-error))

          ;; Switch module names to test possibly different module
          ;; order
          (with-module-table modules
            (build ":module(m2)"
                   "a; c"
                   ":attribute(a, input, 1)"

                   ":module(m1)"
                   ":use(m2)"

                   ":extern(add)"
                   "add(m2.a, b) -> m2.c"

                   "m2.c -> out1"
                   "m2.c -> d"

                   "d -> out2"
                   "d -> m2.a"

                   ":attribute(b, input, 1)")

            (is-error (finish-build) 'node-cycle-error)))))

    (subtest "Ambiguous Context Checks"
      (subtest "Simple Bindings"
        (with-module-table modules
          (build "a -> d"
                 "a -> b; b -> c; c -> d"
                 "d -> e"

                 ":attribute(a, input, 1)")

          (is-error (finish-build) 'ambiguous-context-error))

        (subtest "Cross-Module Bindings"
          (with-module-table modules
            (build ":module(m1)"
                   "a -> d"
                   ":attribute(a, input, 1)"

                   ":module(m2)"
                   ":use(m1)"

                   "m1.a -> b; b -> c; c -> m1.d"
                   "m1.d -> e")

            (is-error (finish-build) 'ambiguous-context-error))

          ;; Switch module names to test possibly different module
          ;; order.
          (with-module-table modules
            (build ":module(m2)"
                   "a -> d"
                   ":attribute(a, input, 1)"

                   ":module(m1)"
                   ":use(m2)"

                   "m2.a -> b; b -> c; c -> m2.d"
                   "m2.d -> e")

            (is-error (finish-build) 'ambiguous-context-error))))

      (subtest "Functional Bindings"
        (with-module-table modules
          (build ":extern(add)"
                 "add(a, b) -> d"
                 "a -> c; c -> d"
                 ":attribute(a, input, 1)"
                 ":attribute(b, input, 1)")

          (is-error (finish-build) 'ambiguous-context-error))

        (subtest "Cross-Module Bindings"
          (with-module-table modules
            (build ":module(m1)"
                   "a; b;"
                   ":attribute(a, input, 1)"
                   ":attribute(b, input, 1)"

                   ":module(m2)"
                   ":use(m1)"
                   ":extern(add)"

                   "add(m1.a, m1.b) -> d"
                   "m1.a -> c; c -> d"
                   "d -> e")

            (is-error (finish-build) 'ambiguous-context-error))

          ;; Switch module names to test possibly different module
          ;; order.
          (with-module-table modules
            (build ":module(m2)"
                   "a; b;"
                   ":attribute(a, input, 1)"
                   ":attribute(b, input, 1)"

                   ":module(m1)"
                   ":use(m2)"
                   ":extern(add)"

                   "add(m2.a, m2.b) -> d"
                   "m2.a -> c; c -> d"
                   "d -> e")

            (is-error (finish-build) 'ambiguous-context-error)))))))

(run-test 'structure-checks)


(defmacro! test-meta-node (o!meta-node (&rest operands) function)
  "Test that various properties of a meta-node are correct, namely
   that its definition has been built, its subgraph contains the
   implicit self-node and that its value-function is correct.

   OPERANDS is a list of the meta-node's operands (in the form passed
   to WITH-DEPENDENCIES).

   FUNCTION is the expected value function of the meta-node. Any
   symbol in the value function, that is bound to an operand (appears
   in OPERANDS), is replaced with the `NODE-LINK' corresponding to the
   dependency between the operand node and meta-node."

  (flet ((make-input-test (operand)
           `(ok (input-node? ,(car operand))
                (format nil "Argument ~a is an input node" ,(cadr operand)))))
    `(subtest (format nil "Test Meta-Node: ~a" ,g!meta-node)
       (is-type ,g!meta-node 'meta-node
                (format nil "~a is a meta-node" ,g!meta-node))

       (with-slots ((,g!def definition)) ,g!meta-node
         (is-type ,g!def 'node-table "Meta-Node Body Built")

         (is (gethash (node-id "self") (all-nodes ,g!def)) ,g!meta-node
             "Implicit Self Node in ALL-NODES")

         (is (gethash (node-id "self") (nodes ,g!def)) ,g!meta-node
             "Implicit Self Node in NODES")

         (with-nodes ,operands ,g!def
           ,@(mapcar #'make-input-test operands)
           (has-value-function ,(mapcar #'car operands) ,g!meta-node ,function))))))

(deftest meta-nodes
  (subtest "Test Building Meta-Node Definitions"
    (subtest "Simple Functions"
      (with-module-table modules
        (build ":extern(add)"
               "1+(n) : add(n, 1)")
        (finish-build)

        (with-nodes ((add "add") (fn "1+")) modules
          (test-meta-node fn ((n "n")) `(,add ,n 1)))))

    (subtest "Recursive Functions"
      (subtest "Simple Recursion"
        (with-module-table modules
          (build-source-file "./modules/core.trd" modules)

          (build ":import(core)"
                 "fact(n) : {
                    case(
                      n < 2 : 1,
                      n * fact(n - 1)
                    )
                  }")

          (finish-build)

          (with-modules ((core "core")) modules
            (with-nodes ((if "if")) core
              (with-nodes ((- "-") (+ "+") (* "*") (< "<") (fact "fact")) modules
                (test-meta-node fact ((n "n")) `(,if (,< ,n 2) 1 (,* ,n (,fact (,- ,n 1))))))))))

      (subtest "Tail Recursion with Nested Functions"
        (with-module-table modules
          (build-source-file "./modules/core.trd" modules)

          (build ":import(core)")
          (build "fact(n) : {
                    iter(n, acc) :
                        case(n < 2 : acc, iter(n - 1, n * acc))
                    iter(n, 1)
                  }")

          (finish-build)

          (with-modules ((core "core")) modules
            (with-nodes ((if "if")) core
              (with-nodes ((- "-") (+ "+") (* "*") (< "<") (fact "fact")) modules
                (with-nodes ((iter "iter")) (definition fact)
                  (test-meta-node fact ((n "n")) `(,iter ,n 1))
                  (test-meta-node iter ((n "n") (acc "acc"))
                                  `(,if (,< ,n 2) ,acc (,iter (,- ,n 1) (,* ,n ,acc)))))))))))))

(run-test 'meta-nodes)

(finalize)

(cl-interpol:disable-interpol-syntax)
