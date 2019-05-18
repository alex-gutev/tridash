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
  (:use :generic-cl
        :alexandria
        :anaphora
        :cl-arrows
        :iterate
        :optima
        :prove
        :named-readtables

        :tridash.parser
        :tridash.frontend

        :tridash.test.util)

  (:shadowing-import-from :generic-cl
                          :emptyp
                          :multiply
                          :accumulate)

  (:shadowing-import-from :prove :fail)

  (:import-from :lol
                :defmacro!
                :lol-syntax)

  (:import-from :tridash.parser
                :declaration-parse-error)

  (:import-from :tridash.frontend
                :outer-nodes)

  (:export
   :*flat-node-table*
   :build-node
   :ensure-node-table

   :get-node
   :test/get-node
   :test-nodes

   :test-not-node
   :test-not-nodes

   :test-binding
   :test-simple-binding

   :value-fn-equal
   :object-fn-equal

   :test-value-function
   :test-node-function
   :init-context

   :test-error

   :with-module-table
   :with-nodes
   :with-modules
   :with-dependencies
   :has-value-function))


(in-package :tridash.test.builder)

(in-readtable lol-syntax)

(cl-interpol:enable-interpol-syntax)

(plan nil)


(defvar *flat-node-table*)


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

(defgeneric get-node (name node-table)
  (:documentation "Retrieves the node with name NAME from NODE-TABLE.")

  (:method (name (table flat-node-table))
    (or (find name (nodes table) :key #'name)
        (find name (meta-nodes table) :key #'name)))

  (:method (name (modules module-table))
    (get-node name (node-table modules)))

  (:method (name (table node-table))
    (get name (all-nodes table)))

  (:method :around (name table)
           (call-next-method (node-id name) table)))

(defun test/get-node (name node-table)
  "Retrieves the node with name NAME from NODE-TABLE and checks that
   its NAME slot matches NAME."

  (aprog1 (get-node name node-table)
    (subtest (format nil "Test Node: ~a" name)
      (is-type! it 'node "~a is a node" name)
      (isf (name it) (node-id name) "Node name = ~a" name))))

(defun test-nodes (table &rest ids)
  "Tests that each identifier in IDS names a node in MODULES."

  (map (rcurry #'test/get-node table) ids))


(defun test-not-node (table id)
  "Tests that there is no node with identifier ID in MODULES."

  (ok (null (get-node id table))
      (format nil "~a is not a node" id)))

(defun test-not-nodes (node-table &rest ids)
  "Tests that there are no nodes with identifiers IDS in MODULES."

  (foreach (curry #'test-not-node node-table) ids))


;;; Test Bindings

(defun test-binding (src target &key (context nil context-sp))
  "Tests that a binding between node SRC and TARGET has been
   established in the context CONTEXT. If CONTEXT is not supplied the
   context of the `NODE-LINK' between SRC and TARGET is
   checked. Returns the `NODE-LINK' object."

  (let ((link (get target (observers src))))
    (subtest (format nil "Test binding ~a -> ~a" (name src) (name target))
      (is-type! link 'node-link "Node link created")

      (isf link (get src (dependencies target))
	   "Node link added to dependencies of ~a" (name target))

      (isf (node-link-node link) src
	   "Node link points to node ~a" (name src))

      (when context-sp
	(isf (node-link-context link) context
	     "Link context is ~a" context))

      (let* ((id (if context-sp context (node-link-context link)))
	     (context (get id (contexts target))))

	(is-typef context 'node-context
		  "Node context ~a created" id)
	(isf (get src (operands context)) link
	     "~a added to context operands" (name src))))

    link))

(defun test-simple-binding (src target &rest args)
  "Tests that a binding between node SRC and TARGET has been
   established and that the value function of TARGET (in the context
   of the `NODE-LINK' or the context provided as the :CONTEXT
   parameter) is the `NODE-LINK' itself."

  (let ((link (apply #'test-binding src target args)))
    (test-value-function target (node-link-context link) link)))


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

    (((sub-function- (expression expr-a) (count count-a))
      (sub-function- (expression expr-b) (count count-b)))

     (and (value-fn-equal expr-a expr-b)
          (= count-a count-b)))

    (((sub-function- (expression expr-a)) b)
     (value-fn-equal expr-a b))

    ((_ _) (= a b))))

(defun object-fn-equal (a b)
  "Object value function equality comparison. Returns true if the
   object value function A declares the same fields as the object
   value function B."

  (multiple-value-match (values a b)
    (((sub-function- expression) _)
     (object-fn-equal expression b))

    (((list* :object a) (list* :object b))

     (flet ((pair-equal (pair)
              (destructuring-bind (key value) pair
                (aand (find key b :key #'car)
                      (value-fn-equal value (second it))))))
       (every #'pair-equal a)))))


(defun test-value-function (node context fn &key (test #'value-fn-equal))
  "Tests that the context CONTEXT of node NODE has value function FN."

  (subtest (format nil "Test value function of ~a in context ~a" (name node) context)
    (let ((context (get context (contexts node))))
      (is-type! context 'node-context "Node ~a has context ~a" node context)
      (is (value-function context) fn :test test))))

(defun test-node-function (node context fn &rest operands)
  "Tests that node NODE has a context CONTEXT with value function FN
   applied to operands OPERANDS."

  (->>
   (list* fn (map (rcurry #'test-binding node :context context) operands))
   (test-value-function node context)))

(defun init-context (node)
  "Checks whether node has an init context and returns its identifier."

  (flet ((init-context? (context)
           (destructuring-bind (id . context) context
             (and (/= id :input)
                  (emptyp (operands context))))))
    (aprog1
        (car (find-if #'init-context? (contexts node)))
      (ok! it "~a has an :INIT context" node))))


;;; Test Errors

(defun test-error (str &optional (error 'semantic-error))
  "Tests that building the source STR results in a `SEMANTIC-ERROR'."

  (is-error (build-nodes str (make-instance 'module-table)) error
	    (format nil "`~a` raises an error of type `~s'" str error)))

(defun test-top-level-only (decl &rest code)
  "Tests that an error is raised if the declaration DECL appears in a
   non-top-level position. Each element in CODE is
   prepended (separated by ';') to DECL before performing the tests."

  (test-error (format nil "~{~a; ~}~a -> a" code decl) 'special-operator-operand)
  (test-error (format nil "~{~a; ~}a -> ~a" code decl) 'special-operator-operand)
  (test-error (format nil "~{~a; ~}f(x) : x; f(~a)" code decl)) 'special-operator-operand)


;;;; Utility Macros

(defmacro! with-module-table (var &body body)
  "Creates a new `MODULE-TABLE' and binds it to VAR. The binding is
   visible to the forms in BODY.

   Two lexical function definitions are visible to the forms in BODY:
   BUILD and FINISH-BUILD. BUILD builds each source string, passed as
   an argument, into the module-table VAR. FINISH-BUILD calls
   FINISH-BUILD-GRAPH on the module-table VAR."

  `(let* ((,var (make-instance 'module-table))
          (*global-module-table* ,var)
          (*flat-node-table* nil))
     (flet ((build (&rest ,g!strings)
              (foreach (rcurry #'build-nodes ,var) ,g!strings))

            (finish-build (&optional (,g!module-table ,var))
              (setf *flat-node-table* (finish-build-graph ,g!module-table))))
       ,@body)))

(defmacro with-nodes ((&rest nodes) modules &body body)
  "Binds the nodes to variables. Each element of NODES is of the
   form (VAR NAME) where VAR is the variable to which the node is
   bound and NAME (quoted) designates the node's name."

  `(with-nodes% ,(map #`(,(first a1) ',(node-id (second a1))) nodes) ,modules ,@body))

(defmacro! with-nodes% ((&rest nodes) o!modules &body body)
  "Binds the nodes to variables. Each element of NODES is of the
   form (VAR NAME) where VAR is the variable to which the node is
   bound and NAME (evaluated) designates the node's name."

  (flet ((make-binding (node)
           (destructuring-bind (var name) node
             `(,var (test/get-node ,name ,g!node-table)))))
    `(let ((,g!node-table ,g!modules))
       (let ,(map #'make-binding nodes)
         ,@body))))

(defmacro! with-modules ((&rest modules) o!module-table &body body)
  "Binds module `NODE-TABLE's to variables. Each element of MODULES is
   of the form (VAR NAME) where VAR is the variable to which the
   module is bound and NAME designates the module's name."

  (flet ((make-binding (module)
           (destructuring-bind (var name) module
             `(,var (aprog1 (get ',(node-id name) (modules ,g!module-table))
                      (is-type! it 'node-table ,(format nil "~a is a module" name)))))))
    `(let ,(map #'make-binding modules)
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
	     ((list (or (and (type symbol) dep var)
			(list var dep))
		    :context context)

              (list var `(test-binding ,dep ,g!node :context ,context)))

	     ((or (and (type symbol) dep var)
		  (list var dep))

	      (list var `(test-binding ,dep ,g!node))))))

    `(let ,(map #'make-binding deps)
       ,@body)))

(defmacro! has-value-function ((&rest deps) o!node function &rest test-args)
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
       (test-value-function ,g!node ,g!context ,function ,@test-args))))


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
         (test-simple-binding a b :context a)
         (test-simple-binding c b :context c)))

     (with-module-table modules
       (build "a -> (b -> c)")

       (with-nodes ((a "a") (b "b") (c "c") (b->c ("->" "b" "c"))) modules
         (test-simple-binding a b->c :context a)

	 (has-value-function
	  ((b :context b) (b->c :context b))
	  c

	  `(if ,b->c ,b :fail)))))

   (subtest "Functor Nodes"
     (with-module-table modules
       (build-source-file #p"./modules/core.trd" modules)
       (build "a + b -> output; int(x) -> z")

       (with-nodes ((+ "+") (a "a") (b "b")
                    (a+b ("+" "a" "b")) (output "output"))
           modules

         (test-simple-binding a+b output :context a+b)
         (test-node-function a+b + + a b))

       (with-nodes ((int "int") (x "x") (z "z") (int-x ("int" "x"))) modules
         (test-simple-binding int-x z :context int-x)

         (test-node-function int-x int int x)
         (test-node-function x int-x int int-x))))

   (subtest "Special Operators"
     (subtest ":op Operator - Registering Infix Operators"
       (flet ((test-op (id operators prec assoc)
                (diag (format nil "Test infix operator ~a" id))

                (let ((op (get (id-symbol id) operators)))
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
         (test-error ":op()" 'invalid-arguments-error)
         (test-error ":op(*)" 'invalid-arguments-error)
         (test-error ":op(*,*)" 'invalid-arguments-error)
         (test-error ":op(*,9,x)" 'invalid-arguments-error)
         (test-error ":op(\"*\", 10, left)" 'invalid-arguments-error)

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
           (ok (memberp node2 (input-nodes (node-table modules))) "node2 in input-nodes")))

       (subtest "Errors"
         (test-error ":attribute()" 'invalid-arguments-error)
         (test-error "node; :attribute(node, attribute)" 'invalid-arguments-error)
         (test-error "node; :attribute(node, 1, 2)" 'invalid-arguments-error)
         (test-error ":attribute(1, attribute, value)" 'invalid-arguments-error)
         (test-error ":attribute(non-existent-node, public-name, \"node\")" 'non-existent-node)

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
         (test-error "x : y" 'invalid-arguments-error)
         (test-error "{x; y} : z" 'invalid-arguments-error)
         (test-error "{w; x}(y) : z" 'invalid-arguments-error)
         (test-error ":(x)" 'invalid-arguments-error)
         (test-error ":()" 'invalid-arguments-error)
         (test-error ":(x,y,z)" 'invalid-arguments-error)

         (test-top-level-only "(g(x,y) : f(x,y))")

         (diag "Redefining Special Operators")
         (test-error "->(x, y) : fn(x, y)" 'special-operator-name-error)
         (test-error ":(x, y) : z" 'special-operator-name-error)
         (test-error ":extern(x) : x" 'special-operator-name-error)
         (test-error ":op(a, b) : f(b, a)" 'special-operator-name-error)
         (test-error "..(x,z) : g(x, z)" 'special-operator-name-error)
         (test-error ".(a) : h(a)" 'special-operator-name-error)
         (test-error ":attribute(m, n) : f(m,n)" 'special-operator-name-error)
         (test-error ":module(m) : m" 'special-operator-name-error)
         (test-error ":import(x) : x" 'special-operator-name-error)
         (test-error ":use(z) : z" 'special-operator-name-error)
         (test-error ":export(y) : h(y)" 'special-operator-name-error)
         (test-error ":in(x, y) : add(x, y)" 'special-operator-name-error)

         (with-module-table modules
           ;; Test node name collisions with meta-nodes
           (build "a;b")
           (is-error (build #1="a(x,y) : add(x,y)") 'meta-node-name-collision #1#)

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
                 (list* :object (map (curry #'make-field node) fields))
                 (test-value-function node :object <> :test (rcurry #'set-equal :test #'value-fn-equal))))

              (make-field (node field)
                (destructuring-bind (field dep) field
                  (list (id-symbol field) (test-binding dep node :context :object))))

              (test-member-fn (node field dep)
                (->>
                 `(:member ,(test-binding dep node :context dep) ,(id-symbol field))
                 (test-value-function node dep))))

       (with-module-table modules
         (build "x -> a.first; y -> a.last; a.last -> y")

         (with-nodes ((x "x") (y "y") (a "a")
                      (a.first ("." "a" "first")) (a.last ("." "a" "last")))
             modules

           (test-object-fn a (list "first" a.first) (list "last" a.last))
           (test-member-fn a.first "first" a)
           (test-member-fn a.last "last" a)

           (test-simple-binding x a.first :context x)
           (test-simple-binding y a.last :context y)
           (test-simple-binding a.last y :context a.last)))

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
           (test-simple-binding person.first name :context person.first)))

       (with-module-table modules
         (build ":module(mod1)"
                "x -> a.first"

                ":module(mod2)"
                ":use(mod1)"
                "y -> mod1.a.second")

         (with-modules ((mod1 "mod1") (mod2 "mod2")) modules
           (with-nodes ((a "a")
                        (x "x")
                        (a.first ("." "a" "first"))
                        (a.second ("." "a" "second")))
               mod1

             (with-nodes ((y "y")) mod2
               (test-object-fn a (list "first" a.first) (list "second" a.second))
               (test-member-fn a.first "first" a)
               (test-member-fn a.second "second" a)

               (test-simple-binding x a.first :context x)
               (test-simple-binding y a.second :context y)
               ))
           ))))

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
                 (test-simple-binding my-mod.a a :context my-mod.a)
                 (test-node-function n+m + + n m))))

           (subtest "Test :alias operator"
             (build ":module(mod3); :alias(my-mod, m); m.a -> a; m.+(j,k)")

             (with-modules ((mod3 "mod3")) modules
               (with-nodes ((a "a") (j "j") (k "k") (j+k (("." "m" "+") "j" "k"))) mod3
                 (test-simple-binding my-mod.a a :context my-mod.a)
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
           (is-error (build ":module(mod2); +(j,k)") 'non-existent-node)
           (is-error (build ":module(mod2); j + k") 'declaration-parse-error)
           (is-error (build ":module(mod2); my-mod.z") 'non-existent-node)

           (is-error (build ":module(mod3); +(j,k)") 'non-existent-node)
           (is-error (build ":module(mod3); j + k") 'declaration-parse-error)
           (is-error (build ":module(mod3); m.z") 'non-existent-node)

           (is-error (build ":module(mod4); my-mod.+(a,b)") 'non-existent-node)
           (is-error (build ":module(mod4); :in(my-mod, z)") 'non-existent-node)

           (test-error ":use(no-such-module)" 'non-existent-module)
           (test-error ":alias(no-such-module, m)" 'non-existent-module)
           (test-error ":import(no-such-module)" 'non-existent-module)
           (test-error ":import(no-such-module, node)" 'non-existent-module)
           (test-error ":in(no-such-module, x)" 'non-existent-module)
           (test-error ":export(no-such-node)" 'non-existent-node)
           (test-error "x; :export(x, no-such-node)" 'non-existent-node))

         (subtest ":module Operator Syntax"
           (test-error ":module()" 'invalid-arguments-error)
           (test-error ":module(a, b, c)" 'invalid-arguments-error)
           (test-error ":module(1, 2, 3)" 'invalid-arguments-error)
           (test-error ":module(1)" 'invalid-arguments-error)

           (test-top-level-only ":module(m)"))

         (subtest ":use Operator Syntax"
           (test-error ":use(1,2,3)")
           (test-top-level-only ":use(m1)" ":module(m1)" ":module(m2)"))

         (subtest ":alias Operator Syntax"
           (test-error ":alias()" 'invalid-arguments-error)
           (test-error ":alias(m)" 'invalid-arguments-error)
           (test-error ":alias(1)" 'invalid-arguments-error)
           (test-error ":alias(1,2)" 'invalid-arguments-error)
           (test-error ":module(m1); :module(m2); :alias(m1, m, x, y)" 'invalid-arguments-error)

           (test-top-level-only ":alias(mod, m)" ":module(mod)" ":module(m1)"))

         (subtest ":import Operator Syntax"
           (test-error ":import()" 'invalid-arguments-error)
           (test-error ":import(1)" 'invalid-arguments-error)

           (test-top-level-only ":import(mod)" ":module(mod)" ":module(m1)")
           (test-top-level-only ":import(mod, x)" ":module(mod); x" ":module(m1)"))

         (subtest ":export Operator Syntax"
           (test-error ":export(1, 2, 3)")

           (test-top-level-only ":export(x)" "x"))

         (subtest ":in Operator Syntax"
           (test-error ":in()" 'invalid-arguments-error)
           (test-error ":in(x)" 'invalid-arguments-error)
           (test-error ":module(m1); x; :module(m2); :in(m1, x, y)" 'invalid-arguments-error)
           (test-error ":in(1, x)" 'invalid-arguments-error)))))))

(run-test 'builder)


(deftest coalescer
  (subtest "Test Node Coalescer"
    (subtest "Simple Nodes"
      (subtest "One-Way Bindings"
        (with-module-table modules
	  (build "a -> b; b -> c; c -> d"
	         ":attribute(a, input, 1)")

	  (let ((table (finish-build)))
	    (test-not-nodes table "b" "c")

	    (with-nodes ((a "a") (d "d")) table
	      (test-simple-binding a d))))

        (subtest "NO-COALESCE Attribute"
          (with-module-table modules
            (build "a -> b; b -> c; c -> d"
                   ":attribute(a, input, 1)"
                   ":attribute(b, no-coalesce, 1)")

            (let ((table (finish-build)))
              (test-not-nodes table "c")

              (with-nodes ((a "a") (b "b") (d "d")) table
	        (test-simple-binding a b)
	        (test-simple-binding b d))))))

      (subtest "Two-Way Bindings"
        (with-module-table modules
          (build "a -> b; b -> c; c -> d"
                 "d -> c; c -> b; b -> a"
                 ":attribute(a, input, 1)")

          (let ((table (finish-build)))
            (test-not-nodes table "b" "c")

	    (with-nodes ((a "a") (d "d")) table
	      (test-simple-binding a d))))

        (subtest "NO-COALESCE Attribute"
          (with-module-table modules
            (build "a -> b; b -> c; c -> d"
                   "d -> c; c -> b; b -> a"
                   ":attribute(a, input, 1)"
                   ":attribute(b, input, 1)")

            (let ((table (finish-build)))
              (test-not-nodes table "c")

	      (with-nodes ((a "a") (b "b") (d "d")) table
                (test-simple-binding a b)
                (test-simple-binding b a)
	        (test-simple-binding b d))))))

      (subtest "Multiple Observers"
        (with-module-table modules
          (build "a -> b; b -> c; b -> d; c -> e; d -> f"
                 ":attribute(a, input, 1)")

          (let ((table (finish-build)))
            (test-not-nodes table "c" "d")

            (with-nodes ((a "a") (b "b") (e "e") (f "f")) table
              (test-simple-binding a b)
              (test-simple-binding b e)
              (test-simple-binding b f))))))

    (subtest "Functor Nodes"
      (subtest "Simple Functor Nodes"
        (with-module-table modules
          (build ":extern(+); :op(+, 50, left)"
                 "a + b + c + d -> output"
                 ":attribute(a, input, 1)"
                 ":attribute(b, input, 1)"
                 ":attribute(c, input, 1)"
                 ":attribute(d, input, 1)")

          (let ((table (finish-build)))
            (test-not-nodes table
                            '("+" "a" "b")
                            '("+" ("+" "a" "b") "c")
                            '("+" ("+" ("+" "a" "b") "c") "d"))

            (with-nodes ((+ "+") (a "a") (b "b") (c "c") (d "d") (output "output")) table
              (has-value-function (a b c d) output `(,+ (,+ (,+ ,a ,b) ,c) ,d)))))

        (subtest "NO-COALESCE Attribute"
          (with-module-table modules
            (build ":extern(+); :op(+, 50, left)"
                   "a + b + c + d -> output"
                   ":attribute(a, input, 1)"
                   ":attribute(b, input, 1)"
                   ":attribute(c, input, 1)"
                   ":attribute(d, input, 1)"
                   ":attribute(a + b, no-coalesce, 1)")

            (let ((table (finish-build)))
              (test-not-nodes table
                              '("+" ("+" "a" "b") "c")
                              '("+" ("+" ("+" "a" "b") "c") "d"))

              (with-nodes ((+ "+") (a "a") (b "b") (c "c") (d "d")
                           (a+b ("+" "a" "b"))
                           (output "output"))
                  table

                (has-value-function (a b) a+b `(,+ ,a ,b))
                (has-value-function (a+b c d) output `(,+ (,+ ,a+b ,c) ,d)))))))

      (subtest "Multiple Observers"
        (with-module-table modules
          (build ":extern(+); :op(+, 50, left)"
                 "a + b + c + d -> out1"
                 "a + b -> out2"
                 ":attribute(a, input, 1)"
                 ":attribute(b, input, 1)"
                 ":attribute(c, input, 1)"
                 ":attribute(d, input, 1)")

          (let ((table (finish-build)))
            (test-not-nodes table
                            '("+" ("+" "a" "b") "c")
                            '("+" ("+" ("+" "a" "b") "c") "d"))

            (with-nodes ((+ "+") (a "a") (b "b") (c "c") (d "d")
                         (a+b ("+" "a" "b"))
                         (out1 "out1") (out2 "out2"))
                table

              (has-value-function (a+b c d) out1 `(,+ (,+ ,a+b ,c) ,d))

              (test-simple-binding a+b out2)
              (has-value-function (a b) a+b `(,+ ,a ,b)))))

        (with-module-table modules
          (build ":extern(add)"
                 "a -> b; b -> c; b -> d; add(c,d) -> e"
                 ":attribute(a, input, 1)")

          (let ((table (finish-build)))
            (test-not-nodes table "b" "c" "d")

            (with-nodes ((add "add") (a "a") (e "e")) table
              (has-value-function (a) e (list add a a))))))

      (subtest "Object Nodes"
        (with-module-table modules
          (build ":extern(parse, not)"
                 "parse(in1) -> p"

                 "not(p.fail) -> (p.value -> a)"
                 "p.fail -> (in2 -> b)"

                 ":attribute(in1, input, 1)"
                 ":attribute(in2, input, 1)")

          (let ((table (finish-build)))
            (test-not-nodes table
                            '("parse" "in1")
                            '("." "p" "value")
                            '("not" ("." "p" "fail"))
                            '("->" ("." "p" "value") "a")
                            '("->" "in2" "b"))

            (with-nodes ((in1 "in1") (in2 "in2") (p "p")
                         (p.fail ("." "p" "fail"))
                         (a "a") (b "b")

                         (parse "parse") (not "not"))
                table

              (has-value-function
               (p p.fail) a
               `(if (,not ,p.fail) (:member ,p ,(id-symbol "value")) :fail))

              (has-value-function
               (in2 p.fail) b
               `(if ,p.fail ,in2 :fail))

              (has-value-function (in1) p `(,parse ,in1))
              (is (length (contexts p)) 1 "Node p has a single context."))))))

    (subtest "Removing Unreachable Nodes"
      (with-module-table modules
        (build "a -> b; b -> c;"
               "e -> f"                 ; Unreachable Nodes
               ":attribute(a, input, 1)")

        (let ((table (finish-build)))
          (test-not-nodes table "b" "e" "f")

          (with-nodes ((a "a") (c "c")) table
            (test-simple-binding a c))))

      (subtest "NO-REMOVE Attribute"
        (with-module-table modules
          (build "some-special-node"
                 ":attribute(some-special-node, no-remove, 1)")

          (let ((table (finish-build)))
            (test-nodes table "some-special-node"))))

      (subtest "Unreachable Dependency Errors"
        (with-module-table modules
          (build ":extern(add)"
                 "a -> b; add(b, d) -> output"
                 "c -> d"               ; Unreachable Nodes
                 ":attribute(a, input, 1)")

          (is-error (finish-build) 'dependency-not-reachable))))

    (subtest "Cross-Module Bindings"
      (subtest "Simple Bindings"
        (with-module-table modules
          (build ":module(m1); a -> b; b -> c; c -> d"
                 ":attribute(a, input, 1)")

          (build ":module(m2); :use(m1); m1.b -> a; b -> c; c -> d"
                 ":attribute(b, input, 1)")

          (let ((table (finish-build)))
            (with-modules ((m1 "m1") (m2 "m2")) modules
              (test-not-nodes table "c")

              (with-nodes ((m1.a "a") (m1.b "b") (m1.d "d")) m1
                (test-simple-binding m1.a m1.b)
                (test-simple-binding m1.b m1.d)

                (with-nodes ((m2.a "a") (m2.b "b") (m2.d "d")) m2
                  (test-simple-binding m1.b m2.a)
                  (test-simple-binding m2.b m2.d)))))))

      (subtest "Functor Nodes"
        (with-module-table modules
          (build ":module(m1); in -> a; in -> b"
                 ":attribute(in, input, 1)")

          (build ":module(m2); :use(m1);"
                 ":extern(+); :op(+, 50, left)"
                 "m1.a + m1.b + c -> output"
                 ":attribute(c, input, 1)")

          (let ((table (finish-build)))
            (with-modules ((m1 "m1") (m2 "m2")) modules
              (test-not-nodes table
                              "a" "b" '("+" ("+" ("." "m1" "a") ("." "m1" "b")) "c"))

              (with-nodes ((in "in")) m1
                (with-nodes ((+ "+") (c "c") (output "output")) m2
                  (has-value-function (in c) output `(,+ (,+ ,in ,in) ,c)))))))))

    (subtest "Common Sub Expressions"
      (with-module-table modules
        (build ":extern(parse, NaN?)"

               "parse(in) -> p"
               "p -> self.value"
               "NaN?(p) -> self.fail"
               "self -> out"

               ":attribute(in, input, 1)")

        (let ((table (finish-build)))
          (test-not-nodes table
                          '("parse" "in")
                          "p"
                          '("NaN?" "p")
                          "self"
                          '("." "self" "value")
                          '("." "self" "fail"))

          (with-nodes ((in "in") (out "out")
                       (parse "parse") (nan? "NaN?"))
              table

            (has-value-function
             (in) out
             `(:object
               (,(id-symbol "fail")
                 (,nan? ,(sub-function `(,parse ,in) :count 2)))

               (,(id-symbol "value")
                 ,(sub-function `(,parse ,in) :count 2)))
             :test #'object-fn-equal)))))))

(run-test 'coalescer)


(deftest constant-folding
  (subtest "Test Constant Folding"
    (subtest "Literal Constant Values"
      (with-module-table modules
        (build ":extern(add)"
               "add(a, b) -> c"
               "1 -> constant; constant -> b"
               ":attribute(a, input, 1)")

        (let ((table (finish-build)))
          (test-not-nodes table "b" "constant")

          (with-nodes ((add "add") (a "a") (c "c")) table
            (has-value-function (a) c `(,add ,a 1)))))

      (with-module-table modules
        (build "a -> b; b -> c"
               "3 -> constant; constant -> b"
               ":attribute(a, input, 1)")

        (let ((table (finish-build)))
          (test-not-nodes table "constant")

          (with-nodes ((a "a") (b "b") (c "c")) table
            (test-simple-binding a b)
            (test-simple-binding b c)

            (test-value-function b (init-context b) 3))))

      (with-module-table modules
        (build "a -> b; b -> c"
               "\"hello\" -> constant; constant -> a"
               ":attribute(a, input, 1)")

        (let ((table (finish-build)))
          (test-not-nodes table "b" "constant")

          (with-nodes ((a "a") (c "c")) table
            (test-simple-binding a c)

            (test-value-function a (init-context a) "hello")))))

    (subtest "Constant Functor Nodes"
      (with-module-table modules
        (build ":extern(add)"
               "add(a, b) -> c"

               "1 -> c1; 2 -> c2; c2 -> c3"
               "add(c1, c3) -> b"

               ":attribute(a, input, 1)")

        (let ((table (finish-build)))
          (test-not-nodes table "b" "c1" "c2" "c3")

          (with-nodes ((add "add") (a "a") (c "c")) table
            (has-value-function (a) c `(,add ,a (,add 1 2))))))

      (with-module-table modules
        (build ":extern(add)"
               "a -> b; b -> c"

               "5 -> c1; 10 -> c2; c2 -> c3"
               "add(c1, add(c2, c3)) -> b"

               ":attribute(a, input, 1)")

        (let ((table (finish-build)))
          (test-not-nodes table "c1" "c2" "c3")

          (with-nodes ((add "add") (a "a") (b "b") (c "c")) table
            (test-simple-binding a b)
            (test-simple-binding b c)

            (test-value-function b (init-context b) `(,add 5 (,add 10 10))))))

      (with-module-table modules
        (build "a -> b; b -> c"

               "\"hello\" -> constant; constant -> a"
               "constant -> c"

               ":attribute(a, input, 1)")

        (let ((table (finish-build)))
          (test-not-nodes table "b" "constant")

          (with-nodes ((a "a") (c "c")) table
            (test-simple-binding a c)

            (test-value-function a (init-context a) "hello")
            (test-value-function c (init-context c) "hello")))))

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

        (let ((table (finish-build)))
          (test-not-nodes table "constant" "param" "sum" "b")

          (with-nodes ((add "add") (a "a") (c "c")) table
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

        (let ((table (finish-build)))
          (test-not-nodes table "constant" "param" "sum" "b")

          (with-nodes ((add "add") (a "a") (c "c")) table
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


(defmacro! test-meta-node (o!meta-node (&rest operands) function &rest test-args)
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
           `(ok (input-node? ,(first operand))
                (format nil "Argument ~a is an input node" ',(second operand))))

         (get-operand-node (operand)
           (ematch operand
             ((list var (list 'outer node))
              `(,var (cdr (get ,node (outer-nodes ,g!meta-node)))))

             ((list var name)
              `(,var ',(node-id name))))))

    `(subtest (format nil "Test Meta-Node: ~a" ,g!meta-node)
       (is-type ,g!meta-node 'meta-node
                (format nil "~a is a meta-node" ,g!meta-node))

       (with-slots ((,g!def definition)) ,g!meta-node
         (is-type ,g!def 'flat-node-table "Meta-Node Body Built")

         (with-nodes% ,(map #'get-operand-node operands) ,g!def
           ,@(map #'make-input-test operands)
           (has-value-function ,(map #'car operands) ,g!meta-node ,function ,@test-args))))))

(defun make-function-call (fn args outers)
  "Creates a meta-node function call expression where FN is the
   meta-node, ARGS is the list of the `NODE-LINK' objects
   corresponding to the explicit operands of the meta-node and OUTERS
   is the list of `NODE-LINK' objects corresponding to the outer node
   referenced by the meta-node."

  (with-slots (operands outer-nodes) fn
    (labels ((get-arg-pos (arg)
               (match arg
                 ((cons outer-node link)
                  (cons link (find-pos outer-node)))

                 (_
                  (cons arg (find-pos arg)))))

             (find-pos (arg)
               (position (outer-node arg) operands))

             (outer-node (arg)
               (ematch arg
                 ((node-link- node)
                  (outer-node node))

                 ((type node)
                  (cdr (get arg outer-nodes))))))

      (-<> (map #'get-arg-pos outers)
           (sort :key #'cdr)
           (map #'car <>)
           (append args <>)
           (list* fn <>)))))

(deftest meta-nodes
  (subtest "Test Building Meta-Node Definitions"
    (subtest "Simple Functions"
      (subtest "Single Module"
        (with-module-table modules
          (build ":extern(add)"
                 "1+(n) : add(n, 1)")

          (let ((table (finish-build)))
            (with-nodes ((add "add") (fn "1+")) table
              (test-meta-node fn ((n "n")) `(,add ,n 1))
              (test-not-nodes (definition fn) '("add" "n" 1))))))

      (subtest "Multiple Modules"
        (with-module-table modules
          (build ":module(m1)"
                 ":extern(add)"

                 ":module(m2)"
                 ":import(m1, add)"
                 "1+(n) : add(n, 1)")

          (let ((table (finish-build)))
            (with-nodes ((add "add") (fn "1+")) table
              (test-meta-node fn ((n "n")) `(,add ,n 1))
              (test-not-nodes (definition fn) '("add" "n" 1)))))))

    ;; The following tests also test that node-coalescing and constant
    ;; folding work within meta-node definitions.

    (subtest "Recursive Functions"
      (subtest "Simple Recursion"
        (with-module-table modules
          (build-source-file "./modules/core.trd" modules)

          (build ":import(core)"

                 "fact(n) : {
                    # Additional nodes to test node coalescing
                    n -> m
                    m -> k

                    m - 1 -> next

                    # Additional nodes to test constant folding
                    two -> limit;
                    2 -> two

                    case(
                      k < limit : 1,
                      k * fact(next)
                    )
                  }")

          (let ((table (finish-build)))
            (with-nodes ((if "if") (- "-") (* "*") (< "<") (fact "fact")) table
              (test-meta-node fact ((n "n")) `(,if (,< ,n 2) 1 (,* ,n (,fact (,- ,n 1)))))
              (test-not-nodes (definition fact)
                              "m" "k" '("-" "m" 1) "next"
                              "two" "limit"
                              '#1=("<" "k" "limit")
                              '#2=("fact" "next")
                              '#3=("*" "k" #2#)
                              '("case"
                                (":" #1# 1)
                                #3#))))))

      (subtest "Tail Recursion with Nested Functions"
        (with-module-table modules
          (build-source-file "./modules/core.trd" modules)

          (build ":import(core)"

                 "fact(n) : {
                    n -> m

                    iter(n, acc) : {
                      # Additional nodes to test node coalescing
                      n -> m
                      m -> k

                      m - 1 -> next

                      # Additional nodes to test constant folding
                      two -> limit
                      2 -> two

                      case(
                        k < limit : acc,
                        iter(next, k * acc)
                      )
                    }

                    iter(m, 1)
                  }")

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
                                '#10=("<" "k" "limit")
                                '#11=("*" "k" "acc")
                                '#12=("iter" "next" #11#)
                                '("case"
                                  (":" #10# "acc")
                                  #12#)))))))

      (subtest "Mutually Recursive Functions"
        (subtest "Single Module"
          (with-module-table modules
            (build-source-file "./modules/core.trd" modules)

            (build ":import(core)"
                   "fib(n) :
                      case(
                        n > 1 : fib1(n) + fib2(n),
                        1
                      )"
                   "fib1(n) : fib(n - 1)"
                   "fib2(n) : fib(n - 2)"
                   "fib(in) -> out"
                   ":attribute(in, input, 1)")

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
                                '#20=(">" "n" "1")
                                '#21=("fib1" "n")
                                '#22=("fib2" "n")
                                '#23=("+" #21# #22#)
                                '("case" (":" #20# #23#) 1))

                (test-not-nodes (definition fib1)
                                '#30=("-" "n" "1")
                                '#31=("fib" #30#))

                (test-not-nodes (definition fib2)
                                '#40=("-" "n" "2")
                                '#41=("fib" #40#))

                (has-value-function (in) out `(,fib ,in))))))

        (subtest "Multiple Modules"
          (with-module-table modules
            (build-source-file "./modules/core.trd" modules)

            (build ":module(m1)"
                   ":import(core)"
                   "fib1(n) : { :use(m2); m2.fib(n - 1) }"
                   "fib2(n) : { :use(m2); m2.fib(n - 2) }"

                   ":module(m2)"
                   ":import(core)"
                   ":import(m1, fib1)"
                   ":use(m1)"

                   "fib(n) :
                      case(
                        n > 1 : fib1(n) + m1.fib2(n),
                        1
                      )"

                   "fib(in) -> out"
                   ":attribute(in, input, 1)")

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
                                '#50=(">" "n" "1")
                                '#51=("fib1" "n")
                                '#52=(("." "m1" "fib2") "n")
                                '#53=("+" #51# #52#)
                                '("case" (":" #50# #53#) 1))

                (test-not-nodes (definition fib1)
                                '#60=("-" "n" "1")
                                '#61=(("." "m2" "fib") #60#))

                (test-not-nodes (definition fib2)
                                '#70=("-" "n" "2")
                                '#71=(("." "m2" "fib") #70#))

                (has-value-function (in) out `(,fib ,in))))))))

    (subtest "Self-Node References"
      (with-module-table modules
        (build "Person(name, surname) : {
                  name -> self.first
                  surname -> self.last
                }")

        (let ((table (finish-build)))
          (with-nodes ((person "Person")) table
            (test-meta-node person ((name "name") (surname "surname"))
                            `(:object (,(id-symbol "first") ,name)
                                      (,(id-symbol "last") ,surname))
                            :test #'object-fn-equal)))))

    (subtest "Outer-Node References"
      (subtest "Simple Outer-Node References"
        (subtest "Single Module"
          (with-module-table modules
            (build-source-file "./modules/core.trd" modules)

            (build ":import(core)"

                   "addx(a) : addy(..(x) + a)"
                   "addy(a) : a + ..(y)"

                   "x; y"

                   "addx(in1) -> out1"
                   "addy(in2) -> out2"

                   ":attribute(x, input, 1)"
                   ":attribute(y, input, 1)"
                   ":attribute(in1, input, 1)"
                   ":attribute(in2, input, 1)")

            (let ((table (finish-build)))

              (with-nodes ((+ "+")
                           (addx "addx") (addy "addy")
                           (x "x") (y "y")
                           (in1 "in1") (in2 "in2") (out1 "out1") (out2 "out2"))
                  table

                (test-meta-node addy ((a "a") (y (outer y))) `(,+ ,a ,y))
                (test-meta-node addx ((a "a") (x (outer x)) (y (outer y)))
                                `(,addy (,+ ,x ,a) ,y))

                (has-value-function (in2 y) out2 `(,addy ,in2 ,y))
                (has-value-function (in1 x y) out1 (make-function-call addx (list in1) (list x y)))))))

        (subtest "Multiple Modules"
          (with-module-table modules
            (build-source-file "./modules/core.trd" modules)

            (build ":module(m1)"
                   ":import(core)"

                   "addx(a) : { :import(m2); addy(x + a) }"

                   "y"
                   ":attribute(y, input, 1)"

                   ":module(m2)"
                   ":import(core)"
                   ":use(m1)"

                   "addy(a) : a + m1.y"

                   "x"
                   ":export(x)"
                   ":export(addy)"

                   "m1.addx(in1) -> out1"
                   "addy(in2) -> out2"

                   ":attribute(x, input, 1)"
                   ":attribute(in1, input, 1)"
                   ":attribute(in2, input, 1)")

            (let ((table (finish-build)))
              (with-nodes ((+ "+") (addx "addx") (y "y")
                           (addy "addy") (x "x")
                           (in1 "in1") (in2 "in2")
                           (out1 "out1") (out2 "out2"))
                  table

                (test-meta-node addy ((a "a") (y (outer y))) `(,+ ,a ,y))
                (test-meta-node addx ((a "a") (x (outer x)) (y (outer y)))
                                `(,addy (,+ ,x ,a) ,y))

                (has-value-function (in2 y) out2 `(,addy ,in2 ,y))
                (has-value-function (in1 x y) out1 (make-function-call addx (list in1) (list x y))))))

          (subtest "Test subnodes of outer nodes."
           (with-module-table modules
             (build-source-file "./modules/core.trd" modules)

             (build ":module(m1)"
                    ":import(core)"

                    "addx(a) : { :use(m2); a + m2.dict.x }"

                    ":module(m2)"
                    ":import(core)"
                    ":use(m1)"

                    "m1.addx(in2) -> out1"
                    "dict.x -> out2"

                    ":attribute(in2, input, 1)"

                    ":module(m3)"
                    ":import(m2, dict)"

                    "in1 -> dict.x"
                    ":attribute(in1, input, 1)")

             (let ((table (finish-build)))
               (with-nodes ((+ "+") (addx "addx")
                            (in1 "in1") (in2 "in2")
                            (dict "dict") (dict.x ("." "dict" "x"))
                            (out1 "out1"))
                   table

                 (test-meta-node addx ((a "a") (x (outer dict.x))) `(,+ ,a ,x))

                 (has-value-function (in2 dict.x) out1 `(,addx ,in2 ,dict.x))
                 (has-value-function (dict.x) dict `(:object (,(id-symbol "x") ,dict.x)))
                 (test-simple-binding in1 dict.x)))))))

      (subtest "Outer-Node References from Mutually Recursive Functions"
        (subtest "Single Module"
          (with-module-table modules
            (build-source-file "./modules/core.trd" modules)

            (build ":import(core)"

                   "addx(a) : addy(..(x) + a)"
                   "addy(a) : addx(a + ..(y))"

                   "x; y"

                   "addx(in1) -> out1"
                   "addy(in2) -> out2"

                   ":attribute(x, input, 1)"
                   ":attribute(y, input, 1)"
                   ":attribute(in1, input, 1)"
                   ":attribute(in2, input, 1)")

            (let ((table (finish-build)))
              (with-nodes ((+ "+")
                           (addx "addx") (addy "addy")
                           (x "x") (y "y")
                           (in1 "in1") (in2 "in2") (out1 "out1") (out2 "out2"))
                  table

                (test-meta-node addy ((a "a") (in-x (outer x)) (in-y (outer y)))
                                (make-function-call addx `((,+ ,a ,in-y)) `((,x . ,in-x) (,y . ,in-y))))
                (test-meta-node addx ((a "a") (in-x (outer x)) (in-y (outer y)))
                                (make-function-call addy `((,+ ,in-x ,a)) `((,x . ,in-x) (,y . ,in-y))))

                (has-value-function (in1 x y) out1 (make-function-call addx (list in1) (list x y)))
                (has-value-function (in2 x y) out2 (make-function-call addy (list in2) (list x y)))))))

        (subtest "Multiple Modules"
          (with-module-table modules
            (build-source-file "./modules/core.trd" modules)

            (build ":module(m1)"
                   ":import(core)"

                   "addx(a) : { :import(m2); addy(x + a) }"

                   "y"
                   ":attribute(y, input, 1)"

                   ":module(m2)"
                   ":import(core)"
                   ":use(m1)"

                   "addy(a) : m1.addx(a + m1.y)"

                   "x"
                   ":export(x)"
                   ":export(addy)"

                   "m1.addx(in1) -> out1"
                   "addy(in2) -> out2"

                   ":attribute(x, input, 1)"
                   ":attribute(in1, input, 1)"
                   ":attribute(in2, input, 1)")

            (let ((table (finish-build)))
              (with-nodes ((+ "+") (addx "addx") (y "y")
                           (addy "addy") (x "x")
                           (in1 "in1") (in2 "in2")
                           (out1 "out1") (out2 "out2"))
                table

                (test-meta-node addy ((a "a") (in-x (outer x)) (in-y (outer y)))
                                (make-function-call addx `((,+ ,a ,in-y)) `((,x . ,in-x) (,y . ,in-y))))
                (test-meta-node addx ((a "a") (in-x (outer x)) (in-y (outer y)))
                                (make-function-call addy `((,+ ,in-x ,a)) `((,x . ,in-x) (,y . ,in-y))))

                (has-value-function (in1 x y) out1 (make-function-call addx (list in1) (list x y)))
                (has-value-function (in2 x y) out2 (make-function-call addy (list in2) (list x y))))))))

      (subtest "Outer-Node References from Nested Functions"
        (subtest "Single Module"
          (with-module-table modules
            (build-source-file "./modules/core.trd" modules)

            (build ":import(core)"

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
                   ":attribute(start, input, 1)"

                   "count(in) -> out"
                   ":attribute(in, input, 1)")

            (let ((table (finish-build)))
              (with-nodes ((+ "+") (< "<") (if "if")
                           (count "count")
                           (start "start")
                           (in "in") (out "out"))
                  table

                (with-nodes ((iter "iter") (end "end")) (definition count)
                  (test-meta-node count ((end "end") (in-start (outer start)))
                                  (make-function-call iter '(0 0) `((,start . ,in-start) ,end)))

                  (test-meta-node
                   iter
                   ((n "n") (acc "acc") (in-start (outer start)) (in-end (outer end)))
                   `(,if (,< ,n ,in-end)
                         ,(make-function-call iter `((,+ ,n 1) (,+ ,acc ,n)) `((,start . ,in-start) (,end . ,in-end)))
                         (,+ ,in-start ,acc))))

                (has-value-function (in start) out (make-function-call count (list in) (list start)))))))

        (subtest "Multiple Modules"
          (with-module-table modules
            (build-source-file "./modules/core.trd" modules)

            (build ":module(m1)"
                   "start; :export(start)"
                   ":attribute(start, input, 1)"

                   ":module(m2)"
                   ":import(core)"
                   ":import(m1)"

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
                   ":attribute(in, input, 1)")

            (let ((table (finish-build)))
              (with-nodes ((start "start")
                           (+ "+") (< "<") (if "if")
                           (count "count")
                           (in "in") (out "out"))
                table

                (with-nodes ((iter "iter") (end "end")) (definition count)
                  (test-meta-node count ((end "end") (in-start (outer start)))
                                  (make-function-call iter '(0 0) `((,start . ,in-start) ,end)))

                  (test-meta-node
                   iter
                   ((n "n") (acc "acc") (in-start (outer start)) (in-end (outer end)))
                   `(,if (,< ,n ,in-end)
                         ,(make-function-call iter `((,+ ,n 1) (,+ ,acc ,n)) `((,start . ,in-start) (,end . ,in-end)))
                         (,+ ,in-start ,acc))))

                (has-value-function (in start) out (make-function-call count (list in) (list start)))))))))

    (subtest "Structure Checking"
      (subtest "Cycle Checks"
        (subtest "Simple Bindings"
          (with-module-table modules
            (build "f(a) : {
                      a -> b
                      b -> c
                      c -> d
                      c -> self.out2
                      d -> self.out1
                      d -> b
                    }")

            (is-error (finish-build) 'node-cycle-error)))

        (subtest "Functional Bindings"
          (with-module-table modules
            (build ":extern(add)"

                   "f(a, b) : {
                      add(a, b) -> c
                      c -> self.out1
                      c -> d
                      d -> self.out2
                      d -> a
                    }")

            (is-error (finish-build) 'node-cycle-error))))

      (subtest "Ambiguous Context Checks"
        (subtest "Simple Bindings"
          (with-module-table modules
            (build "f(a) : {
                      a -> d
                      a -> b
                      b -> c
                      c -> d
                      d -> e
                      e
                    }")

            (is-error (finish-build) 'ambiguous-context-error))

          (with-module-table modules
            (build "f(a, b) : {
                      a -> self
                      b -> self
                    }")

            (is-error (finish-build) 'ambiguous-context-error)))

        (subtest "Functional Bindings"
          (with-module-table modules
            (build ":extern(add)"

                   "f(a, b) : {
                      add(a, b) -> d
                      a -> c; c -> d
                      d
                    }")

            (is-error (finish-build) 'ambiguous-context-error))

          (with-module-table modules
            (build ":extern(add)"

                   "f(a, b) : {
                      a -> self
                      b -> self
                      add(a, b) -> self
                    }")

            (is-error (finish-build) 'ambiguous-context-error)))))))

(run-test 'meta-nodes)


(finalize)

(cl-interpol:disable-interpol-syntax)
