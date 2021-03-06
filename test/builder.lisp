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

  (:import-from :tridash.util
                :with-struct-slots)

  (:import-from :tridash.frontend
                :*node-true*
                :*node-false*
                :outer-nodes
                :change-module)

  (:export
   :*flat-node-table*
   :build-node
   :build-core-module
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
   :build
   :finish-build
   :with-nodes
   :with-modules
   :with-dependencies
   :has-value-function))

(in-package :tridash.test.builder)

(in-readtable lol-syntax)

(cl-interpol:enable-interpol-syntax)

(plan nil)


(defvar *flat-node-table*)


;;;; Prevent undefined function warnings

(declaim (ftype function test-value-function))


;;;; Building From Strings

(defun build-nodes (string modules)
  "Builds the nodes parsed from STRING into the `MODULE-TABLE'
   MODULES."

  (with-input-from-string (in string)
    (build-parsed-nodes (make-parser in) modules)))

(defun build-core-module (&optional (modules *global-module-table*))
  "Builds the `core` module into the `MODULE-TABLE' MODULES."

  (build-source-file #p"./modules/core/operators.trd" modules)
  (build-source-file #p"./modules/core/external.trd" modules)
  (build-source-file #p"./modules/core/primitives.trd" modules)
  (build-source-file #p"./modules/core/failures.trd" modules)
  (build-source-file #p"./modules/core/lists.trd" modules)
  (build-source-file #p"./modules/core/introspection.trd" modules)
  (build-source-file #p"./modules/core/types.trd" modules)
  (build-source-file #p"./modules/core/patterns.trd" modules)
  (build-source-file #p"./modules/core/macros.trd" modules)
  (build-source-file #p"./modules/core/strings.trd" modules)
  (build-source-file #p"./modules/core/js-backend.trd" modules)
  (build-source-file #p"./modules/core/wasm32-backend.trd" modules))


;;;; Getting a Node-Table

(defun ensure-node-table (thing)
  "If THING is a `MODULE-TABLE' return the node table bound to the
   CURRENT-MODULE slot. Otherwise if THING is a `MODULE' return it."

  (etypecase thing
    (module-table (current-module thing))
    (module thing)))


;;;; Test for the existence of nodes

(defgeneric get-node (name node-table)
  (:documentation "Retrieves the node with name NAME from NODE-TABLE.")

  (:method (name (table flat-node-table))
    (or (find name (nodes table) :key #'name)
        (find name (meta-nodes table) :key #'name)))

  (:method (name (modules module-table))
    (some (curry #'get-node name) (map-values (modules modules))))

  (:method (name (table module))
    (get name (nodes table)))

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


;;;; Test Bindings

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


;;;; Test Value Functions

(defvar *strict-test* t
  "If set to true the expression comparison is strict meaning the
   expected expression has to match the actual expression exactly,
   with the exception that the actual expression may have
   `EXPRESSION-BLOCK's not specified in the expected expression. If
   set to NIL certain rules will be relaxed, such as expressions may
   be contained inside `NODE-LINK' objects.")

(defgeneric value-fn-equal (a b))

(defmethod value-fn-equal ((a node-link) (b node-link))
  (eq a b))

(defmethod value-fn-equal ((a node-link) b)
  (unless *strict-test*
    (value-fn-equal (node-link-node a) b)))

(defmethod value-fn-equal ((a node-ref) (b node-ref))
  (eq (node-ref-node a)
      (node-ref-node b)))

(defmethod value-fn-equal ((got functor-expression) (exp list))
  (flet ((split-args (args)
           (aif (position :outer args)
                (values (subseq args 0 it) (subseq args (1+ it)))
                (values args nil))))

    (match* (got exp)
      (((functor-expression- (meta-node got-op) (arguments got-args) (outer-nodes got-outer-nodes))
        (list* exp-op exp-args))

       (multiple-value-bind (exp-args exp-outer-nodes) (split-args exp-args)
         (and (value-fn-equal got-op exp-op)

              (= (length got-args) (length exp-args))
              (every #'value-fn-equal got-args exp-args)

              (= (length got-outer-nodes) (length exp-outer-nodes))
              (every
               (lambda (outer-node)
                 (value-fn-equal (get (car outer-node) got-outer-nodes)
                                 (cdr outer-node)))
               exp-outer-nodes)))))))

(defmethod value-fn-equal ((a object-expression) (b list))
  (flet ((field= (a b)
           (and (= (first a) (first b))
                (value-fn-equal (second a) (second b)))))

    (match* (a b)
      (((object-expression- (entries entries-a))
        (list* :object entries-b))

       (and (= (length entries-a) (length entries-b))
            (every #'field=
                   (sort entries-a :key (compose #'symbol-name #'first))
                   (sort entries-b :key (compose #'symbol-name #'first))))))))

(defvar *parent-expression-blocks* nil)

(defmethod value-fn-equal ((a expression-block) (b expression-block))
  (let ((*parent-expression-blocks* (acons a b *parent-expression-blocks*)))
    (match* (a b)
      (((expression-block- (expression expr-a) (count count-a))
        (expression-block- (expression expr-b) (count count-b)))

       (and (value-fn-equal expr-a expr-b)
            (= count-a count-b))))))

(defmethod value-fn-equal ((a expression-block) b)
  (value-fn-equal (expression-block-expression a) b))

(defmethod value-fn-equal ((got cyclic-reference) (exp cyclic-reference))
  (=
   (cdr (assoc (cyclic-reference-expression got) *parent-expression-blocks*))
   (cyclic-reference-expression exp)))

(defmethod value-fn-equal ((got meta-node-ref) (exp meta-node-ref))
  (with-struct-slots meta-node-ref-
      ((got-node node)
       (got-optional optional)
       (got-outer-nodes outer-nodes))
      got

    (with-struct-slots meta-node-ref-
        ((exp-node node)
         (exp-optional optional)
         (exp-outer-nodes outer-nodes))
        exp

      (and (= got-node exp-node)

           (= (length got-optional) (length exp-optional))
           (every #'value-fn-equal got-optional exp-optional)

           (= (length got-outer-nodes) (length exp-outer-nodes))
           (every
            (lambda (outer-node)
              (value-fn-equal (get (car outer-node) got-outer-nodes)
                              (cdr outer-node)))
            exp-outer-nodes)))))

(defmethod value-fn-equal ((a argument-list) (b argument-list))
  (with-struct-slots argument-list- ((args-a arguments))
      a
    (with-struct-slots argument-list- ((args-b arguments))
        b

      (and (= (length args-a) (length args-b))
           (every #'value-fn-equal args-a args-b)))))

(defmethod value-fn-equal ((a external-meta-node) b)
  (match* ((name a) b)
    (((eql (id-symbol "if")) 'if)
     t)

    (((eql (id-symbol "member")) :member)
     t)

    (((eql (id-symbol "fail")) :fail)
     t)

    (((eql (id-symbol "catch")) :catch)
     t)

    ((_ _) (call-next-method))))

(defmethod value-fn-equal (a b)
  (= a b))

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
              (change-module :init *global-module-table*)
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
  "Binds module `MODULE's to variables. Each element of MODULES is
   of the form (VAR NAME) where VAR is the variable to which the
   module is bound and NAME designates the module's name."

  (flet ((make-binding (module)
           (destructuring-bind (var name) module
             `(,var (aprog1 (get ',(node-id name) (modules ,g!module-table))
                      (is-type! it 'module ,(format nil "~a is a module" name)))))))
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
       (declare (ignorable ,@(map #'ensure-car deps)))
       ,@body)))

(defmacro! has-value-function ((&rest deps) o!node &body (function &rest test-args))
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

(defmacro! test-meta-node (o!meta-node (&rest operands) &body (function &rest test-args))
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
              `(,var (name (get ,node (outer-nodes ,g!meta-node)))))

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

;;; Test Errors

(defmacro! test-error (error &rest code)
  "Tests that building the source code results raises the error
   ERROR (not evaluated). The `core` module is built prior to building
   CODE."

  `(with-module-table ,g!modules
     (build-core-module)
     (is-error (build ,@code)
               ,error
	       (format nil "`~a` raises an error of type `~s'" (list ,@code) ',error))))

(defun test-top-level-only (decl &rest code)
  "Tests that an error is raised if the declaration DECL appears in a
   non-top-level position. Each element in CODE is
   prepended (separated by ';') to DECL before performing the tests."

  (test-error special-operator-reference-error (format nil "~{~a; ~}~a -> a" code decl))
  (test-error special-operator-reference-error (format nil "~{~a; ~}a -> ~a" code decl))
  (test-error special-operator-reference-error (format nil "~{~a; ~}f(x) : x; f(~a)" code decl)))


;;;; Tests

(subtest "Node Builder"
  (subtest "Simple Atom Nodes"
    (with-module-table modules
      (build "a;b;cc")

      (test-nodes modules "a" "b" "cc")))

  (subtest "Bindings"
    (with-module-table modules
      (build "a -> b; cc -> b")

      (with-nodes ((a "a") (b "b") (cc "cc")) modules
        (test-simple-binding a b :context a)
        (test-simple-binding cc b :context cc)))

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

  (subtest "Functor Nodes"
    (subtest "Meta-Node Operators"
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

    (subtest "Node Operators"
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

    (subtest "Non-Node Operators"
      (test-error non-node-operator-error "1(x, y)")
      (test-error non-node-operator-error " \"hello\"(1, x, 2) ")
      (test-error non-existent-node-error "z(x, y)")))

  (subtest "Functors in Target Position"
    (subtest "No Target Node"
      (test-error target-node-error "/external(add, x, y); a -> add(b, cc)"))

    (subtest "With Target Node"
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

    (subtest "With Target Node Cross Module"
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

    (subtest "With Target Node and Functor Arguments"
      (subtest "In Source Position"
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

            (is (length (contexts f-a)) 1)
            (is (length (contexts b)) 0))))

      (subtest "In Target Position"
        (test-error target-node-error
                    "/external(add, x, y)"
                    "/external(reverse-add, sum)"
                    "/external(f, x)"

                    "/attribute(add, target-node, reverse-add)"
                    "input -> add(f(a), b)"))))

  (subtest "Explicit Contexts"
    (subtest "In Target of Binding"
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

    (subtest "In Target of Literal Binding"
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

    (subtest "With Test Function"
      (subtest "Node Source"
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

      (subtest "Constant Source"
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
                  :none)))))))

    (subtest "In Source of Binding"
      ;; Test that the /context operator has no effect in source
      ;; position.

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

    (subtest "Subnodes"
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

    (subtest "Subnode Targets"
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

            `(:object (,(id-symbol "field") ,a.field)))))))

  (subtest "Special Operators"
    (subtest "/operator Operator - Registering Infix Operators"
      (flet ((test-op (id operators prec assoc)
               (diag (format nil "Test infix operator ~a" id))

               (let ((op (get (id-symbol id) operators)))
                 (ok op "Operator registered")
                 (is (length op) 2 "Operator details added")
                 (is (first op) prec "Operator precedence")
                 (is (second op) assoc "Operator associativity"))))

        (with-module-table modules
          (build "/operator(+, 50, left); /operator(-, 70); /operator(*, 100, right)")

          (with-slots (operator-nodes) (current-module modules)

            (test-op "+" operator-nodes 50 :left)
            (test-op "-" operator-nodes 70 :right)
            (test-op "*" operator-nodes 100 :right))))

      (subtest "Errors"
        (test-error invalid-arguments-error "/operator()")
        (test-error invalid-arguments-error "/operator(*)")
        (test-error invalid-arguments-error "/operator(*,*)")
        (test-error invalid-arguments-error "/operator(*,9,x)")
        (test-error invalid-arguments-error "/operator(\"*\", 10, left)")

        (test-top-level-only "/operator(*, 10, left)")))

    (subtest "/attribute Operator - Node Attributes"
      (with-module-table modules
        ;; Build Nodes
        (build "add(a, b) : +(a, b); node1; node2")

        ;; Build Attribute Declarations
        (build "/attribute(node1, coalescable, 0)")
        (build "/attribute(add, \"public-name\", \"sum\")")
        (build "/attribute(node2, input, 1)")

        (with-nodes ((add "add") (node1 "node1") (node2 "node2")) modules
          (is (attribute :coalescable node1) 0)
          (is (attribute :public-name add) "sum")
          (is (attribute :input node2) 1)

          (ok (input-node? node2) "(INPUT-NODE? node2)")
          (ok (memberp node2 (input-nodes (current-module modules))) "node2 in input-nodes")))

      (subtest "Errors"
        (test-error invalid-arguments-error "/attribute()")
        (test-error invalid-arguments-error "node; /attribute(node, attribute)")
        (test-error invalid-arguments-error "node; /attribute(node, 1, 2)")
        (test-error invalid-arguments-error "/attribute(1, attribute, value)")

        (test-top-level-only "/attribute(node, input, 1)" "node"))))

  (subtest "Meta-Nodes"
    (subtest "Meta-Node Definitions"
      (subtest "Simple Definitions"
        (with-module-table modules
          (build-core-module)
          (build "/import(core); add(x,y) : x + y; add(a,b)")

          (with-nodes ((add "add") (a "a") (b "b") (add-ab ("add" "a" "b"))) modules
            (is-type! add 'meta-node)
            (is (operands add) (decls '!\x '!\y))
            (is (definition add) (decls '(!+ !\x !\y)) :test #'decl=)

            (test-node-function add-ab add add a b))))

      (subtest "Optional and Rest Arguments"
        (with-module-table modules
          (build-core-module)
          (build "/import(core)"
                 "f(a, b : 1, c : x, :(d), ..(rest)) : a + b + c + d + rest")

          (with-nodes ((f "f") (x "x")) modules
            (is-type! f 'meta-node)
            (is (operands f)
                (decls '!\a
                       '(!\: !\b 1)
                       (list '!\: '!\c x)
                       '(!\: !\d :none)
                       '(!.. !|rest|))))))

      (subtest "Errors"
        (subtest "Syntax"
          (test-error invalid-arguments-error "x : y")
          (test-error semantic-error "{x; y} : z")
          (test-error invalid-arguments-error "{w; x}(y) : z")
          (test-error invalid-arguments-error ":(x)")
          (test-error invalid-arguments-error ":()")
          (test-error invalid-arguments-error ":(x,y,z)")

          (test-top-level-only "(g(x,y) : f(x,y))"))

        (subtest "Invalid Operand List"
          (test-error invalid-operand-list-error "f(x, g(h)) : x")
          (test-error invalid-operand-list-error "f(1, x, 3) : x")

          (test-error invalid-operand-list-error "f(x, y : 3, z) : x")
          (test-error invalid-operand-list-error "f(x, ..(y), ..(z)) : x")
          (test-error invalid-operand-list-error "f(x, ..(y), z) : x")
          (test-error invalid-operand-list-error "f(x, ..(y), z : 3) : x")

          (test-error invalid-operand-list-error "f(x, :(g(y), 3)) : x")
          (test-error invalid-operand-list-error "f(x, ..(g(y))) : x"))

        (subtest "Redefining Special Operators"
          (test-error redefine-special-operator-error "/context(x) : x")
          (test-error redefine-special-operator-error "/external(x) : x")
          (test-error redefine-special-operator-error "/operator(a, b) : f(b, a)")
          (test-error redefine-special-operator-error "/attribute(m, n) : f(m,n)")
          (test-error redefine-special-operator-error "/module(m) : m")
          (test-error redefine-special-operator-error "/import(x) : x")
          (test-error redefine-special-operator-error "/use(z) : z")
          (test-error redefine-special-operator-error "/export(y) : h(y)")
          (test-error redefine-special-operator-error "/in(x, y) : add(x, y)"))

        (subtest "Name Collisions"
          (with-module-table modules
            (build "a;b")
            (is-error (build #1="a(x,y) : add(x,y)") 'node-exists-error #1#))

          (with-module-table modules
            (build "f(x) : x")
            (is-error (build "f(x,y) : +(x, y)") 'node-exists-error "f(x,y) : +(x, y)")))))

    (subtest "External Meta-Node Definitions"
      (subtest "Simple"
        (with-module-table modules
          (build "/external(add, x, y); /external(sub, x, y); add(a,b); sub(a,b)")

          (with-nodes ((add "add") (sub "sub")
                       (a "a") (b "b")
                       (add-ab ("add" "a" "b")) (sub-ab ("sub" "a" "b")))
              modules

            (is-type! add 'external-meta-node)
            (is-type! sub 'external-meta-node)

            (is (operands add) (decls '!\x '!\y))
            (is (operands sub) (decls '!\x '!\y))

            (test-node-function add-ab add add a b)
            (test-node-function sub-ab sub sub a b))))

      (subtest "Optional and Rest Arguments"
        (with-module-table modules
          (build-core-module)
          (build "/import(core)"
                 "/external(f, a, b : 1, c : x, :(d), ..(rest))")

          (with-nodes ((f "f") (x "x")) modules
            (is-type! f 'external-meta-node)
            (is (operands f)
                (decls '!\a
                       '(!\: !\b 1)
                       (list '!\: '!\c x)
                       '(!\: !\d :none)
                       '(!.. !|rest|))))))

      (subtest "Errors"
        (subtest "Syntax"
          (test-top-level-only "/external(y)")

          (test-error invalid-arguments-error "/external(g(h))")
          (test-error semantic-error "/external({x; y})")
          (test-error invalid-arguments-error "/external({w; x}(y) : z)")
          (test-error invalid-arguments-error "/external()"))

        (subtest "Invalid Operand List"
          (test-error invalid-operand-list-error "/external(f, x, g(h))")
          (test-error invalid-operand-list-error "/external(f, 1, x, 3) : x")

          (test-error invalid-operand-list-error "/external(f, x, y : 3, z)")
          (test-error invalid-operand-list-error "/external(f, x, ..(y), ..(z))")
          (test-error invalid-operand-list-error "/external(f, x, ..(y), z)")
          (test-error invalid-operand-list-error "/external(f, x, ..(y), z : 3)")

          (test-error invalid-operand-list-error "/external(f, x, :(g(y), 3))")
          (test-error invalid-operand-list-error "/external(f, x, ..(g(y)))"))

        (subtest "Redefining Special Operators"
          (test-error redefine-special-operator-error "/external(/context)")
          (test-error redefine-special-operator-error "/external(/external)")
          (test-error redefine-special-operator-error "/external(/operator)")
          (test-error redefine-special-operator-error "/external(/attribute)")
          (test-error redefine-special-operator-error "/external(/module)")
          (test-error redefine-special-operator-error "/external(/import)")
          (test-error redefine-special-operator-error "/external(/use)")
          (test-error redefine-special-operator-error "/external(/export)")
          (test-error redefine-special-operator-error "/external(/in)"))

        (subtest "Name Collisions"
          (with-module-table modules
            (build "a;b")
            (is-error (build "/external(a)") 'node-exists-error "/external(a)"))

          (with-module-table modules
            (build "f(x) : x")
            (is-error (build "/external(f)") 'node-exists-error "/external(f)"))))))

  (subtest "Arity Checking"
    (subtest "Meta-Nodes"
      (subtest "Required Only"
        (subtest "Not Enough Arguments"
          (test-error arity-error
                      "/import(core)"
                      "add(x, y) : x + y"
                      "add(a) -> b"))

        (subtest "Too Many Arguments"
          (test-error arity-error
                      "/import(core)"
                      "1+(x) : x + 1"
                      "1+(a,b) -> cc")))

      (subtest "Optional Arguments"
        (subtest "Not Enough Arguments"
          (test-error arity-error
                      "/import(core)"
                      "add(x, y, z : 1) : x + y + z"
                      "add(a) -> b"))

        (subtest "Too Many Arguments"
          (test-error arity-error
                      "/import(core)"
                      "1+(x, y : 1) : x + y"
                      "1+(a,b,cc) -> d"))

        (subtest "Required Only"
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

        (subtest "Required and Optional"
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

        (subtest "Optional with Node Default Value"
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

        (subtest "Optional with Functor Default Value"
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

              (test-simple-binding 1+a b)))))

      (subtest "Rest Arguments"
        (subtest "Not Enough Arguments"
          (test-error arity-error
                      "/import(core); add(x, y, ..(xs)) : x + y + xs; add(a) -> b"))

        (subtest "Minimum Number of Arguments"
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

        (subtest "More than number of operands"
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

        (subtest "Zero arguments"
          (with-module-table modules
            (build "f(..(xs)) : xs"
                   "f() -> a")

            (with-nodes ((f "f") (ff ("f")) (a "a"))
                modules

              (test-value-function ff f (list f (argument-list nil)))
              (test-simple-binding ff a))))))

    (subtest "External Meta-Nodes"
      (subtest "Required Only"
        (subtest "Not Enough Arguments"
          (test-error arity-error
                      "/external(add, x, y)"
                      "add(a) -> b"))

        (subtest "Too Many Arguments"
          (test-error arity-error
                      "/external(1+, x)"
                      "1+(a,b) -> cc")))

      (subtest "Optional Arguments"
        (subtest "Not Enough Arguments"
          (test-error arity-error
                      "/external(add, x, y, z : 1)"
                      "add(a) -> b"))

        (subtest "Too Many Arguments"
          (test-error arity-error
                      "/external(1+, x, y : 1)"
                      "1+(a,b,cc) -> d"))

        (subtest "Required Only"
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

        (subtest "Required and Optional"
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

        (subtest "Optional with Node Default Value"
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

        (subtest "Optional with Functor Default Value"
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

              (test-simple-binding 1+a b)))))

      (subtest "Rest Arguments"
        (subtest "Not Enough Arguments"
          (test-error arity-error
                      "/import(core)"
                      "/external(add, x, y, ..(xs))"
                      "add(a) -> b"))

        (subtest "Minimum Number of Arguments"
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

        (subtest "More than number of operands"
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

        (subtest "Zero arguments"
          (with-module-table modules
            (build "/external(f, ..(xs))"
                   "f() -> a")

            (with-nodes ((f "f") (ff ("f")) (a "a"))
                modules

              (test-value-function ff f (list f (argument-list nil)))
              (test-simple-binding ff a)))))))

  (subtest "Referencing Meta-Nodes as Values"
    (subtest "As Operands"
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

    (subtest "With Default Arguments"
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

    (subtest "As Source of Binding"
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

    (subtest "As Target of Binding"
      (test-error target-node-error "/external(add); x -> add")))

  (subtest "Subnodes"
    (labels ((test-object-fn (node &rest fields)
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
          (test-simple-binding a.last y :context a.last)))

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
          (test-simple-binding person.first name :context person.first)))

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

  (subtest "Node States"
    (subtest "From any state transition"
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

    (subtest "From specific state transition"
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
            (test-binding the-state state-on))))))

  (subtest "Modules"
    (with-module-table modules
      (build "x;y"
             "/module(my-mod); a; b"
             "/module(my-mod); /import(builtin); +(x,y) : add(x,y); /operator(+, 50, left)"
             "/module(my-mod); /export(a,+)")

      (with-modules ((init :init) (my-mod "my-mod"))
          modules
        (test-nodes init "x" "y")

        (with-nodes ((my-mod.a "a") (my-mod.b "b") (+ "+")) my-mod
          (subtest "Test /use operator"
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
                  (test-node-function n+m + + n m)))))

          (subtest "Test /use-as operator"
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
                  (test-node-function j+k + + j k)))))

          (subtest "Test /import operator with arguments"
            (build "/module(mod4); /import(my-mod, +); a + b")

            (with-modules ((mod4 "mod4")) modules
              (with-nodes ((a "a") (b "b"))
                  mod4

                (with-nodes ((a+b (("/in" "my-mod" "+") ("/in" "mod4" "a") ("/in" "mod4" "b"))))
                    init

                  (test-node-function a+b + + a b)

                  (isnt a my-mod.a :test #'eq)
                  (isnt b my-mod.b :test #'eq)))))

          (subtest "Test /import operator without arguments"
            (build "/module(mod5); /import(my-mod); a + b")

            (with-modules ((mod5 "mod5")) modules
              (with-nodes ((a "a") (b "b"))
                  mod5

                (with-nodes ((a+b (("/in" "my-mod" "+") ("/in" "my-mod" "a") ("/in" "mod5" "b"))))
                    init

                  (test-node-function a+b + + a b)

                  (is a my-mod.a :test #'eq)
                  (isnt b my-mod.b :test #'eq)))))

          (subtest "Test /in operator"
            (build "/module(mod6); /in(my-mod,+)(/in(my-mod, a), b)")

            (with-modules ((mod6 "mod6")) modules
              (with-nodes ((b "b"))
                  mod6

                (with-nodes ((a+b (("/in" "my-mod" "+") ("/in" "my-mod" "a") ("/in" "mod6" "b"))))
                    init

                  (test-node-function a+b + + my-mod.a b)

                  (isnt b my-mod.b :test #'eq)))))

          (subtest "Cross-Module Conditionally Active Bindings"
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

      (subtest "Errors"
        (subtest "Module Semantics"
          (is-error (build "/module(mod2); +(j,k)") 'non-existent-node-error)
          (is-error (build "/module(mod2); j + k") 'declaration-parse-error)
          (is-error (build "/module(mod2); my-mod.z") 'non-existent-node-error)

          (is-error (build "/module(mod3); +(j,k)") 'non-existent-node-error)
          (is-error (build "/module(mod3); j + k") 'declaration-parse-error)
          (is-error (build "/module(mod3); m.z") 'non-existent-node-error)

          (is-error (build "/module(mod4); /import(builtin); my-mod.+(a,b)")
                    'non-existent-node-error)
          (is-error (build "/module(mod4); /in(my-mod, z)") 'non-existent-node-error)

          (test-error non-existent-module-error "/use(no-such-module)")
          (test-error non-existent-module-error "/use-as(no-such-module, m)")
          (test-error non-existent-module-error "/import(no-such-module)")
          (test-error non-existent-module-error "/import(no-such-module, node)")
          (test-error non-existent-module-error "/in(no-such-module, x)")
          (test-error non-existent-node-error "/export(no-such-node)")
          (test-error non-existent-node-error "x; /export(x, no-such-node)"))

        (subtest "/module Operator Syntax"
          (test-error invalid-arguments-error "/module()")
          (test-error invalid-arguments-error "/module(a, b, c)")
          (test-error invalid-arguments-error "/module(1, 2, 3)")
          (test-error invalid-arguments-error "/module(1)")

          (test-top-level-only "/module(m)"))

        (subtest "/use Operator Syntax"
          (test-error invalid-arguments-error "/use(1,2,3)")
          (test-top-level-only "/use(m1)" "/module(m1)" "/module(m2); /import(builtin)"))

        (subtest "/use-as Operator Syntax"
          (test-error invalid-arguments-error "/use-as()")
          (test-error invalid-arguments-error "/use-as(m)")
          (test-error invalid-arguments-error "/use-as(1)")
          (test-error invalid-arguments-error "/use-as(1,2)")
          (test-error invalid-arguments-error
                      "/module(m1); /module(m2); /use-as(m1, m, x, y)")

          (test-top-level-only "/use-as(mod, m)" "/module(mod)" "/module(m1); /import(builtin)"))

        (subtest "/import Operator Syntax"
          (test-error invalid-arguments-error "/import()")
          (test-error invalid-arguments-error "/import(1)")

          (test-top-level-only "/import(mod)" "/module(mod)" "/module(m1); /import(builtin)")
          (test-top-level-only "/import(mod, x)" "/module(mod); x" "/module(m1); /import(builtin)"))

        (subtest "/export Operator Syntax"
          (test-error semantic-error "/export(1, 2, 3)")

          (test-top-level-only "/export(x)" "x"))

        (subtest "/in Operator Syntax"
          (test-error invalid-arguments-error "/in()")
          (test-error invalid-arguments-error "/in(x)")
          (test-error invalid-arguments-error
                      "/module(m1); x; /module(m2); /in(m1, x, y)")
          (test-error invalid-arguments-error "/in(1, x)"))

        (subtest "Referencing Module Pseudo Nodes"
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
                      "x -> m1"))))))

(subtest "Node Coalescer"
  (subtest "Simple Nodes"
    (subtest "One-Way Bindings"
      (with-module-table modules
	(build "a -> b; b -> cc; cc -> d"
	       "/attribute(a, input, 1)")

	(let ((table (finish-build)))
	  (test-not-nodes table "b" "cc")

	  (with-nodes ((a "a") (d "d")) table
	    (test-simple-binding a d))))

      (subtest "COALESCABLE Attribute"
        (with-module-table modules
          (build "a -> b; b -> cc; cc -> d"
                 "/attribute(a, input, 1)"
                 "/attribute(b, coalescable, 0)")

          (let ((table (finish-build)))
            (test-not-nodes table "cc")

            (with-nodes ((a "a") (b "b") (d "d")) table
	      (test-simple-binding a b)
	      (test-simple-binding b d))))))

    (subtest "Two-Way Bindings"
      (with-module-table modules
        (build "a -> b; b -> cc; cc -> d"
               "d -> cc; cc -> b; b -> a"
               "/attribute(a, input, 1)")

        (let ((table (finish-build)))
          (test-not-nodes table "b" "cc")

	  (with-nodes ((a "a") (d "d")) table
	    (test-simple-binding a d))))

      (subtest "COALESCABLE Attribute"
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
	      (test-simple-binding b d))))))

    (subtest "Multiple Observers"
      (with-module-table modules
        (build "a -> b; b -> cc; b -> d; cc -> e; d -> f"
               "/attribute(a, input, 1)")

        (let ((table (finish-build)))
          (test-not-nodes table "cc" "d")

          (with-nodes ((a "a") (b "b") (e "e") (f "f")) table
            (test-simple-binding a b)
            (test-simple-binding b e)
            (test-simple-binding b f))))))

  (subtest "Functor Nodes"
    (subtest "Simple Functor Nodes"
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
            (has-value-function (a b cc d) output `(,+ (,+ (,+ ,a ,b) ,cc) ,d)))))

      (subtest "COALESCABLE Attribute"
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
              (has-value-function (a+b cc d) output `(,+ (,+ ,a+b ,cc) ,d)))))))

    (subtest "Multiple Observers"
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
            (has-value-function (a b) a+b `(,+ ,a ,b)))))

      (with-module-table modules
        (build "/external(add, x, y)"
               "a -> b; b -> cc; b -> d; add(cc,d) -> e"
               "/attribute(a, input, 1)")

        (let ((table (finish-build)))
          (test-not-nodes table "b" "cc" "d")

          (with-nodes ((add "add") (a "a") (e "e")) table
            (has-value-function (a) e (list add a a))))))

    (subtest "Object Nodes"
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

    (subtest "Cycles"
      (subtest "Non-Coalescable"
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

      (subtest "Coalescable"
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

              (let ((block (expression-block nil :count 2)))
                (has-value-function (a b) output
                  (progn
                    (setf (expression-block-expression block)
                          `(,cons ,a (,cons ,b ,(cyclic-reference block))))

                    block)))))))))

  (subtest "Removing Unreachable Nodes"
    (with-module-table modules
      (build "a -> b; b -> cc;"
             "e -> f"                   ; Unreachable Nodes
             "/attribute(a, input, 1)")

      (let ((table (finish-build)))
        (test-not-nodes table "b" "e" "f")

        (with-nodes ((a "a") (cc "cc")) table
          (test-simple-binding a cc))))

    (subtest "REMOVABLE Attribute"
      (with-module-table modules
        (build "some-special-node"
               "/attribute(some-special-node, removable, 0)")

        (let ((table (finish-build)))
          (test-nodes table "some-special-node"))))

    (subtest "Unreachable Dependency Errors"
      (with-module-table modules
        (build "/external(add, x, y)"
               "a -> b; add(b, d) -> output"
               "cc -> d"                ; Unreachable Nodes
               "/attribute(a, input, 1)")

        (is-error (finish-build) 'dependency-not-reachable-error))))

  (subtest "Removing Unused Meta-nodes"
    (subtest "Remove all nodes in core module"
      (with-module-table modules
        (build-core-module)
        (build "/import(core)"
               "f(x) : x"
               "x -> y"
               "/attribute(x, input, 1)")

        (let ((table (finish-build)))
          (is (length (meta-nodes table)) 0))))

    (subtest "Do not remove used nodes"
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

    (subtest "Do not remove referenced nodes"
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
              `(,map ,(meta-node-ref f) ,a))

            (test-meta-node f ((z "z")) `(,1+ ,z)))))))

  (subtest "Cross-Module Bindings"
    (subtest "Simple Bindings"
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

    (subtest "Functor Nodes"
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
                (has-value-function (in cc) output `(,+ (,+ ,in ,in) ,cc)))))))))

  (subtest "Common Sub Expressions"
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
                (,nan? ,(expression-block `(,parse ,in) :count 2)))

              (,(id-symbol "value")
                ,(expression-block `(,parse ,in) :count 2)))))))

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
            `(,add (,add ,a ,(expression-block `(if (,even? (,add ,a ,1)) (,add ,a 1) (:fail :none)))) ,d))))))

  (subtest "Explicit Context"
    (subtest "Common Operands"
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
                 ,(expression-block
                   `(if (,< ,a 3) (,+ ,a ,b) (:fail :none)))
                 ,(expression-block
                   `(if (,> ,a 4) (,- ,a ,b) (:fail :none)))
                 :none)
                ,b
                :none))))))

    (subtest "Common Operands without Coalescing"
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
                ,(expression-block
                  `(if (,< ,a 3) ,cc (:fail :none)))
                ,b
                :none))))))

    (subtest "Common Operand without Default"
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
                ,(expression-block
                  `(if (,< ,a 3) (,+ ,a ,b) (:fail :none)))
                ,(expression-block
                  `(if (,< ,b 4) ,b (:fail :none)))
                :none))))))

    (subtest "Single Common Ancestor"
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
                ,(expression-block
                  `(if (,< ,a 3) (,+ ,a ,b) (:fail :none)))
                ,b
                :none))))))

    (subtest "Single Common Ancestor without Default"
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
                ,(expression-block
                  `(if (,< ,a 3) (,+ ,a ,b) (:fail :none)))
                ,(expression-block
                  `(if (,< ,b 4) ,b (:fail :none)))
                :none))))))

    (subtest "Test Creation Order"
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
                :none)))))))

  (subtest "Node States"
    (subtest "From any state transition"
      (with-module-table modules
        (build-core-module)
        (build "/import(core, not)"
               "not(on) -> /state(on, toggle)"
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
                    (,previous ,(node-ref toggle-state-on))
                    (,previous ,(node-ref on))))

            (test-binding start on)


            (has-value-function (on)
                toggle-state-on
              `(,not ,on))))))

    (subtest "From specific state transition"
      (with-module-table modules
        (build-core-module)
        (build "/import(core, not)"
               "not(on) -> /state(on, default, toggle)"
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
                (,if (,equal (,previous ,(node-ref state-on)) ,(id-symbol "default"))
                     (,equal ,state ,(id-symbol "toggle"))
                     ,(node-ref *node-false*))

                (,previous ,(node-ref toggle-state-on))
                (,previous ,(node-ref on))))

            (test-binding start on)


            (has-value-function (on)
                toggle-state-on
              `(,not ,on))

            (test-binding the-state state-on)))))))

(subtest "Constant Folding"
  (subtest "Literal Constant Values"
    (with-module-table modules
      (build "/external(add, x, y)"
             "add(a, b) -> cc"
             "1 -> constant; constant -> b"
             "/attribute(a, input, 1)")

      (let ((table (finish-build)))
        (test-not-nodes table "b" "constant")

        (with-nodes ((add "add") (a "a") (cc "cc")) table
          (has-value-function (a) cc `(,add ,a 1)))))

    (with-module-table modules
      (build "a -> b; b -> cc"
             "3 -> constant; constant -> b"
             "/attribute(a, input, 1)")

      (let ((table (finish-build)))
        (test-not-nodes table "constant")

        (with-nodes ((a "a") (b "b") (cc "cc")) table
          (test-simple-binding a b)
          (test-simple-binding b cc)

          (test-value-function b (init-context b) 3))))

    (with-module-table modules
      (build "a -> b; b -> cc"
             "\"hello\" -> constant; constant -> a"
             "/attribute(a, input, 1)")

      (let ((table (finish-build)))
        (test-not-nodes table "b" "constant")

        (with-nodes ((a "a") (cc "cc")) table
          (test-simple-binding a cc)

          (test-value-function a (init-context a) "hello")))))

  (subtest "Constant Functor Nodes"
    (with-module-table modules
      (build "/external(add, x, y)"
             "add(a, b) -> cc"

             "1 -> c1; 2 -> c2; c2 -> c3"
             "add(c1, c3) -> b"

             "/attribute(a, input, 1)")

      (let ((table (finish-build)))
        (test-not-nodes table "b" "c1" "c2" "c3")

        (with-nodes ((add "add") (a "a") (cc "cc")) table
          (has-value-function (a) cc `(,add ,a (,add 1 2))))))

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

          (test-value-function b (init-context b) `(,add 5 (,add 10 10))))))

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

  (subtest "Cross-Module Constant Folding"
    (with-module-table modules
      (build "/module(m1)"
             "/import(builtin)"
             "1 -> constant"
             "constant -> param"

             "/module(m2)"
             "/import(builtin)"
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

  (subtest "Two-way Bindings"
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

            `(,add 1 ,y)))))))

(subtest "Structure Checking"
  (subtest "Ambiguous Context Checks"
    (subtest "Simple Bindings"
      (with-module-table modules
        (build "a -> d"
               "a -> b; b -> cc; cc -> d"
               "d -> e"

               "/attribute(a, input, 1)")

        (is-error (finish-build) 'ambiguous-context-error))

      (subtest "Cross-Module Bindings"
        (with-module-table modules
          (build "/module(m1)"
                 "/import(builtin)"
                 "a -> d"
                 "/attribute(a, input, 1)"

                 "/module(m2)"
                 "/import(builtin)"
                 "/use(m1)"

                 "m1.a -> b; b -> cc; cc -> m1.d"
                 "m1.d -> e")

          (is-error (finish-build) 'ambiguous-context-error))))

    (subtest "Functional Bindings"
      (with-module-table modules
        (build "/external(add, x, y)"
               "add(a, b) -> d"
               "a -> cc; cc -> d"
               "/attribute(a, input, 1)"
               "/attribute(b, input, 1)")

        (is-error (finish-build) 'ambiguous-context-error))

      (subtest "Cross-Module Bindings"
        (with-module-table modules
          (build "/module(m1)"
                 "a; b;"
                 "/attribute(a, input, 1)"
                 "/attribute(b, input, 1)"

                 "/module(m2)"
                 "/import(builtin)"
                 "/use(m1)"

                 "/external(add, x, y)"

                 "add(m1.a, m1.b) -> d"
                 "m1.a -> cc; cc -> d"
                 "d -> e")

          (is-error (finish-build) 'ambiguous-context-error))))))

(subtest "Test Building Meta-Node Definitions"
  (subtest "Simple Functions"
    (subtest "Single Module"
      (with-module-table modules
        (build "/external(add, x, y)"
               "1+(n) : add(n, 1)"
               "/attribute(1+, removable, 0)")

        (let ((table (finish-build)))
          (with-nodes ((add "add") (fn "1+")) table
            (test-meta-node fn ((n "n")) `(,add ,n 1))
            (test-not-nodes (definition fn) '("add" "n" 1))))))

    (subtest "Multiple Modules"
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
            (test-not-nodes (definition fn) '(("/in" "m1" "add") "n" 1)))))))

  ;; The following tests also test that node-coalescing and constant
  ;; folding work within meta-node definitions.

  (subtest "Recursive Functions"
    (subtest "Simple Recursion"
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

    (subtest "Tail Recursion with Nested Functions"
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
                              '#10=(("/in" "core" "<") "k" "limit")
                              '#11=(("/in" "core" "*") "k" "acc")
                              '#12=("iter" "next" #11#)
                              '(("/in" "core" "if") #10# "acc" #12#)))))))

    (subtest "Mutually Recursive Functions"
      (subtest "Single Module"
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
                              '#20=(("/in" "core" ">") "n" "1")
                              '#21=(("/in" :init "fib1") "n")
                              '#22=(("/in" :init "fib2") "n")
                              '#23=(("/in" "core" "+") #21# #22#)
                              '(("/in" "core" "if") #20# #23# 1))

              (test-not-nodes (definition fib1)
                              '#30=(("/in" "core" "-") "n" "1")
                              '#31=((":in" /init "fib") #30#))

              (test-not-nodes (definition fib2)
                              '#40=(("/in" "core" "-") "n" "2")
                              '#41=(("/in" :init "fib") #40#))

              (has-value-function (in) out `(,fib ,in))))))

      (subtest "Multiple Modules"
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
                              '#50=(("/in" "core" ">") "n" "1")
                              '#51=(("/in" "m1" "fib1") "n")
                              '#52=(("/in" "m1" "fib2") "n")
                              '#53=(("/in" "core" "+") #51# #52#)
                              '(("/in" "core" "if") #50# #53# 1))

              (test-not-nodes (definition fib1)
                              '#60=(("/in" "core" "-") "n" "1")
                              '#61=(("/in" "m2" "fib") #60#))

              (test-not-nodes (definition fib2)
                              '#70=(("/in" "core" "-") "n" "2")
                              '#71=(("/in" "m2" "fib") #70#))

              (has-value-function (in) out `(,fib ,in))))))))

  (subtest "Self-Node References"
    (subtest "Single Context"
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

    (subtest "Multiple Contexts"
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
             (test-meta-node fn ((x "x")))))))))

  (subtest "Outer-Node References"
    (subtest "Simple Outer-Node References"
      (subtest "Single Module"
        (subtest "Explicit"
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

        (subtest "Implicit"
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
                  `(,addx ,in1 :outer (,x . ,dx) (,y . ,dy))))))))

      (subtest "Multiple Modules"
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
                `(,addx ,in1 :outer (,x . ,dx) (,y . ,dy))))))

        (subtest "Subnodes of outer nodes."
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

                (test-simple-binding in1 dict.x)))))))

    (subtest "Outer-Node References from Mutually Recursive Functions"
      (subtest "Single Module"
        (subtest "Explicit"
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

        (subtest "Implicit"
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
                  `(,addy ,in2 :outer (,x . ,dx) (,y . ,dy))))))))

      (subtest "Multiple Modules"
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
                `(,addy ,in2 :outer (,x . ,dx) (,y . ,dy))))))))

    (subtest "Outer-Node References from Nested Functions"
      (subtest "Single Module"
        (subtest "Explicit"
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

        (subtest "Implicit"
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
                  `(,count ,in :outer (,start . ,in-start))))))))

      (subtest "Multiple Modules"
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
                `(,count ,in :outer (,start . ,in-start))))))))

    (subtest "Optional Argument Default Values"
      (subtest "With Default Values"
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

      (subtest "Without Default Values"
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

              (has-value-function (in) out `(,fact ,in)))))))

    (subtest "Constant Folding"
      (subtest "Constant Outer Nodes"
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

      (subtest "Functor Outer Nodes"
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

      (subtest "Functor of Constant Nodes"
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

      (subtest "In Functor"
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

      (subtest "In Nested Node"
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

      (subtest "Functor of Constant Outer Node In Nested Node"
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

      (subtest "Outer-Nodes of Meta-node used in Multiple Other Meta-nodes"
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

      (subtest "Outer-Nodes of Compiled Meta-Node"
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

      (subtest "Outer-Nodes in Meta-Node References"
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
              `(,inc-all ,x)))))))

  (subtest "Meta-Node References"
    (subtest "Referencing Meta-Nodes Without Outer Nodes"
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

    (subtest "Referencing Meta-Nodes With Outer Nodes"
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

    (subtest "Referencing Meta-Nodes With Outer Nodes From Nested Meta-Nodes"
      (subtest "Case 1"
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

      (subtest "Case 2"
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
                `(,x+-all ,in :outer (,x . ,in-x))))))))

    (subtest "Cyclic Meta-Node References"
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

    (subtest "As Source of Binding"
      (subtest "Without Outer Nodes"
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

      (subtest "With Outer Nodes"
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
                `(,map ,(meta-node-ref add-x :outer-nodes `((,x . ,x-in))) ,in)))))))

    (subtest "Referencing Meta-Nodes with Optional Arguments"
      (subtest "With constant default values"
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

      (subtest "With node default values"
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
              `(,inc-all ,input :outer (,delta . ,in-delta))))))))

  (subtest "Node Auto Creation"
    ;; The previous tests already test automatic node
    ;; creation. These tests focus on the special cases:

    (subtest "Operand of Meta-Node in Target Position"
      (with-module-table modules
        (build "/external(int, x)"
               "/attribute(int, target-node, int)"
               "f(x) : { x -> int(y); y }"
               "/attribute(f, removable, 0)")

        (let ((table (finish-build)))
          (with-nodes ((f "f") (int "int"))
              table

            (test-meta-node f ((x "x")) `(,int ,x))))))

    (subtest "Atom Declarations"
      (with-module-table modules
        (build "/external(add,a,b)"
               "f(x) : { z; add(x,1) -> z; z; }"
               "/attribute(f, removable, 0)")

        (with-nodes ((f "f") (add "add")) (finish-build)
          (test-meta-node f ((x "x"))
            `(,add ,x 1)))))

    (subtest "Errors"
      (subtest "Operands"
        (with-module-table modules
          (build "/external(add, x, y)"
                 "f(x) : add(x, y, z)"
                 "/attribute(f, removable, 0)")

          (is-error (finish-build) 'non-existent-node-error)))

      (subtest "Binding Source"
        (with-module-table modules
          (build "f(x) : z -> self"
                 "/attribute(f, removable, 0)")

          (is-error (finish-build) 'non-existent-node-error)))

      (subtest "Object Nodes"
        (with-module-table modules
          (build "f(x) : n.z"
                 "/attribute(f, removable, 0)")

          (is-error (finish-build) 'non-existent-node-error)))))

  (subtest "Cyclic References"
    (with-module-table modules
      (build-source-file "./test/inputs/builder/cyclic-references.trd" modules)

      (with-nodes ((f "f") (cons "cons")) (finish-build)
        (test-meta-node f ((a "a") (b "b"))
          (aprog1 (expression-block nil :count 2)
            (setf (expression-block-expression it)
                  `(,cons ,a (,cons ,b ,(cyclic-reference it)))))))))

  (subtest "Structure Checking"
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
                    }"
                 "/attribute(f, removable, 0)")

          (is-error (finish-build) 'ambiguous-context-error))

        (with-module-table modules
          (build "f(a, b) : {
                      a -> self
                      b -> self
                    }"
                 "/attribute(f, removable, 0)")

          (is-error (finish-build) 'ambiguous-context-error)))

      (subtest "Functional Bindings"
        (with-module-table modules
          (build "/external(add, x, y)"

                 "f(a, b) : {
                      add(a, b) -> d
                      a -> c; c -> d
                      d
                    }"
                 "/attribute(f, removable, 0)")

          (is-error (finish-build) 'ambiguous-context-error))

        (with-module-table modules
          (build "/external(add, x, y)"

                 "f(a, b) : {
                      a -> self
                      b -> self
                      add(a, b) -> self
                    }"
                 "/attribute(f, removable, 0)")

          (is-error (finish-build) 'ambiguous-context-error)))

      (subtest "Multiple Contexts Activated by Multiple Operands"
        (with-module-table modules
          (build "f(a, b) : { a -> c; b -> c; c }")

          (is-error (finish-build) 'ambiguous-context-error)))

      (subtest "Node States within meta-node"
        (with-module-table modules
          (build-core-module)
          (build-source-file "./test/inputs/builder/states-in-meta-node.trd" modules)
          (is-error (finish-build) 'ambiguous-context-error)))))

  (subtest "Constant Functions"
    (subtest "Ignored Arguments"
      (with-module-table modules
        (build "f(x) : 1"
               "/attribute(f, removable, 0)")

        (let ((table (finish-build)))
          (with-nodes ((f "f")) table
            (test-value-function f (car (first (contexts f))) 1))))))

  (subtest "Errors"
    (subtest "Meta-Node Without Function."
      (with-module-table modules
        (build "f(x) : x -> z"

               "f(in) -> out"
               "/attribute(in, input, 1)")

        (is-error (finish-build) meta-node-no-function-error)))))


(finalize)

(cl-interpol:disable-interpol-syntax)
