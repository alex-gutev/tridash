;;;; common.lisp
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

(defpackage :tridash/test.util
  (:use :generic-cl
        :alexandria
        :anaphora
        :arrows
        :iterate
        :optima
        :named-readtables

        :tridash.parser
        :tridash.frontend

        :fiveam)

  (:shadowing-import-from :generic-cl
                          :emptyp
                          :multiply
                          :accumulate)

  (:shadowing-import-from :fiveam :fail)

  (:import-from :lol
                :defmacro!
                :lol-syntax)

  (:import-from :tridash.util
                :with-struct-slots)

  (:import-from :tridash.frontend
                :outer-nodes
                :change-module)

  (:local-nicknames (:ir :tridash.ir))

  (:export :match*
           :shadow-function
           :previous

           :decls
           :declaration-symbols
           :node-id
           :decl=
           :s

           :with-test-suite

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

           :*strict-test*
           :value-fn-equal
           :object-fn-equal

           :test-value-function
           :test-node-function
           :test-meta-node
           :init-context
           :outer

           :test-error
           :test-top-level-only

           :with-module-table
           :build
           :finish-build
           :with-nodes
           :with-modules
           :with-dependencies
           :has-value-function)

  (:documentation
   "Contains utility functions and macros which are used in all test
    packages."))


(in-package :tridash/test.util)

(in-readtable lol-syntax)


;;; General Utilities

(defmacro match* ((&rest forms) &rest clauses)
  `(multiple-value-match (values ,@forms) ,@clauses))

(defmacro! shadow-function ((name (&rest fn-lambda-list) &body fn-body) &body body)
  "Replaces the definition of a function, by (SETF FDEFINITION), such
   that any invocation of the function, within the dynamic extent of
   the SHADOW-FUNCTION form will invoke the new definition.

   FN-LAMBDA-LIST and FN-BODY are the new lambda list and body of the
   function. BODY are the forms making up the body (the dynamic
   extent) of the SHADOW-FUNCTION form."

  `(let ((,g!previous (fdefinition ',name)))
     (unwind-protect
          (progn
            (flet ((previous (&rest ,g!args)
                     (apply ,g!previous ,g!args)))
              (setf (fdefinition ',name)
                    (lambda ,fn-lambda-list ,@fn-body)))
            ,@body)
       (setf (fdefinition ',name) ,g!previous))))

(defmacro with-test-suite (name (description &optional parent) &body tests)
  `(progn
     (def-suite ,name
         :description ,description
         ,@(when parent
             `(:in ,parent)))

     (in-suite ,name)
     ,@tests
     (in-suite nil)))


;;; Symbol Table

(defun declaration-symbols (decls)
  "Replace symbols in SYMS which begin with a !, with the equivalent
   symbol in the TRIDASH.SYMBOLS package."

  (labels ((!-sym-p (sym)
             (when (= #\! (char (symbol-name sym) 0))
               (id-symbol (subseq (symbol-name sym) 1))))

           (replace-!-syms (decl)
             (typecase decl
               (symbol
                (or (!-sym-p decl) decl))

               (cons
                (map #'replace-!-syms decl))

               (otherwise decl))))

    (replace-!-syms decls)))

(defmacro decls (&rest decls)
  "Utility macro for creating node declaration lists. Each form in
   DECLS is evaluated with the all symbols, which begin with a !
   replaced with the symbol with an equivalent symbol name in the
   TRIDASH.SYMBOLS package.

   Might not work across all implementations if the backquote syntax
   is used."

  (labels ((!-sym-p (sym)
             (when (= #\! (char (symbol-name sym) 0))
               (id-symbol (subseq (symbol-name sym) 1))))

           (replace-!-syms (decl)
             (typecase decl
               (symbol
                (or (!-sym-p decl) decl))

               (cons
                (map #'replace-!-syms decl))

               (otherwise decl))))
    `(list ,@(replace-!-syms decls))))

(defun node-id (name)
  "Converts a string/list node identifier to an identifier symbol."

  (typecase name
    (cons (cons (node-id (car name)) (node-id (cdr name))))
    (string (id-symbol name))
    (otherwise name)))

(defun s (string) (node-id string))


;;; Node declaration Comparison

(defun decl= (expected got)
  "Returns true if the node declaration GOT is equivalent to
   EXPECTED."

  (match* (got expected)
    (((cons got-car got-cdr)
      (cons exp-car exp-cdr))

     (and (decl= exp-car got-car)
          (decl= exp-cdr got-cdr)))

    ((nil nil) t)

    (((atom-expression- (identifier got))

      (and (type symbol) expected))

     (= got expected))

    (((node-list-expression- (operands got-ops))
      (list* (eql (id-symbol +list-operator+)) exp-ops))

     (and (length= got-ops exp-ops)
          (every #'decl= exp-ops got-ops)))

    (((functor-expression- (operator got-op) (operands got-args))

      (list* exp-op exp-args))

     (and (decl= exp-op got-op)
          (length= got-args exp-args)
          (every #'decl= exp-args got-args)))

    (((literal-value- (value got))
      expected)

     (= got expected))))


;;;; Building From Strings

(defun build-nodes (string modules)
  "Builds the nodes parsed from STRING into the `MODULE-TABLE'
   MODULES."

  (with-input-from-string (in string)
    (build-parsed-nodes (make-parser in) modules)))

(defun build-core-module (&optional (modules *global-module-table*))
  "Builds the `core` module into the `MODULE-TABLE' MODULES."

  (build-source-file #p"./modules/core/builtin.trd" modules)
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


;;; Retrieving Nodes

(defun ensure-node-table (thing)
  "If THING is a `MODULE-TABLE' return the node table bound to the
   CURRENT-MODULE slot. Otherwise if THING is a `MODULE' return it."

  (etypecase thing
    (module-table (current-module thing))
    (module thing)))

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


;;; Testing Existence of  Nodes

(defun test/get-node (name node-table)
  "Retrieves the node with name NAME from NODE-TABLE and checks that
   its NAME slot matches NAME."

  (aprog1 (get-node name node-table)
    (is (typep it 'node) "Object for ~a is not a NODE: ~a." name it)
    (is (name it) (node-id name) "Name of ~a is not ~a. Got" it name (name it))))

(defun test-nodes (table &rest ids)
  "Tests that each identifier in IDS names a node in MODULES."

  (map (rcurry #'test/get-node table) ids))


(defun test-not-node (table id)
  "Tests that there is no node with identifier ID in MODULES."

  (let ((node (get-node id table)))
    (is-false node "Unexpected node for ~a. Got: ~a." id node)))

(defun test-not-nodes (node-table &rest ids)
  "Tests that there are no nodes with identifiers IDS in MODULES."

  (foreach (curry #'test-not-node node-table) ids))


;;; Testing Bindings

(defun test-binding (src target &key (context nil context-sp))
  "Tests that a binding between node SRC and TARGET has been
   established in the context CONTEXT. If CONTEXT is not supplied the
   context of the `IR:LINK' between SRC and TARGET is
   checked. Returns the `IR:LINK' object."

  (let ((link (get target (observers src)))
        (name-src (name src))
        (name-target (name target)))

    (is (typep link 'ir:link)
        "Binding object for ~a -> ~a is not a LINK Got: ~a."
        name-src name-target link)

    (is (= link (get src (dependencies target)))
        "Node link (~a -> ~a) not added to dependency set of ~a."
        name-src name-target name-target)

    (is (= (ir:node link) src)
        "Node link (~a -> ~a) does not point to ~a. Got: ~a."
        name-src name-target name-src (ir:node link))

    (when context-sp
      (is (= (ir:context link) context)
          "Node link (~a -> ~a) context is not ~a. Got: ~a."
          name-src name-target context (ir:context link)))

    (let* ((id (if context-sp context (ir:context link)))
	   (context (get id (contexts target))))

      (is (typep context 'node-context)
	  "Context ~a of link ~a -> a is not a NODE-CONTEXT. Got: ~a."
          id name-src name-target context)

      (is (= (get src (operands context)) link)
          "Operand ~a of context ~a is not equal to link ~a -> ~b. Got: ~a."
          name-src id name-src name-target
          (get src (operands context))))

    link))

(defun test-simple-binding (src target &rest args)
  "Tests that a binding between node SRC and TARGET has been
   established and that the value function of TARGET (in the context
   of the `IR:LINK' or the context provided as the :CONTEXT
   parameter) is the `IR:LINK' itself."

  (let ((link (apply #'test-binding src target args)))
    (test-value-function target (ir:context link) link)))


;;;; Compare Node Value Functions

(defvar *strict-test* t
  "If set to true the expression comparison is strict meaning the
   expected expression has to match the actual expression exactly,
   with the exception that the actual expression may have
   `EXPRESSION-BLOCK's not specified in the expected expression. If
   set to NIL certain rules will be relaxed, such as expressions may
   be contained inside `IR:LINK' objects.")

(defgeneric value-fn-equal (a b))

(defmethod value-fn-equal ((a ir:link) (b ir:link))
  (eq a b))

(defmethod value-fn-equal ((a ir:link) b)
  (unless *strict-test*
    (value-fn-equal (ir:node a) b)))

(defmethod value-fn-equal ((a ir:node-ref) (b ir:node-ref))
  (eq (ir:node a)
      (ir:node b)))

(defmethod value-fn-equal ((got ir:functor) (exp list))
  (flet ((split-args (args)
           (aif (position :outer args)
                (values (subseq args 0 it) (subseq args (1+ it)))
                (values args nil))))

    (match* (got exp)
      (((ir:functor (ir:operator got-op) (ir:operands got-args) (ir:outer-nodes got-outer-nodes))
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

(defmethod value-fn-equal ((a ir:object) (b list))
  (flet ((field= (a b)
           (and (= (first a) (first b))
                (value-fn-equal (second a) (second b)))))

    (match* (a b)
      (((ir:object (ir:entries entries-a))
        (list* :object entries-b))

       (and (= (length entries-a) (length entries-b))
            (every #'field=
                   (sort entries-a :key (compose #'symbol-name #'first))
                   (sort entries-b :key (compose #'symbol-name #'first))))))))

(defvar *parent-expression-blocks* nil)

(defmethod value-fn-equal ((a ir:expression-block) (b ir:expression-block))
  (let ((*parent-expression-blocks* (acons a b *parent-expression-blocks*)))
    (match* (a b)
      (((ir:expression-block (ir:expression expr-a) (ir:use-count count-a))
        (ir:expression-block (ir:expression expr-b) (ir:use-count count-b)))

       (and (value-fn-equal expr-a expr-b)
            (= count-a count-b))))))

(defmethod value-fn-equal ((a ir:expression-block) b)
  (value-fn-equal (ir:expression a) b))

(defmethod value-fn-equal ((got ir:cyclic-reference) (exp ir:cyclic-reference))
  (=
   (cdr (assoc (ir:expression got) *parent-expression-blocks*))
   (ir:expression exp)))

(defmethod value-fn-equal ((got ir:meta-node-ref) (exp ir:meta-node-ref))
  (with-accessors ((got-node ir:meta-node)
                   (got-optional ir:optional-arguments)
                   (got-outer-nodes ir:outer-nodes))
      got

    (with-accessors ((exp-node ir:meta-node)
                     (exp-optional ir:optional-arguments)
                     (exp-outer-nodes ir:outer-nodes))
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

(defmethod value-fn-equal ((a ir:argument-list) (b ir:argument-list))
  (with-accessors ((args-a ir:arguments))
      a
    (with-accessors ((args-b ir:arguments))
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


;;; Testing Node Value Functions

(defun test-value-function (node context-id fn &key (test #'value-fn-equal))
  "Tests that the context CONTEXID of node NODE has value function FN."

  (with-slots (name) node
    (let ((context (get context-id (contexts node))))
      (is (typep context 'node-context)
          "Context ~a of node ~a is not a NODE-CONTEXT. Got ~a."
          context-id node context)

      (is (funcall test (value-function context) fn)
          "Value function of ~a in context ~a not equal to:~%~a~%Got:~%~a"
          node context-id fn (value-function context)))))

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
      (is-true it "~a does not have an :INIT context." node))))


;;; Test Utility Macros

(defvar *flat-node-table*)

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
                      (is (typep it 'module)
                          "~a is not a module. Got:~%~a"
                          ,name it))))))
    `(let ,(map #'make-binding modules)
       ,@body)))

(defmacro! with-dependencies ((&rest deps) o!node &body body)
  "Tests that NODE has the dependencies in DEPS and binds the
   corresponding `IR:LINK' objects to variables. Each element of
   DEPS is either a symbol naming a variable which evaluates to the
   dependency NODE or is of the form (VAR NODE) where NODE is a form
   that evaluates to the dependency node and VAR is the variable to
   which the `IR:LINK' object is bound."

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
   form with DEPS bound to the dependency `IR:LINK' objects.

   The value function of the context of the `IR:LINK', corresponding
   to the first dependency in DEPS, is tested that it is equal to
   FUNCTION."

  `(with-dependencies ,deps ,g!node
     (let ((,g!context (ir:context ,(ensure-car (first deps)))))
       (test-value-function ,g!node ,g!context ,function ,@test-args))))

(defmacro! test-meta-node (o!meta-node (&rest operands) &body (function &rest test-args))
  "Test that various properties of a meta-node are correct, namely
   that its definition has been built, its subgraph contains the
   implicit self-node and that its value-function is correct.

   OPERANDS is a list of the meta-node's operands (in the form passed
   to WITH-DEPENDENCIES).

   FUNCTION is the expected value function of the meta-node. Any
   symbol in the value function, that is bound to an operand (appears
   in OPERANDS), is replaced with the `IR:LINK' corresponding to the
   dependency between the operand node and meta-node."

  (flet ((make-input-test (operand)
           `(is (input-node? ,(first operand))
                "Argument ~a is not an input node." ',(second operand)))

         (get-operand-node (operand)
           (ematch operand
             ((list var (list 'outer node))
              `(,var (name (get ,node (outer-nodes ,g!meta-node)))))

             ((list var name)
              `(,var ',(node-id name))))))

    `(progn
       (is (typep ,g!meta-node 'meta-node)
           "~a is not a meta-node." ,g!meta-node)

       (with-slots ((,g!def definition)) ,g!meta-node
         (is (typep ,g!def 'flat-node-table)
             "Expected definition of meta-node ~a to be a `FLAT-NODE-TABLE'.~%Got:~%~a"
             ,g!meta-node ,g!def)

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
     (signals
         ,error
       (build ,@code)
       "`~a` does not raise an error of type `~s'." (list ,@code) ',error)))

(defun test-top-level-only (decl &rest code)
  "Tests that an error is raised if the declaration DECL appears in a
   non-top-level position. Each element in CODE is
   prepended (separated by ';') to DECL before performing the tests."

  (test-error special-operator-reference-error (format nil "~{~a; ~}~a -> a" code decl))
  (test-error special-operator-reference-error (format nil "~{~a; ~}a -> ~a" code decl))
  (test-error special-operator-reference-error (format nil "~{~a; ~}f(x) : x; f(~a)" code decl)))
