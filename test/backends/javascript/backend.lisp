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

;;;; JavaScript Backend Tests

(defpackage :tridash.test.backend.js
  (:use :cl
        :alexandria
        :anaphora
        :cl-arrows
        :iterate
        :optima
        :prove
        :named-readtables

        :tridash.parser
        :tridash.frontend
        :tridash.interface
        :tridash.backend.js.ast

        :tridash.test.util)

  (:shadowing-import-from :prove :fail)

  (:import-from :lol
                :defmacro!
                :lol-syntax)

  (:import-from :tridash
                :*module-search-paths*
                :build-program)

  (:import-from :tridash.frontend
                :build-source-file)

  (:import-from :tridash.backend.js

                :+thunk-class+

                :*node-ids*
                :*node-link-indices*
                :*meta-node-ids*
                :*context-ids*
                :*lazy-nodes*
                :*context-counter*
                :*initial-values*

                :async

                :meta-node-id
                :dependency-index

                :create-compute-function
                :create-meta-node

                :strip-redundant
                :output-code)

  (:import-from :tridash.test.builder
                :with-module-table
                :with-nodes

                :build
                :finish-build))


(in-package :tridash.test.backend.js)

(in-readtable lol-syntax)

(plan nil)


;;;; AST Equality Comparison

(defvar *current-context* nil
  "The context whose value function is currently being tested.")

(defvar *values-var* nil
  "The name of the variable storing the 'values' argument to the
   compute function.")

(defvar *ast-aliases* nil
  "Hash table mapping alias identifier to the aliased AST nodes.")


(defmethod ast-list= (got expected)
  "Returns true if every element of GOT is equal (by AST=) to the
   corresponding element of EXPECTED."

  (every #'ast= got expected))

(defgeneric ast= (got expected)
  (:documentation
   "Returns true if the AST node GOT is equivalent to the AST node expected."))

(defmethod ast= ((got js-call) (expected js-call))
  (and (ast= (js-call-operator got) (js-call-operator expected))
       (ast-list= (js-call-operands got) (js-call-operands expected))))

(defmethod ast= ((got js-new) (expected js-new))
  (and (ast= (js-call-operator got) (js-call-operator expected))
       (ast-list= (js-call-operands got) (js-call-operands expected))))


(defmethod ast= ((got js-element) (expected js-element))
  (and (ast= (js-element-object got) (js-element-object expected))
       (ast= (js-element-element got) (js-element-element expected))))

(defmethod ast= ((got js-member) (expected js-member))
  (and (ast= (js-member-object got) (js-member-object expected))
       (ast= (js-member-field got) (js-member-field expected))))

(defmethod ast= ((got js-string) (expected js-string))
  (string= (js-string-string got) (js-string-string expected)))

(defmethod ast= ((got js-object) (expected js-object))
  (flet ((field= (got expected)
           (match* (got expected)
             (((list g-field g-value)
               (list e-field e-value))

              (and (ast= g-field e-field)
                   (ast= g-value e-value))))))
    (every #'field= (js-object-fields got) (js-object-fields expected))))

(defmethod ast= ((got js-array) (expected js-array))
  (ast-list= (js-array-elements got) (js-array-elements expected)))


(defmethod ast= ((got js-if) (expected js-if))
  (and (ast= (js-if-condition got) (js-if-condition expected))
       (ast= (js-if-then got) (js-if-then expected))
       (ast= (js-if-else got) (js-if-else expected))))

(defmethod ast= ((got js-while) (expected js-while))
  (and (ast= (js-while-condition got) (js-while-condition expected))
       (ast= (js-while-body got) (js-while-body expected))))

(defmethod ast= ((got js-function) (expected js-function))
  (and (equal (js-function-name got) (js-function-name expected))
       (ast-list= (js-function-arguments got) (js-function-arguments expected))
       (ast-list= (js-function-statements got) (js-function-statements expected))))

(defmethod ast= ((got js-block) (expected js-block))
  (ast-list= (js-block-statements got) (js-block-statements expected)))


(defmethod ast= ((got js-var) (expected js-var))
  (and (ast= (js-var-var got) (js-var-var expected))
       (ast= (js-var-value got) (js-var-value expected))))

(defmethod ast= ((got js-return) (expected js-return))
  (ast= (js-return-value got) (js-return-value expected)))

(defmethod ast= ((got js-continue) (expected js-continue))
  t)

(defmethod ast= ((got js-throw) (expected js-throw))
  (ast= (js-throw-expression got) (js-throw-expression expected)))


(defmethod ast= (got (expected list))
  (match expected
    ((list 'd dependency)
     (ast= got (js-element *values-var* (dependency-index *current-context* dependency))))

    ((list '$ alias)
     (acond
       ((gethash alias *ast-aliases*)
        (ast= got it))

       (t (setf (gethash alias *ast-aliases*) got)
          t)))

    (_ (call-next-method))))

(defmethod ast= (got (expected meta-node))
  (ast= got (meta-node-id expected)))

(defmethod ast= (got expected)
  (equal got expected))


(defmacro $ (alias)
  "Convenience macro expanding to `($ ,ALIAS). When this appears in an
   EXPECTED argument to AST= it is interpreted as an alias to the AST
   node in the GOT argument. On the first occurrence an alias, with
   identifier, for the corresponding node in GOT is created and stored
   in *AST-ALIASES*. Subsequent occurrences are replaced with the
   aliased AST node, retrieved from *AST-ALIASES*."

  `'($ ,alias))

(defmacro d (dependency)
  "Convenience macro expanding to `(d ,DEPENDENCY). When this appears
   in an EXPECTED argument to AST= it is replaced with an expression
   that references the dependency node DEPENDENCY."

  `'(d ,dependency))


;;;; Test Mock Utilities

(defmacro mock-backend-state (&body body)
  "Evaluates the forms in BODY in an environment in which the
   backend's state has been initialized with mocked values."

  `(let ((*node-ids* (make-hash-table :test #'eq))
         (*node-link-indices* (make-hash-table :test #'eq))
         (*meta-node-ids* (make-hash-table :test #'eq))
         (*context-ids* (make-hash-table :test #'eq))
         (*lazy-nodes* (make-hash-table :test #'eq))
         (*context-counter* 0)
         (*initial-values* nil))
     ,@body))

(defmacro with-lazy-nodes ((&rest nodes) &body body)
  "Evaluates the forms in BODY with the nodes in NODES set as lazy
   nodes."

  `(let ((*lazy-nodes* (alist-hash-table (list ,@(mapcar #`(cons ,a1 t) nodes)) :test #'eq)))
     ,@body))

(defun mock-meta-nodes% (names)
  "Returns a list of `EXTERNAL-META-NODE's with names NAMES."

  (flet ((make-meta-node (name)
           (make-instance 'external-meta-node :name name)))
    (mapcar #'make-meta-node names)))

(defmacro mock-meta-nodes ((&rest names) &body body)
  "Evaluates the forms in BODY with each symbol in NAMES bound to an
   `EXTERNAL-META-NODE' created by MOCK-META-NODES%."

  `(destructuring-bind ,names (mock-meta-nodes% ',names)
     ,@body))

(defmacro! mock-context ((&rest operands) value-function)
  "Creates a context (the return value of the form) with operands
   OPERANDS and value VALUE-FUNCTION.

   VALUE-FUNCTION is evaluated in an environment in which each symbol
   in OPERANDS is bound to the `NODE-LINK' object of the
   dependency. If an element of OPERANDS is a CONS with the CAR being
   the symbol ASYNC an asynchronous `NODE-LINK' is created and bound
   to the symbol in the CDR."

  (flet ((make-binding (operand)
           (match operand
             ((or (cons 'async dep)
                  dep)

              `(,dep (setf (gethash ',dep (operands ,g!context))
                               (node-link ',operand)))))))

    `(let ((,g!context (make-instance 'node-context)))
       (let ,(mapcar #'make-binding operands)
         (setf (value-function ,g!context) ,value-function))
       ,g!context)))

(defmacro! mock-contexts ((&rest contexts) &body body)
  "Evaluates the forms in BODY with a number of mocked
   `NODE-CONTEXT's. Each element of CONTEXTS is of the form (SYMBOL
   . ARGS) where SYMBOL is the variable to which the context is bound
   and ARGS are the arguments passed to MOCK-CONTEXT."

  (flet ((make-binding (context)
           `(,(first context) (mock-context ,@(rest context)))))
    `(let ,(mapcar #'make-binding contexts) ,@body)))


;;;; Test Value Function Utilities

(defun test-compute-function% (code fn)
  "Tests that the compute function CODE contains the statements in
   FN."

  (is-type! code 'js-function "Is a function")

  (with-accessors ((name js-function-name)
                   (args js-function-arguments)
                   (body js-function-statements))
      code

    (is! name nil "Anonymous function")

    (is! (length args) 1 "Single argument")

    (let ((prove:*default-test-function* #'ast-list=)
          (*values-var* (first args))
          (*ast-aliases* (make-hash-table :test #'eq)))
      (is body fn))))

(defmacro! test-compute-function (o!context &body statements)
  "Tests that the generated compute function, of CONTEXT, contains
   STATEMENTS in its body.

   Some nodes have a special meaning:

   A `META-NODE' is replaced with the name of the function
   implementing the meta-node, obtained by META-NODE-ID.

   A CONS with the CAR being the symbol D references the dependency
   node in the CDR. It is replaced with an expression that references
   the element, at the index corresponding to the dependency index of
   the dependency node, within the values argument to the compute
   function.

   A CONS with the car being the symbol $ creates an alias, with the
   identifier in the CDR, for the corresponding AST node of the
   generated compute function. The first occurrence of ($ a) always
   compares equal to the corresponding AST node of the generated
   function and establishes an establishes an alias, with identifier
   'a', for that node. Subsequent occurrences are replaced with the
   aliased AST node."

  `(let ((*current-context* ,g!context))
     (test-compute-function% (strip-redundant (create-compute-function ,g!context)) (list ,@statements))))


(defmacro promise ((&rest args) &body body)
  "Creates an expression which resolves the expressions in ARGS and
   attaches the 'then' handler function BODY to the promise.

   Each element of args is of the form (EXPR VAR) where EXPR is the
   expression that is to be resolved and VAR is the name of the
   argument, to the 'then' handler, storing the resolved value.

   BODY contains the statements making up the body of the 'then'
   handler function."

  `(promise% (list ,@(mapcar #'first args)) (list ,@(mapcar #'second args)) (list ,@body)))

(defun promise% (exprs args body)
  "Creates a Promise.all expression resolving the expressions
   EXPRS. ARGS are the list of argument variable names storing the
   resolved values of the corresponding expressions. BODY is the list
   of statements making up the body of the 'then' handler function,
   which is attached to the promise object."

  (js-call
   (js-member
    (js-call
     (js-member "Promise" "all")
     (js-array exprs))
    "then")

   (js-lambda (list (js-array args)) body)))


;;;; Tests

(deftest node-functions
  (subtest "Test Node Value Function Code Generation"
    (subtest "Function Calls"
      (mock-backend-state
        (mock-meta-nodes (f)
          (mock-contexts
              ((context (a b) `(,f ,a ,b)))

            (test-compute-function context
              (js-return (js-call f (d a) (d b)))))))

      (subtest "Lazy Dependencies"
        (mock-backend-state
          (mock-meta-nodes (f)
            (mock-contexts
                ((context ((async . a) b) `(,f ,a ,b)))

              (test-compute-function context
                (js-return
                 (js-call
                  (js-member

                   (js-call
                    (js-member "Promise" "all")
                    (js-array (list (js-call (d a)))))

                   "then")

                  (js-lambda
                   (list (js-array (list ($ a))))
                   (list
                    (js-return (js-call f ($ a) (d b))))))))))))

      (subtest "Lazy Nodes"
        (mock-backend-state
          (mock-meta-nodes (f)
            (mock-contexts
                ((context (a b) `(,f ,a ,b)))

              (with-lazy-nodes (context)
                (test-compute-function context
                  (js-return
                   (js-call
                    +thunk-class+
                    (js-lambda
                     nil
                     (list
                      (js-return (js-call f (d a) (d b))))))))))))))

    (subtest "Conditionals"
      (subtest "Simple If Statements"
        (mock-backend-state
          (mock-meta-nodes (< -)
            (mock-contexts
                ((context (a b) `(if (,< ,a ,b) (,- ,b ,a) (,- ,a ,b))))

              (test-compute-function context
                (js-if (js-call < (d a) (d b))
                       (js-return (js-call - (d b) (d a)))
                       (js-return (js-call - (d a) (d b))))))))

        (subtest "Lazy Dependencies"
          (mock-backend-state
            (mock-meta-nodes (< -)
              (mock-contexts
                  ((context ((async . a) (async . b)) `(if (,< ,a ,b) (,- ,b ,a) (,- ,a ,b))))

                (test-compute-function context
                  (js-return
                   (js-call
                    (js-member
                     (promise (((js-call (d a)) ($ a1))
                               ((js-call (d b)) ($ b1)))
                       (js-return (js-call < ($ a1) ($ b1))))
                     "then")

                    (js-lambda
                     (list ($ a<b))
                     (list
                      (js-if ($ a<b)
                             (js-return
                              (promise (((js-call (d b)) ($ b2))
                                        ((js-call (d a)) ($ a2)))
                                (js-return (js-call - ($ b2) ($ a2)))))

                             (js-return
                              (promise (((js-call (d a)) ($ a3))
                                        ((js-call (d b)) ($ b3)))
                                (js-return (js-call - ($ a3) ($ b3))))))))))))))))

      (subtest "Nested If Statements"
        (subtest "Function Call Arguments"
          (mock-backend-state
            (mock-meta-nodes (f < +)
              (mock-contexts
                  ((context (a b c d)
                            `(,f (if (,< ,a 3) (,+ ,b ,c) ,d) ,a)))

                (test-compute-function context
                  (js-var ($ arg1))
                  (js-if (js-call < (d a) 3)
                         (js-call "=" ($ arg1) (js-call + (d b) (d c)))
                         (js-call "=" ($ arg1) (d d)))

                  (js-return (js-call f ($ arg1) (d a)))))))

          (subtest "Lazy Dependencies"
            (mock-backend-state
              (mock-meta-nodes (f < +)
                (mock-contexts
                    ((context ((async . a) b c (async . d))
                              `(,f (if (,< ,a 3) (,+ ,b ,c) ,d) ,a)))

                  (test-compute-function context
                    (-<>
                     (js-call
                      (js-member
                       (promise (((js-call (d a)) ($ a1)))
                         (js-return (js-call < ($ a1) 3)))

                       "then")

                      (js-lambda
                       '(($ cond))

                       (list
                        (js-if ($ cond)
                               (js-return (js-call + (d b) (d c)))
                               (js-return (js-call (d d)))))))

                     (<> ($ arg1))
                     (<> ((js-call (d a)) ($ a2)))
                     (promise <>
                       (js-return (js-call f ($ arg1) ($ a2))))
                     (js-return))))))))

        (subtest "Nested in If Condition"
          (mock-backend-state
            (mock-meta-nodes (< +)
              (mock-contexts
                  ((context (a b c d e)
                            `(if ,a
                                 (if (if (,< ,b ,c) ,b ,c)
                                     (,+ ,b ,c)
                                     ,d)
                                 ,e)))

                (test-compute-function context
                  (js-if (d a)
                         (js-block
                          (js-var ($ cond))
                          (js-if (js-call < (d b) (d c))
                                 (js-call "=" ($ arg) (d b))
                                 (js-call "=" ($ arg) (d c)))

                          (js-if ($ cond)
                                 (js-return (js-call + (d b) (d c)))
                                 (js-return (d d))))
                         (js-return (d e)))))))

          (subtest "Lazy Dependencies"
            (mock-backend-state
              (mock-meta-nodes (< +)
                (mock-contexts
                    ((context (a (async . b) c (async . d) e)
                              `(if ,a
                                   (if (if (,< ,b ,c) ,b ,c)
                                       (,+ ,b ,c)
                                       ,d)
                                   ,e)))

                  (test-compute-function context
                    (js-if
                     (d a)

                     (-<>
                      (js-call
                       (js-member
                        (promise (((js-call (d b)) ($ b1)))
                          (js-return (js-call < ($ b1) (d c))))
                        "then")

                       (js-lambda '(($ cond1))
                                  (list
                                   (js-if ($ cond1)
                                          (js-return (js-call (d b)))
                                          (js-return (d c))))))

                      (js-member "then")

                      (js-call
                       (js-lambda
                        '(($ cond2))

                        (list
                         (js-if ($ cond2)
                                (js-return
                                 (promise (((js-call (d b)) ($ b2)))
                                   (js-return (js-call + ($ b2) (d c)))))
                                (js-return (js-call (d d)))))))

                      (js-return))

                     (js-return (d e))))))))))

      (subtest "Old Value References"
        (mock-backend-state
          (mock-contexts
              ((context (a cond)
                        `(if ,cond ,a ,(node-link :self))))

            (test-compute-function context
              (js-var ($ old-value) (js-member "this" "value"))
              (js-if (d cond)
                     (js-return (d a))
                     (js-return ($ old-value))))))

        (subtest "Lazy Node"
          (mock-backend-state
            (mock-contexts
                ((context (a cond)
                          `(if ,cond ,a ,(node-link :self))))

              (with-lazy-nodes (context)
                (test-compute-function context
                  (js-var ($ old-value) (js-member "this" "value"))
                  (js-return
                   (js-call
                    +thunk-class+

                    (js-lambda
                     nil

                     (list
                      (js-if (d cond)
                             (js-return (d a))
                             (js-return (js-call ($ old-value)))))))))))))))

    (subtest "Objects"
      (subtest "Object Creation"
        (subtest "Simple Field Value Expressions"
          (mock-backend-state
            (mock-meta-nodes (+ -)
              (mock-contexts
                  ((context (x y) `(:object (|sum| (,+ ,x ,y))
                                            (|difference| (,- ,x ,y))
                                            (|x| ,x)
                                            (|y| ,y))))

                (test-compute-function context
                  (js-return
                   (js-object
                    (list
                     (list (js-string "sum") (js-call + (d x) (d y)))
                     (list (js-string "difference") (js-call - (d x) (d y)))
                     (list (js-string "x") (d x))
                     (list (js-string "y") (d y)))))))))

          (subtest "Lazy Dependencies"
            (mock-backend-state
              (mock-meta-nodes (+ -)
                (mock-contexts
                    ((context ((async . x) y) `(:object (|sum| (,+ ,x ,y))
                                                        (|difference| (,- ,x ,y))
                                                        (|x| ,x)
                                                        (|y| ,y))))

                  (test-compute-function context
                    (js-return
                     (promise
                         (((promise (((js-call (d x)) ($ x1)))
                             (js-return (js-call + ($ x1) (d y))))
                           ($ sum))

                          ((promise (((js-call (d x)) ($ x2)))
                             (js-return (js-call - ($ x2) (d y))))
                           ($ diff))

                          ((js-call (d x)) ($ x3)))

                       (js-return
                        (js-object
                         (list
                          (list (js-string "sum") ($ sum))
                          (list (js-string "difference") ($ diff))
                          (list (js-string "x") ($ x3))
                          (list (js-string "y") (d y)))))))))))))

        (subtest "If Expressions in Field Values"
          (mock-backend-state
            (mock-meta-nodes (<)
              (mock-contexts
                  ((context (x y) `(:object (|min| (if (,< ,x ,y) ,x ,y))
                                            (|max| (if (,< ,y ,x) ,x ,y)))))

                (test-compute-function context
                  (js-var ($ min))
                  (js-if (js-call < (d x) (d y))
                         (js-call "=" ($ min) (d x))
                         (js-call "=" ($ min) (d y)))

                  (js-var ($ max))
                  (js-if (js-call < (d y) (d x))
                         (js-call "=" ($ max) (d x))
                         (js-call "=" ($ max) (d y)))
                  (js-return
                   (js-object
                    (list
                     (list (js-string "min") ($ min))
                     (list (js-string "max") ($ max)))))))))

          (subtest "Lazy Dependencies"
            (mock-backend-state
              (mock-meta-nodes (<)
                (mock-contexts
                    ((context ((async . x) y) `(:object (|min| (if (,< ,x ,y) ,x ,y))
                                                        (|max| (if (,< ,y ,x) ,x ,y)))))

                  (test-compute-function context
                    (js-return
                     (-<>
                      (((js-call
                         (js-member
                          (promise (((js-call (d x)) ($ x1)))
                            (js-return (js-call < ($ x1) (d y))))
                          "then")

                         (js-lambda
                          '(($ cond-min))

                          (list
                           (js-if ($ cond-min)
                                  (js-return (js-call (d x)))
                                  (js-return (d y))))))
                        ($ min))

                       ((js-call
                         (js-member
                          (promise (((js-call (d x)) ($ x2)))
                            (js-return (js-call < (d y) ($ x2))))
                          "then")

                         (js-lambda
                          '(($ cond-max))

                          (list
                           (js-if ($ cond-max)
                                  (js-return (js-call (d x)))
                                  (js-return (d y))))))
                        ($ max)))

                      (promise <>
                        (js-return
                         (js-object
                          (list
                           (list (js-string "min") ($ min))
                           (list (js-string "max") ($ max)))))))))))))))

      (subtest "Member Access"
        (subtest "Direct Member Access"
          (mock-backend-state
            (mock-meta-nodes (f)
              (mock-contexts
                  ((context (object) `(,f (:member ,object |field|))))

                (test-compute-function context
                  (js-return
                   (js-call f (js-element (d object) (js-string "field"))))))))

          (subtest "Lazy Dependencies"
            (mock-backend-state
              (mock-meta-nodes (f)
                (mock-contexts
                    ((context ((async . object)) `(,f (:member ,object |field|))))

                  (test-compute-function context
                    (js-return
                     (promise
                         (((js-call
                            (js-member (js-call (d object)) "then")

                            (js-lambda
                             '(($ object))

                             (list
                              (js-return (js-element ($ object) (js-string "field"))))))

                           ($ value)))
                       (js-return (js-call f ($ value)))))))))))

        (subtest "Expression Member Access"
          (mock-backend-state
            (mock-meta-nodes (f)
              (mock-contexts
                  ((context (a) `(:member (,f ,a) \x)))

                (test-compute-function context
                  (js-return (js-element (js-call f (d a)) (js-string "x")))))))

          (subtest "Lazy Dependencies"
            (mock-backend-state
              (mock-meta-nodes (f)
                (mock-contexts
                    ((context ((async . a)) `(:member (,f ,a) \x)))

                  (test-compute-function context
                    (js-return
                     (js-call
                      (js-member
                       (promise (((js-call (d a)) ($ a)))
                         (js-return (js-call f ($ a))))
                       "then")

                      (js-lambda
                       '(($ object))

                       (list
                        (js-return (js-element ($ object) (js-string "x")))))))))))))

        (subtest "Member Access of If Expression"
          (mock-backend-state
            (mock-contexts
                ((context (cond a b) `(:member (if ,cond ,a ,b) \z)))

              (test-compute-function context
                (js-var ($ object))
                (js-if (d cond)
                       (js-call "=" ($ object) (d a))
                       (js-call "=" ($ object) (d b)))

                (js-return (js-element ($ object) (js-string "z"))))))

          (subtest "Lazy Dependencies"
            (mock-backend-state
              (mock-contexts
                  ((context (cond a (async . b)) `(:member (if ,cond ,a ,b) \z)))

                (test-compute-function context
                  (js-var ($ object))
                  (js-if (d cond)
                         (js-call "=" ($ object) (d a))
                         (js-call "=" ($ object) (js-call (d b))))

                  (js-return
                   (js-call
                    (js-member
                     (js-call (js-member "Promise" "resolve") ($ object))
                     "then")

                    (js-lambda '(($ arg))
                               (list (js-return (js-element ($ arg) (js-string "z"))))))))))))))))

(run-test 'node-functions)


(defun test-function% (got expected)
  "Tests that the generated code GOT is equal to EXPECTED. The test is
   performed with *AST-ALIASES* bound to a new empty alias table."

  (let ((prove:*default-test-function* #'ast-list=)
        (*ast-aliases* (make-hash-table :test #'eq)))
    (is got expected)))

(defmacro! test-meta-node-function (o!meta-node &body code)
  "Tests that the code generated for the function of META-NODE is
   equal to CODE."

  `(test-function% (strip-redundant (create-meta-node ,g!meta-node)) (list ,@code)))

(deftest meta-node-functions
  (subtest "Test Meta-Node Function Code Generation"
    (subtest "Simple Single Function Meta-Nodes"
      (subtest "Simple Function"
        (with-module-table modules
          (build-source-file #p"./modules/core.trd" modules)
          (build "add(x, y) : x + y")
          (finish-build)

          (with-nodes ((add "add")) modules
            (mock-backend-state
              (test-meta-node-function add
                (js-function
                 (meta-node-id add)
                 '(($ x) ($ y))

                 (list
                  (js-return (js-call "+" ($ x) ($ y))))))))))

      (subtest "Recursive Meta-Nodes"
        (with-module-table modules
          (build-source-file #p"./modules/core.trd" modules)
          (build "fact(n) : case(n < 1 : 1, n * fact(n - 1))")
          (finish-build)

          (with-nodes ((fact "fact")) modules
            (mock-backend-state
              (test-meta-node-function fact
                (js-function
                 (meta-node-id fact)
                 '(($ n))

                 (list
                  (js-if (js-call "<" ($ n) 1)
                         (js-return 1)
                         (js-return (js-call "*" ($ n) (js-call fact (js-call "-" ($ n) 1))))))))))))

      (subtest "Tail Recursive Meta-Nodes"
        (with-module-table modules
          (build-source-file #p"./modules/core.trd" modules)
          (build "fact(n) : { iter(n, acc) : case(n < 1 : acc, iter(n - 1, n * acc)); iter(n, 1) }")
          (finish-build)

          (with-nodes ((fact "fact")) modules
            (with-nodes ((iter "iter")) (definition fact)
              (mock-backend-state
                (test-meta-node-function fact
                  (js-function
                   (meta-node-id fact)
                   '(($ n1))

                   (list
                    (js-function
                     (meta-node-id iter)
                     '(($ n2) ($ acc))

                     (list
                      (js-while
                       "true"
                       (js-if (js-call "<" ($ n2) 1)
                              (js-return ($ acc))
                              (js-block
                               (js-call "="
                                        (js-array '(($ n2) ($ acc)))
                                        (js-array (list (js-call "-" ($ n2) 1) (js-call "*" ($ n2) ($ acc)))))
                               (js-continue))))))

                    (js-return (js-call iter ($ n1) 1))))))))))

      (subtest "Mutually Recursive Meta-Nodes"
        (with-module-table modules
          (build-source-file #p"./modules/core.trd" modules)
          (build "fib(n) : case(n > 1 : fib1(n) + fib2(n), 1)")
          (build "fib1(n) : fib(n - 1)")
          (build "fib2(n) : fib(n - 2)")
          (finish-build)

          (with-nodes ((fib "fib") (fib1 "fib1") (fib2 "fib2")) modules
            (mock-backend-state
              (test-meta-node-function fib
                (js-function
                 (meta-node-id fib)
                 '(($ n))

                 (list
                  (js-if (js-call ">" ($ n) 1)
                         (js-return
                          (js-call "+" (js-call fib1 ($ n)) (js-call fib2 ($ n))))
                         (js-return 1)))))

              (test-meta-node-function fib1
                (js-function
                 (meta-node-id fib1)
                 '(($ n))

                 (list
                  (js-return (js-call fib (js-call "-" ($ n) 1))))))

              (test-meta-node-function fib2
                (js-function
                 (meta-node-id fib2)
                 '(($ n))

                 (list
                  (js-return (js-call fib (js-call "-" ($ n) 2)))))))))))))

(run-test 'meta-node-functions)

(finalize)
