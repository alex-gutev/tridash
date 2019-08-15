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
        :tridash.interface
        :tridash.backend.js.ast

        :tridash.test.util)

  (:shadowing-import-from :generic-cl
                          :emptyp
                          :accumulate
                          :multiply)

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

                :+tridash-namespace+
                :+end-update-class+
                :+thunk-class+
                :+catch-thunk-class+
                :+resolve-function+

                :*node-ids*
                :*node-link-indices*
                :*meta-node-ids*
                :*context-ids*
                :*context-counter*
                :*initial-values*
                :*output-code*

                :make-code-array
                :meta-node-id
                :dependency-index

                :create-node
                :create-compute-function
                :create-meta-node
                :make-meta-node-call

                :strip-redundant
                :output-code)

  (:import-from :tridash.test.builder
                :with-module-table
                :with-modules
                :with-nodes

                :build
                :build-core-module
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


(defgeneric ast= (got expected)
  (:documentation
   "Returns true if the AST node GOT is equivalent to the AST node expected."))

(defun ast-list= (got expected)
  "Returns true if every element of GOT is equal (by AST=) to the
   corresponding element of EXPECTED."

  (and (length= got expected)
       (every #'ast= got expected)))

(defmethod ast= ((got js-call) (expected js-call))
  (and (ast= (js-call-operator got) (js-call-operator expected))
       (ast-list= (js-call-operands got) (js-call-operands expected))))

(defmethod ast= ((got js-new) (expected js-new))
  (and (ast= (js-new-operator got) (js-new-operator expected))
       (ast-list= (js-new-operands got) (js-new-operands expected))))


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

    (and (length= (js-object-fields got) (js-object-fields expected))
         (every #'field= (js-object-fields got) (js-object-fields expected)))))

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

(defmethod ast= ((got js-catch) (expected js-catch))
  (and (ast-list= (js-catch-try got) (js-catch-try expected))
       (ast= (js-catch-var got) (js-catch-var expected))
       (ast-list= (js-catch-catch got) (js-catch-catch expected))))


(defmethod ast= ((got js-var) (expected js-var))
  (and (ast= (js-var-var got) (js-var-var expected))
       (ast= (js-var-value got) (js-var-value expected))))

(defmethod ast= ((got js-return) (expected js-return))
  (ast= (js-return-value got) (js-return-value expected)))

(defmethod ast= ((got js-continue) (expected js-continue))
  t)

(defmethod ast= ((got js-throw) (expected js-throw))
  (ast= (js-throw-expression got) (js-throw-expression expected)))


(defmethod ast= ((got cons) (expected cons))
  (match* (got expected)
    (((cons 'async got) (cons 'async expected))
     (ast= got expected))

    ((_ _) (call-next-method))))

(defmethod ast= (got (expected list))
  (match expected
    ((list 'd dependency)
     (ast= got (js-element *values-var* (dependency-index *current-context* dependency))))

    ((list '$ '_) t)

    ((list '$ alias)
     (acond
       ((get alias *ast-aliases*)
        (ast= got it))

       (t (setf (get alias *ast-aliases*) got)
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

  `(let ((*node-ids* (make-hash-map))
         (*node-link-indices* (make-hash-map))
         (*meta-node-ids* (make-hash-map))
         (*context-ids* (make-hash-map))
         (*context-counter* 0)
         (*initial-values* nil))
     ,@body))

(defun mock-meta-nodes% (names)
  "Returns a list of `EXTERNAL-META-NODE's with names NAMES."

  (labels
      ((make-operand (operand)
         (match operand
           ((or (list 'optional symb value)
                (list 'optional symb))
            (list +optional-argument+ symb value))

           ((list 'rest symb)
            (list +rest-argument+ symb))

           ((list 'outer node)
            (list +outer-node-argument+ node))

           (_ operand)))

       (make-meta-node (opts)
         (match opts
           ((list name operands strict-operands)
            (aprog1
                (make-instance 'meta-node
                               :name name
                               :operands (map #'make-operand operands))
              (setf (attribute :strictness it)
                    (list* 'or strict-operands))))
           (name
            (make-instance 'meta-node :name name)))))
    (map #'make-meta-node names)))

(defmacro mock-meta-nodes ((&rest names) &body body)
  "Evaluates the forms in BODY with each symbol in NAMES bound to an
   `EXTERNAL-META-NODE' created by MOCK-META-NODES%."

  `(destructuring-bind ,(map #'ensure-car names) (mock-meta-nodes% ',names)
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

              `(,dep (setf (get ',dep (operands ,g!context))
                               (node-link ',operand)))))))

    `(let ((,g!context (make-instance 'node-context)))
       (let ,(map #'make-binding operands)
         (setf (value-function ,g!context) ,value-function))
       ,g!context)))

(defmacro! mock-contexts ((&rest contexts) &body body)
  "Evaluates the forms in BODY with a number of mocked
   `NODE-CONTEXT's. Each element of CONTEXTS is of the form (SYMBOL
   . ARGS) where SYMBOL is the variable to which the context is bound
   and ARGS are the arguments passed to MOCK-CONTEXT."

  (flet ((make-binding (context)
           `(,(first context) (mock-context ,@(rest context)))))
    `(let ,(map #'make-binding contexts) ,@body)))


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
          (*ast-aliases* (make-hash-map)))
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


(defun functor (meta-node &rest arguments)
  "Create a `FUNCTOR-EXPRESSION' with META-NODE applied to ARGUMENTS."

  (functor-expression meta-node arguments))

(defmacro object (&rest entries)
  "Create an `OBJECT-EXPRESSION' with entries ENTRIES."

  `(object-expression (list ,@(map #`(list ',(id-symbol (first a1)) ,(second a1)) entries))))

(defun resolve (expression)
  "Creates an expression which resolves the value of EXPRESSION."

  (js-call +resolve-function+ expression))

(defun thunk (&rest statements)
  "Creates a thunk expression with STATEMENTS being the statements
   making up the body of the thunk."

  (js-new +thunk-class+ (list (js-lambda nil statements))))


;;;; Tests

(subtest "Node Value Function Code Generation"
  (subtest "Literals"
    (subtest "Strings"
      (mock-backend-state
        (mock-contexts
            ((context () "hello"))

          (test-compute-function context
            (js-return (js-string "hello"))))))

    (subtest "Symbols"
      (mock-backend-state
        (mock-contexts
            ((context () '|some-key|))

          (test-compute-function context
            (js-return
             (thunk
              (js-return (js-call "Tridash.get_symbol" (js-string "some-key")))))))))

    (subtest "Integers"
      (mock-backend-state
        (mock-contexts
            ((context () 1))

          (test-compute-function context
            (js-return 1)))))

    (subtest "Characters"
      (mock-backend-state
        (mock-contexts
            ((context () #\h))

          (test-compute-function context
            (js-return
             (thunk
              (js-return (js-new "Tridash.Char" (list (js-string "h")))))))))))

  (subtest "Function Calls"
    (subtest "Positional Arguments"
      (mock-backend-state
        (mock-meta-nodes (f)
          (mock-contexts
              ((context (a b) (functor f a b)))

            (test-compute-function context
              (js-return (thunk (js-return (js-call f (d a) (d b))))))))))

    (subtest "Optional Argument with No Default"
      (mock-backend-state
        (mock-meta-nodes (f)
          (mock-contexts
              ((context (a) (functor f a nil)))

            (test-compute-function context
              (js-return
               (thunk
                (js-return
                 (js-call f (d a) (thunk (js-return (js-call "Tridash.fail"))))))))))))

    (subtest "Rest Arguments"
      (mock-backend-state
        (mock-meta-nodes (f)
          (mock-contexts
              ((context (a b c) (functor f a (argument-list (list b c)))))

            (test-compute-function context
              (js-return
               (thunk
                (js-return
                 (js-call f (d a) (thunk (js-return (js-array (list (d b) (d c))))))))))))))

    (subtest "Empty Rest Argument List"
      (mock-backend-state
        (mock-meta-nodes (f)
          (mock-contexts
              ((context (a) (functor f a (argument-list nil))))

            (test-compute-function context
              (js-return
               (thunk
                (js-return
                 (js-call f (d a) (thunk (js-return (js-call "Tridash.Empty")))))))))))))

  (subtest "Conditionals"
    (subtest "Simple If Statements"
      (mock-backend-state
        (mock-meta-nodes (< -)
          (mock-contexts
              ((context (a b)
                        (if-expression (functor < a b)
                                       (functor - b a)
                                       (functor - a b))))

            (test-compute-function context
              (js-return
               (thunk
                (js-if (resolve (js-call < (d a) (d b)))
                       (js-return
                        (thunk (js-return (js-call - (d b) (d a)))))

                       (js-return
                        (thunk (js-return (js-call - (d a) (d b)))))))))))))

    (subtest "Nested If Statements"
      (subtest "Function Call Arguments"
        (subtest "Strict Argument"
          (mock-backend-state
            (mock-meta-nodes ((f (x y) (x)) < +)
              (mock-contexts
                  ((context (a b c d)
                            (-<> (if-expression (functor < a 3)
                                                (functor + b c)
                                                d)
                                 (functor f <> a))))

                (test-compute-function context
                  (js-return
                   (thunk
                    (js-var ($ arg1))
                    (js-catch
                     (list
                      (js-if (resolve (js-call < (d a) 3))
                             (js-call "="
                                      ($ arg1)
                                      (thunk (js-return (js-call + (d b) (d c)))))

                             (js-call "="
                                      ($ arg1)
                                      (d d))))

                     ($ e)
                     (list
                      (js-call "=" ($ arg1) (thunk (js-throw ($ e))))))

                    (js-return (js-call f ($ arg1) (d a))))))))))

        (subtest "Lazy Argument"
          (mock-backend-state
            (mock-meta-nodes (f < +)
              (mock-contexts
                  ((context (a b c d)
                            (-<> (if-expression (functor < a 3)
                                                (functor + b c)
                                                d)
                                 (functor f <> a))))

                (test-compute-function context
                  (js-return
                   (-<>
                    (js-if (resolve (js-call < (d a) 3))
                           (js-return
                            (thunk (js-return (js-call + (d b) (d c)))))

                           (js-return
                            (d d)))
                    thunk
                    (js-call f <> (d a))
                    js-return
                    thunk))))))))

      (subtest "Nested in If Condition"
        (mock-backend-state
          (mock-meta-nodes (< +)
            (mock-contexts
                ((context (a b c d e)
                          (-<> (if-expression (functor < b c) b c)
                               (if-expression (functor + b c) d)
                               (if-expression a <> e))))

              (test-compute-function context
                (js-return
                 (thunk
                  (js-if (resolve (d a))

                         (js-return
                          (thunk
                           (js-var ($ cond))
                           (js-if (resolve (js-call < (d b) (d c)))
                                  (js-call "="
                                           ($ cond)
                                           (d b))

                                  (js-call "="
                                           ($ cond)
                                           (d c)))

                           (js-if (resolve ($ cond))
                                  (js-return
                                   (thunk (js-return (js-call + (d b) (d c)))))

                                  (js-return
                                   (d d)))))

                         (js-return (d e)))))))))))

    (subtest "Old Value References"
      (mock-backend-state
        (mock-contexts
            ((context (a cond)
                      (if-expression cond a :self)))

          (test-compute-function context
            (js-var "old_value" (js-members "this" "node" "value"))
            (js-return
             (thunk
              (js-if (resolve (d cond))
                     (js-return (d a))
                     (js-return "old_value")))))))))

  (subtest "Objects"
    (subtest "Object Creation"
      (subtest "Simple Field Value Expressions"
        (mock-backend-state
          (mock-meta-nodes (+ -)
            (mock-contexts
                ((context (x y) (object ("sum" (functor + x y))
                                        ("difference" (functor - x y))
                                        ("x" x)
                                        ("y" y))))

              (test-compute-function context
                (js-return
                 (thunk
                  (js-return
                   (js-object
                    (list
                     (list (js-string "sum") (thunk (js-return (js-call + (d x) (d y)))))
                     (list (js-string "difference") (thunk (js-return (js-call - (d x) (d y)))))
                     (list (js-string "x") (d x))
                     (list (js-string "y") (d y))))))))))))

      (subtest "If Expressions in Field Values"
        (mock-backend-state
          (mock-meta-nodes (<)
            (mock-contexts
                ((context (x y) (object ("min" (if-expression (functor < x y) x y))
                                        ("max" (if-expression (functor < y x) x y)))))

              (test-compute-function context
                (js-return
                 (thunk
                  (js-return
                   (js-object
                    (list
                     (list (js-string "min")
                           (thunk
                            (js-if (resolve (js-call < (d x) (d y)))
                                   (js-return (d x))
                                   (js-return (d y)))))

                     (list (js-string "max")
                           (thunk
                            (js-if (resolve (js-call < (d y) (d x)))
                                   (js-return (d x))
                                   (js-return (d y))))))))))))))))

    (subtest "Member Access"
      (subtest "Direct Member Access"
        (subtest "Strict Argument"
          (mock-backend-state
            (mock-meta-nodes ((f (x) (x)))
              (mock-contexts
                  ((context (object) (functor f (member-expression object '|field|))))

                (test-compute-function context
                  (js-return
                   (-<> (d object)
                        (js-call "Tridash.member" <> (js-string "field"))
                        (js-call f <>)
                        js-return
                        thunk)))))))

        (subtest "Lazy Argument"
          (mock-backend-state
            (mock-meta-nodes (f)
              (mock-contexts
                  ((context (object) (functor f (member-expression object '|field|))))

                (test-compute-function context
                  (js-return
                   (thunk
                    (js-return
                     (js-call f (-<> (d object)
                                     (js-call "Tridash.member" <> (js-string "field"))
                                     js-return
                                     thunk)))))))))))

      (subtest "Expression Member Access"
        (mock-backend-state
          (mock-meta-nodes (f)
            (mock-contexts
                ((context (a) (member-expression (functor f a) '\x)))

              (test-compute-function context
                (js-return
                 (thunk
                  (js-return
                   (js-call "Tridash.member" (js-call f (d a)) (js-string "x"))))))))))

      (subtest "Expression Key"
        (mock-backend-state
          (mock-meta-nodes (f)
            (mock-contexts
                ((context (a) (member-expression a (functor f a))))

              (test-compute-function context
                (js-return
                 (thunk
                  (js-return
                   (js-call "Tridash.member" (d a) (js-call f (d a)))))))))))

      (subtest "Member Access of If Expression"
        (mock-backend-state
          (mock-contexts
              ((context (cond a b) (member-expression (if-expression cond a b) '\z)))

            (test-compute-function context
              (js-return
               (thunk
                (js-var ($ object))
                (js-catch
                 (list
                  (js-if (resolve (d cond))
                         (js-call "=" ($ object) (d a))
                         (js-call "=" ($ object) (d b))))

                 ($ e)
                 (list
                  (js-call "=" ($ object) (thunk (js-throw ($ e))))))

                (js-return
                 (js-call "Tridash.member" ($ object) (js-string "z")))))))))))

  (subtest "Conditional Bindings"
    (subtest "Throwing Fail Exception"
      (mock-backend-state
        (mock-meta-nodes (<)
          (mock-contexts
              ((context (a) (expression-block
                             (if-expression (functor < a 0) a (fail-expression)))))

            (test-compute-function context
              (js-return
               (thunk
                (js-if (resolve (js-call < (d a) 0))
                       (js-return (d a))
                       (js-return
                        (thunk
                         (js-return (js-call "Tridash.fail")))))))))))))

  (subtest "Catch Expressions"
    (mock-backend-state
      (mock-meta-nodes (< + -)
        (mock-contexts
            ((context (a b) (catch-expression
                             (expression-block
                              (if-expression (functor < a b)
                                             (functor + a b)
                                             (fail-expression)))

                             (expression-block (functor - a b)))))

          (test-compute-function context
            (js-return
             (thunk
              (js-var ($ try))
              (js-catch
               (list
                (js-if (resolve (js-call < (d a) (d b)))

                       (-<> (js-call + (d a) (d b))
                            js-return
                            thunk
                            (js-call "=" ($ try) <>))

                       (->> (js-call "Tridash.fail")
                            js-return
                            thunk
                            (js-call "=" ($ try)))))

               ($ e)
               (list
                (js-call "=" ($ try) (thunk (js-throw ($ e))))))

              (js-return
               (js-call "Tridash.make_catch_thunk"
                        ($ try)
                        (thunk
                         (js-return
                          (js-call - (d a) (d b)))))))))))))

  (subtest "Meta-Node References"
    (subtest "Without Outer Nodes"
      (mock-backend-state
        (mock-meta-nodes (map f)
          (mock-contexts
              ((context (a)
                        (functor map (meta-node-ref f) a)))

            (test-compute-function context
              (js-return
               (thunk
                (js-return
                 (js-call map f (d a))))))))))

    (subtest "With Optional Arguments"
      (mock-backend-state
        (mock-meta-nodes (map (1+ (n (optional d)) (n d)))
          (mock-contexts
              ((context (a b)
                        (functor map (meta-node-ref 1+ :optional (list b)) a)))

            (test-compute-function context
              (js-return
               (thunk
                (js-return
                 (js-call
                  map

                  (js-lambda
                   (list ($ n) (js-call "=" ($ d) (d b)))
                   (list
                    (js-return
                     (js-call 1+ ($ a) ($ d)))))

                  (d a))))))))))

    (subtest "With Rest Arguments"
      (mock-backend-state
        (mock-meta-nodes (map (list (x (rest xs)) (x)))
          (mock-contexts
              ((context (a) (functor map (meta-node-ref list) a)))

            (test-compute-function context
              (js-return
               (thunk
                (js-return
                 (js-call
                  map

                  (js-lambda
                   (list ($ x) (js-call "..." ($ xs)))
                   (list
                    (js-if (js-call "===" (js-member ($ xs) "length") 0)
                           (js-call "=" ($ xs) (js-call "Tridash.Empty")))

                    (js-return
                     (js-call list ($ x) ($ xs)))))
                  (d a))))))))))

    (subtest "With Outer Nodes"
      (mock-backend-state
        (mock-meta-nodes (map (f (a (outer x)) (a x)))
          (mock-contexts
              ((context (a b)
                        (functor map (meta-node-ref f :outer-nodes (list b)) a)))

            (test-compute-function context
              (js-return
               (thunk
                (js-return
                 (js-call
                  map
                  (js-lambda
                   '(($ a))
                   (list
                    (js-return
                     (js-call f ($ a) (d b)))))
                  (d a))))))))))

    (subtest "With Optional Rest and Outer Node Operands"
      (mock-backend-state
        (mock-meta-nodes (map (f (a (optional b) (rest c) (outer d)) (a d)))
          (mock-contexts
              ((context (a b)
                        (functor map
                                 (meta-node-ref f :optional (list 1) :outer-nodes (list b))
                                 a)))

            (test-compute-function context
              (js-return
               (thunk
                (js-return
                 (js-call
                  map

                  (js-lambda
                   (list
                    ($ a) (js-call "=" ($ b) 1) (js-call "..." ($ c)))

                   (list
                    (js-if (js-call "===" (js-member ($ c) "length") 0)
                           (js-call "=" ($ c) (js-call "Tridash.Empty")))

                    (js-return
                     (js-call f ($ a) ($ b) ($ c) (d b)))))

                  (d a)))))))))))

  (subtest "Raw Node References"
    (subtest "Raw Meta-Node References"
      (mock-backend-state
        (mock-meta-nodes (f)
          (mock-contexts
              ((context () (node-ref f)))

            (test-compute-function context
              (js-return f)))))))

  (subtest "Invoking Nodes as Meta-Nodes"
    (subtest "As Return Value"
      (mock-backend-state
        (mock-contexts
            ((context (f x) (functor f x)))

          (test-compute-function context
            (js-return
             (thunk
              (js-return
               (js-call (resolve (d f)) (d x)))))))))

    (subtest "As Operand"
      (subtest "Strict Argument"
        (mock-backend-state
          (mock-meta-nodes ((func (x y) (x)))
            (mock-contexts
                ((context (f x y) (functor func (functor f x) y)))

              (test-compute-function context
                (js-return
                 (thunk
                  (js-var ($ f))
                  (js-catch
                   (list
                    (js-call "=" ($ f) (js-call (resolve (d f)) (d x))))

                   ($ e)
                   (list
                    (js-call "=" ($ f) (thunk (js-throw ($ e))))))

                  (js-return
                   (js-call func ($ f) (d y))))))))))

      (subtest "Lazy Argument"
        (mock-backend-state
          (mock-meta-nodes (func)
            (mock-contexts
                ((context (f x y) (functor func (functor f x) y)))

              (test-compute-function context
                (js-return
                 (thunk
                  (-<> (js-call (resolve (d f)) (d x))
                       js-return
                       thunk
                       (js-call func <> (d y))
                       js-return)))))))))

    (subtest "With Expression Operands"
      (mock-backend-state
        (mock-meta-nodes (+)
          (mock-contexts
              ((context (f x y) (functor f (functor + x y))))

            (test-compute-function context
              (js-return
               (thunk
                (->> (js-call + (d x) (d y))
                     js-return
                     thunk
                     (js-call (resolve (d f)))
                     js-return))))))))))


(defun test-function% (got expected)
  "Tests that the generated code GOT is equal to EXPECTED. The test is
   performed with *AST-ALIASES* bound to a new empty alias table."

  (let ((prove:*default-test-function* #'ast-list=)
        (*ast-aliases* (make-hash-map)))
    (is got expected)))

(defun test-meta-node-function% (meta-node expected)
  (let ((*ast-aliases* (make-hash-map)))
    (is (strip-redundant (create-meta-node meta-node)) expected :test #'ast-list=)))

(defmacro test-meta-node-function (meta-node &body code)
  "Tests that the code generated for the function of META-NODE is
   equal to CODE."

  `(test-meta-node-function% ,meta-node (list ,@code)))

(defmacro! test-meta-node-calls ((&rest calls) &body body)
  "Tests that the expected expressions are being generated for
   meta-node calls. This is achieved by replacing the definition of
   the MAKE-META-NODE-CALL, by SHADOW-FUNCTION, function in the
   dynamic extent of the forms in BODY.

   Each element of CALLS is of the form (META-NODE EXPRESSION) where
   META-NODE is the meta-node and EXPRESSION is the expression that is
   expected to be generated for a call to the meta-node. Each time
   MAKE-META-NODE is called, the generated expression for the
   meta-node is compared to the corresponding expected expression. The
   comparison is done using AST=, with the existing binding of
   *AST-ALIASES*."

  (flet ((make-test-call (call)
           (destructuring-bind (meta-node expr) call
             `((eq ,g!meta-node ,meta-node)
               (is it ,expr :test #'ast=)))))
    `(shadow-function
         (make-meta-node-call
          (,g!meta-node ,g!operands)

          (aprog1 (previous ,g!meta-node ,g!operands)
            (cond ,@(map #'make-test-call calls))))
       ,@body)))

(defun protected (&rest expressions)
  "Wraps EXPRESSIONS in a TRY-CATCH block with the CATCH block
   returning a 'failing' thunk."

  (js-catch
   expressions

   ($ e)
   (list
    (js-return
     (thunk (js-throw ($ e)))))))


(subtest "Meta-Node Function Code Generation"
  (subtest "Single Function Meta-Nodes"
    (subtest "Simple Function"
      (with-module-table modules
        (build-core-module)
        (build ":import(core)")
        (build "add(x, y) : x + y"
               ":attribute(add, no-remove, 1)")
        (finish-build)

        (with-nodes ((add "add")) modules
          (mock-backend-state
            (test-meta-node-function add
              (js-function
               (meta-node-id add)
               '(($ x) ($ y))

               (list
                (js-catch
                 (list
                  (js-return
                   (js-call
                    "+"

                    (js-call "Tridash.check_number" (resolve ($ x)))
                    (js-call "Tridash.check_number" (resolve ($ y))))))

                 ($ e)
                 (list
                  (js-return (thunk (js-throw ($ e)))))))))))))

    (subtest "Recursive Meta-Nodes"
      (with-module-table modules
        (build-core-module)
        (build ":import(core)")
        (build "fact(n) : case(n < 1 : 1, n * fact(n - 1))")
        (finish-build)

        (with-nodes ((fact "fact")) modules
          (mock-backend-state
            (test-meta-node-function fact
              (js-function
               (meta-node-id fact)
               '(($ n))

               (list
                (js-catch
                 (list
                  (js-if
                   (resolve
                    (js-call
                     "<"
                     (js-call "Tridash.check_number" (resolve ($ n)))
                     (js-call "Tridash.check_number" (resolve 1))))
                   (js-return 1)

                   (js-return
                    (thunk
                     (js-var ($ n-1))
                     (js-catch
                      (list
                       (->> (js-call
                             "-"
                             (js-call "Tridash.check_number" (resolve ($ n)))
                             (js-call "Tridash.check_number" (resolve 1)))
                            (js-call "=" ($ n-1))))

                      ($ e1)
                      (list
                       (js-call "=" ($ n-1) (thunk (js-throw ($ e1))))))

                     (js-return
                      (js-call
                       "*"
                       (js-call "Tridash.check_number" (resolve ($ n)))
                       (js-call "Tridash.check_number" (resolve (js-call fact ($ n-1))))))))))

                 ($ e2)
                 (list
                  (js-return (thunk (js-throw ($ e2)))))))))))))

    (subtest "Tail Recursive Meta-Nodes"
      (with-module-table modules
        (build-core-module)
        (build ":import(core)")
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
                  (js-return
                   (js-call iter ($ n1) 1)))))

              (test-meta-node-function iter
                (js-function
                 (meta-node-id iter)
                 '(($ n2) ($ acc))

                 (list
                  (js-catch
                   (list
                    (js-if
                     (resolve
                      (js-call
                       "<"
                       (js-call "Tridash.check_number" (resolve ($ n2)))
                       (js-call "Tridash.check_number" (resolve 1))))
                     (js-return ($ acc))

                     (js-return
                      (thunk
                       ;; Compute n - 1
                       (js-var ($ n-1))
                       (js-catch
                        (list
                         (->> (js-call
                               "-"
                               (js-call "Tridash.check_number" (resolve ($ n2)))
                               (js-call "Tridash.check_number" (resolve 1)))
                              (js-call "=" ($ n-1))))

                        ($ e1)
                        (list
                         (js-call "=" ($ n-1) (thunk (js-throw ($ e1))))))

                       ;; Compute n * acc
                       (js-var ($ n*acc))
                       (js-catch
                        (list
                         (->> (js-call
                               "*"
                               (js-call "Tridash.check_number" (resolve ($ n2)))
                               (js-call "Tridash.check_number" (resolve ($ acc))))
                              (js-call "=" ($ n*acc))))

                        ($ e2)
                        (list
                         (js-call "=" ($ n*acc) (thunk (js-throw ($ e2))))))

                       ;; iter(n - 1, n * acc)
                       (js-return
                        (js-call iter ($ n-1) ($ n*acc)))))))

                   ($ e3)
                   (list
                    (js-return (thunk (js-throw ($ e3))))))))))))))

    (subtest "Mutually Recursive Meta-Nodes"
      (with-module-table modules
        (build-core-module)
        (build ":import(core)")
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
                (js-catch
                 (list
                  (js-if
                   (resolve
                    (js-call
                     ">"
                     (js-call "Tridash.check_number" (resolve ($ n)))
                     (js-call "Tridash.check_number" (resolve 1))))

                   (js-return
                    (thunk
                     (js-return
                      (js-call
                       "+"
                       (js-call "Tridash.check_number" (resolve (js-call fib1 ($ n))))
                       (js-call "Tridash.check_number" (resolve (js-call fib2 ($ n))))))))
                   (js-return 1)))

                 ($ e)
                 (list
                  (js-return (thunk (js-throw ($ e)))))))))

            (test-meta-node-function fib1
              (js-function
               (meta-node-id fib1)
               '(($ n))

               (list
                ;; Compute n - 1
                (js-var ($ arg))
                (js-catch
                 (list
                  (->> (js-call
                        "-"
                        (js-call "Tridash.check_number" (resolve ($ n)))
                        (js-call "Tridash.check_number" (resolve 1)))
                       (js-call "=" ($ arg))))

                 ($ e)
                 (list
                  (js-call "=" ($ arg) (thunk (js-throw ($ e))))))

                ;; Compute fib(n - 1)
                (js-return
                 (js-call fib ($ arg))))))

            (test-meta-node-function fib2
              (js-function
               (meta-node-id fib2)
               '(($ n))

               (list
                ;; Compute n - 2
                (js-var ($ arg))
                (js-catch
                 (list
                  (->> (js-call
                        "-"
                        (js-call "Tridash.check_number" (resolve ($ n)))
                        (js-call "Tridash.check_number" (resolve 2)))
                       (js-call "=" ($ arg))))

                 ($ e)
                 (list
                  (js-call "=" ($ arg) (thunk (js-throw ($ e))))))

                ;; Compute fib(n - 2)
                (js-return
                 (js-call fib ($ arg))))))))))

    (subtest "Fail Expressions"
      (with-module-table modules
        (build-core-module)
        (build ":import(core)")
        (build "validate(x) : x > 0 -> (x -> self)")
        (finish-build)

        (with-nodes ((validate "validate")) modules
          (mock-backend-state
            (test-meta-node-function validate
              (js-function
               (meta-node-id validate)
               '(($ x))

               (list
                (js-catch
                 (list
                  (js-if (resolve
                          (js-call
                           ">"
                           (js-call "Tridash.check_number" (resolve ($ x)))
                           (js-call "Tridash.check_number" (resolve 0))))

                         (js-return ($ x))
                         (js-return
                          (thunk
                           (js-return
                            (js-call "Tridash.fail"))))))

                 ($ e)
                 (list
                  (js-return
                   (thunk (js-throw ($ e)))))))))))))

    (subtest "Common Sub Expressions"
      (with-module-table modules
        (build-core-module)
        (build ":import(core)")
        (build "func(a, b) : (a + b) - (a + b)"
               ":attribute(func, no-remove, 1)")

        (with-nodes ((func "func")) (finish-build modules)

          (mock-backend-state
            (test-meta-node-function func
              (js-function
               (meta-node-id func)
               '(($ a) ($ b))

               (list
                (js-var ($ g1))
                (->> (js-call
                      "+"
                      (js-call "Tridash.check_number" (resolve ($ a)))
                      (js-call "Tridash.check_number" (resolve ($ b))))

                     js-return
                     thunk
                     (js-call "=" ($ g1)))

                (js-catch
                 (list
                  (js-return
                   (js-call
                    "-"
                    (js-call "Tridash.check_number" (resolve ($ g1)))
                    (js-call "Tridash.check_number" (resolve ($ g1))))))

                 ($ e)
                 (list
                  (js-return
                   (thunk (js-throw ($ e)))))))))))))

    (subtest "Optional Arguments"
      (with-module-table modules
        (build-core-module)
        (build ":import(core)")
        (build "1+(n, d : 1) : n + d")
        (build "f(a) : 1+(a)"
               ":attribute(f, no-remove, 1)")

        (with-nodes ((1+ "1+") (f "f"))
            (finish-build)

          (mock-backend-state
            (test-meta-node-function 1+
              (js-function
               (meta-node-id 1+)
               '(($ n) ($ d))

               (list
                (protected
                 (js-return
                  (js-call
                   "+"
                   (js-call "Tridash.check_number" (resolve ($ n)))
                   (js-call "Tridash.check_number" (resolve ($ d))))))))))

          (mock-backend-state
            (test-meta-node-function f
              (js-function
               (meta-node-id f)
               '(($ n))

               (list
                (js-return (js-call 1+ ($ n) 1)))))))))

    (subtest "Rest Arguments"
      (with-module-table modules
        (build-core-module)
        (build ":import(core)")
        (build "f(x, ..(xs)) : x + xs")
        (build "g(x, y, z) : f(x, y, z)")
        (build "h(x) : f(x)"

               ":attribute(h, no-remove, 1)"
               ":attribute(g, no-remove, 1)")

        (with-nodes ((f "f") (g "g") (h "h"))
            (finish-build)

          (mock-backend-state
            (test-meta-node-function f
              (js-function
               (meta-node-id f)
               '(($ x) ($ xs))

               (list
                (protected
                 (js-return
                  (js-call
                   "+"
                   (js-call "Tridash.check_number" (resolve ($ x)))
                   (js-call "Tridash.check_number" (resolve ($ xs))))))))))

          (mock-backend-state
            (test-meta-node-function g
              (js-function
               (meta-node-id g)
               '(($ x) ($ y) ($ z))

               (list
                (js-return
                 (js-call f ($ x) (js-array (list ($ y) ($ z)))))))))

          (mock-backend-state
            (test-meta-node-function h
              (js-function
               (meta-node-id h)
               '(($ x))

               (list
                (js-return
                 (js-call f ($ x) (js-call "Tridash.Empty"))))))))))

    (subtest "Primitive Functions"
      (subtest "Arithmetic"
        (subtest "Add"
          (with-module-table modules
            (build-core-module)
            (build ":import(core)")
            (build "add(x, y) : x + y")
            (finish-build)

            (with-nodes ((add "add")) modules
              (mock-backend-state
                (test-meta-node-function add
                  (js-function
                   (meta-node-id add)
                   '(($ x) ($ y))

                   (list
                    (protected
                     (js-return
                      (js-call
                       "+"

                       (js-call "Tridash.check_number" (resolve ($ x)))
                       (js-call "Tridash.check_number" (resolve ($ y)))))))))))))

        (subtest "Subtract"
          (with-module-table modules
            (build-core-module)
            (build ":import(core)")
            (build "sub(x, y) : x - y")
            (finish-build)

            (with-nodes ((sub "sub")) modules
              (mock-backend-state
                (test-meta-node-function sub
                  (js-function
                   (meta-node-id sub)
                   '(($ x) ($ y))

                   (list
                    (protected
                     (js-return
                      (js-call
                       "-"

                       (js-call "Tridash.check_number" (resolve ($ x)))
                       (js-call "Tridash.check_number" (resolve ($ y)))))))))))))

        (subtest "Negate"
          (with-module-table modules
            (build-core-module)
            (build ":import(core)")
            (build "neg(x) : -(x)")
            (finish-build)

            (with-nodes ((neg "neg")) modules
              (mock-backend-state
                (test-meta-node-function neg
                  (js-function
                   (meta-node-id neg)
                   '(($ x))

                   (list
                    (protected
                     (js-return
                      (js-call
                       "-"

                       (js-call "Tridash.check_number" (resolve ($ x)))))))))))))

        (subtest "Multiply"
          (with-module-table modules
            (build-core-module)
            (build ":import(core)")
            (build "mul(x, y) : x * y")
            (finish-build)

            (with-nodes ((mul "mul")) modules
              (mock-backend-state
                (test-meta-node-function mul
                  (js-function
                   (meta-node-id mul)
                   '(($ x) ($ y))

                   (list
                    (protected
                     (js-return
                      (js-call
                       "*"
                       (js-call "Tridash.check_number" (resolve ($ x)))
                       (js-call "Tridash.check_number" (resolve ($ y)))))))))))))

        (subtest "Divide"
          (with-module-table modules
            (build-core-module)
            (build ":import(core)")
            (build "div(x, y) : x / y")
            (finish-build)

            (with-nodes ((div "div")) modules
              (mock-backend-state
                (test-meta-node-function div
                  (js-function
                   (meta-node-id div)
                   '(($ x) ($ y))

                   (list
                    (protected
                     (js-return
                      (js-call
                       "/"
                       (js-call "Tridash.check_number" (resolve ($ x)))
                       (js-call "Tridash.check_number" (resolve ($ y))))))))))))))

      (subtest "Comparison"
        (subtest "Less Than"
          (with-module-table modules
            (build-core-module)
            (build ":import(core)")
            (build "less?(x, y) : x < y")
            (finish-build)

            (with-nodes ((less? "less?")) modules
              (mock-backend-state
                (test-meta-node-function less?
                  (js-function
                   (meta-node-id less?)
                   '(($ x) ($ y))

                   (list
                    (protected
                     (js-return
                      (js-call
                       "<"

                       (js-call "Tridash.check_number" (resolve ($ x)))
                       (js-call "Tridash.check_number" (resolve ($ y)))))))))))))

        (subtest "Less Than or Equal"
          (with-module-table modules
            (build-core-module)
            (build ":import(core)")
            (build "less=?(x, y) : x <= y")
            (finish-build)

            (with-nodes ((less=? "less=?")) modules
              (mock-backend-state
                (test-meta-node-function less=?
                  (js-function
                   (meta-node-id less=?)
                   '(($ x) ($ y))

                   (list
                    (protected
                     (js-return
                      (js-call
                       "<="

                       (js-call "Tridash.check_number" (resolve ($ x)))
                       (js-call "Tridash.check_number" (resolve ($ y)))))))))))))

        (subtest "Greater Than"
          (with-module-table modules
            (build-core-module)
            (build ":import(core)")
            (build "greater?(x, y) : x > y")
            (finish-build)

            (with-nodes ((greater? "greater?")) modules
              (mock-backend-state
                (test-meta-node-function greater?
                  (js-function
                   (meta-node-id greater?)
                   '(($ x) ($ y))

                   (list
                    (protected
                     (js-return
                      (js-call
                       ">"

                       (js-call "Tridash.check_number" (resolve ($ x)))
                       (js-call "Tridash.check_number" (resolve ($ y)))))))))))))

        (subtest "Greater Than or Equal"
          (with-module-table modules
            (build-core-module)
            (build ":import(core)")
            (build "greater=?(x, y) : x >= y")
            (finish-build)

            (with-nodes ((greater=? "greater=?")) modules
              (mock-backend-state
                (test-meta-node-function greater=?
                  (js-function
                   (meta-node-id greater=?)
                   '(($ x) ($ y))

                   (list
                    (protected
                     (js-return
                      (js-call
                       ">="

                       (js-call "Tridash.check_number" (resolve ($ x)))
                       (js-call "Tridash.check_number" (resolve ($ y)))))))))))))

        (subtest "Equal"
          (with-module-table modules
            (build-core-module)
            (build ":import(core)")
            (build "equal?(x, y) : x = y")
            (finish-build)

            (with-nodes ((equal? "equal?")) modules
              (mock-backend-state
                (test-meta-node-function equal?
                  (js-function
                   (meta-node-id equal?)
                   '(($ x) ($ y))

                   (list
                    (protected
                     (js-return
                      (js-call
                       "==="

                       (resolve ($ x))
                       (resolve ($ y))))))))))))

        (subtest "Not Equal"
          (with-module-table modules
            (build-core-module)
            (build ":import(core)")
            (build "not-equal?(x, y) : x != y")
            (finish-build)

            (with-nodes ((not-equal? "not-equal?")) modules
              (mock-backend-state
                (test-meta-node-function not-equal?
                  (js-function
                   (meta-node-id not-equal?)
                   '(($ x) ($ y))

                   (list
                    (protected
                     (js-return
                      (js-call
                       "!=="

                       (resolve ($ x))
                       (resolve ($ y)))))))))))))

      (subtest "Boolean Logic"
        (subtest "And"
          (with-module-table modules
            (build-core-module)
            (build ":import(core)")
            (build "my-and(x, y) : x + y and y - x")
            (finish-build)

            (with-nodes ((my-and "my-and")) modules
              (mock-backend-state
                (test-meta-node-function my-and
                  (js-function
                   (meta-node-id my-and)
                   '(($ x) ($ y))

                   (list
                    (js-var ($ arg))
                    (js-catch
                     (list
                      (->> (js-call
                            "+"

                            (js-call "Tridash.check_number" (resolve ($ x)))
                            (js-call "Tridash.check_number" (resolve ($ y))))

                           (js-call "=" ($ arg))))

                     ($ e)
                     (list
                      (js-call "=" ($ arg) (thunk (js-throw ($ e))))))

                    (js-return
                     (js-call "Tridash.and"
                              ($ arg)
                              (thunk
                               (js-return
                                (js-call
                                 "-"

                                 (js-call "Tridash.check_number" (resolve ($ y)))
                                 (js-call "Tridash.check_number" (resolve ($ x)))))))))))))))

        (subtest "Or"
          (with-module-table modules
            (build-core-module)
            (build ":import(core)")
            (build "my-or(x, y) : x + y or y - x")
            (finish-build)

            (with-nodes ((my-and "my-or")) modules
              (mock-backend-state
                (test-meta-node-function my-and
                  (js-function
                   (meta-node-id my-and)
                   '(($ x) ($ y))

                   (list
                    (js-var ($ arg))
                    (js-catch
                     (list
                      (->> (js-call
                            "+"
                            (js-call "Tridash.check_number" (resolve ($ x)))
                            (js-call "Tridash.check_number" (resolve ($ y))))

                           (js-call "=" ($ arg))))

                     ($ e)
                     (list
                      (js-call "=" ($ arg) (thunk (js-throw ($ e))))))

                    (js-return
                     (js-call "Tridash.or"
                              ($ arg)
                              (thunk
                               (js-return
                                (js-call
                                 "-"
                                 (js-call "Tridash.check_number" (resolve ($ y)))
                                 (js-call "Tridash.check_number" (resolve ($ x)))))))))))))))

        (subtest "Not"
          (with-module-table modules
            (build-core-module)
            (build ":import(core)")
            (build "my-not(x) : not(x)")
            (finish-build)

            (with-nodes ((my-and "my-not")) modules
              (mock-backend-state
                (test-meta-node-function my-and
                  (js-function
                   (meta-node-id my-and)
                   '(($ x))

                   (list
                    (protected
                     (js-return (js-call "!" (resolve ($ x)))))))))))))

      (subtest "Type Conversions"
        (subtest "int"
          (with-module-table modules
            (build-core-module)
            (build ":import(core)")
            (build "my-int(x) : int(x)")
            (finish-build)

            (with-nodes ((my-int "my-int")) modules
              (mock-backend-state
                (test-meta-node-function my-int
                  (js-function
                   (meta-node-id my-int)
                   '(($ x))

                   (list
                    (js-return
                     (js-call "Tridash.cast_int" ($ x))))))))))

        (subtest "real"
          (with-module-table modules
            (build-core-module)
            (build ":import(core)")
            (build "my-real(x) : real(x)")
            (finish-build)

            (with-nodes ((my-real "my-real")) modules
              (mock-backend-state
                (test-meta-node-function my-real
                  (js-function
                   (meta-node-id my-real)
                   '(($ x))

                   (list
                    (js-return
                     (js-call "Tridash.cast_real" ($ x))))))))))

        (subtest "string"
          (with-module-table modules
            (build-core-module)
            (build ":import(core)")
            (build "my-string(x) : string(x)")
            (finish-build)

            (with-nodes ((my-string "my-string")) modules
              (mock-backend-state
                (test-meta-node-function my-string
                  (js-function
                   (meta-node-id my-string)
                   '(($ x))

                   (list
                    (js-return
                     (js-call "Tridash.cast_string" ($ x)))))))))))

      (subtest "Type Checks"
        (subtest "int"
          (with-module-table modules
            (build-core-module)
            (build ":import(core)")
            (build "is-int?(x) : int?(x)")
            (finish-build)

            (with-nodes ((is-int? "is-int?")) modules
              (mock-backend-state
                (test-meta-node-function is-int?
                  (js-function
                   (meta-node-id is-int?)
                   '(($ x))

                   (list
                    (js-return
                     (js-call "Tridash.is_int" ($ x))))))))))

        (subtest "real"
          (with-module-table modules
            (build-core-module)
            (build ":import(core)")
            (build "is-real?(x) : real?(x)")
            (finish-build)

            (with-nodes ((is-real? "is-real?")) modules
              (mock-backend-state
                (test-meta-node-function is-real?
                  (js-function
                   (meta-node-id is-real?)
                   '(($ x))

                   (list
                    (js-return
                     (js-call "Tridash.is_real" ($ x))))))))))

        (subtest "string"
          (with-module-table modules
            (build-core-module)
            (build ":import(core)")
            (build "is-string?(x) : string?(x)")
            (finish-build)

            (with-nodes ((is-string? "is-string?")) modules
              (mock-backend-state
                (test-meta-node-function is-string?
                  (js-function
                   (meta-node-id is-string?)
                   '(($ x))

                   (list
                    (js-return
                     (js-call "Tridash.is_string" ($ x))))))))))

        (subtest "Is Infinity"
          (with-module-table modules
            (build-core-module)
            (build ":import(core)")
            (build "is-inf?(x) : inf?(x)")
            (finish-build)

            (with-nodes ((is-inf? "is-inf?")) modules
              (mock-backend-state
                (test-meta-node-function is-inf?
                  (js-function
                   (meta-node-id is-inf?)
                   '(($ x))

                   (list
                    (js-return
                     (js-call "Tridash.is_inf" ($ x))))))))))

        (subtest "Is NaN"
          (with-module-table modules
            (build-core-module)
            (build ":import(core)")
            (build "is-nan?(x) : NaN?(x)")
            (finish-build)

            (with-nodes ((is-nan? "is-nan?")) modules
              (mock-backend-state
                (test-meta-node-function is-nan?
                  (js-function
                   (meta-node-id is-nan?)
                   '(($ x))

                   (list
                    (js-return
                     (js-call "Tridash.is_nan" ($ x))))))))))))))

(finalize)
