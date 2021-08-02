;;;; backend.lisp
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

;;;; JavaScript Backend Tests

(defpackage :tridash/test.backend.js
  (:use :generic-cl
        :alexandria
        :anaphora
        :arrows
        :iterate
        :optima
        :named-readtables

        :tridash.parser
        :tridash.frontend
        :tridash.interface
        :tridash.backend.js.ast

        :fiveam
        :tridash/test
        :tridash/test.util)

  (:shadowing-import-from :generic-cl
                          :emptyp
                          :accumulate
                          :multiply)

  (:shadowing-import-from :fiveam :fail)

  (:import-from :lol
                :defmacro!
                :lol-syntax
                :mkstr)

  (:import-from :tridash
                :*module-search-paths*
                :build-program)

  (:import-from :tridash.frontend
                :build-source-file)

  (:import-from :tridash.backend.js

                :+tridash-namespace+
                :+thunk-class+
                :+resolve-function+

                :js-backend-state
                :*backend-state*
                :meta-node-id

                :make-value-block
                :compile-function
                :create-meta-node
                :make-meta-node-call

                :strip-redundant)

  (:local-nicknames (:ir :tridash.ir)))


(in-package :tridash/test.backend.js)

(in-readtable lol-syntax)


;;; Test Suite Definition

(def-suite javascript-backend
    :description "Test JavaScript Backend."
    :in backend)

(in-suite javascript-backend)


;;; AST Equality Comparison

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

(defmethod ast= ((got js-switch) (expected js-switch))
  (flet ((case= (got expected)
           (and (ast= (first got) (first expected))
                (ast-list= (second got) (second expected)))))

    (and (ast= (js-switch-value got) (js-switch-value expected))
         (= (length (js-switch-cases got) (js-switch-cases expected)))
         (every #'case= (js-switch-cases got) (js-switch-cases expected)))))


(defmethod ast= ((got js-var) (expected js-var))
  (and (ast= (js-var-var got) (js-var-var expected))
       (ast= (js-var-value got) (js-var-value expected))))

(defmethod ast= ((got js-return) (expected js-return))
  (ast= (js-return-value got) (js-return-value expected)))

(defmethod ast= ((got js-continue) (expected js-continue))
  t)

(defmethod ast= ((got js-break) (expected js-break))
  t)

(defmethod ast= ((got js-throw) (expected js-throw))
  (ast= (js-throw-expression got) (js-throw-expression expected)))


(defmethod ast= (got (expected list))
  (match expected
    ((list 'd _)
     (ast= got (get expected *ast-aliases*)))

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


;;; Test Mock Utilities

(defmacro mock-backend-state (&body body)
  "Evaluates the forms in BODY in an environment in which the
   backend's state has been initialized with mocked values."

  `(let ((*backend-state* (make-instance 'js-backend-state)))
     ,@body))

(defun mock-meta-nodes% (names)
  "Returns a list of `EXTERNAL-META-NODE's with names NAMES."

  (labels
      ((split-args (args)
         (aif (position :outer args)
              (values (subseq args 0 it) (subseq args (1+ it)))
              (values args nil)))

       (make-operand (operand)
         (match operand
           ((or (list 'optional symb value)
                (list 'optional symb))
            (list +optional-argument+ (make-instance 'node :name symb) value))

           ((list 'rest symb)
            (list +rest-argument+ (make-instance 'node :name symb)))

           (_ (make-instance 'node :name operand))))

       (make-outer-operand (operand)
         (cons operand (make-instance 'node :name operand)))

       (make-meta-node (opts)
         (match opts
           ((list name operands strict-operands)

            (multiple-value-bind (operands outer-nodes)
                (split-args operands)

              (aprog1
                  (make-instance 'final-meta-node
                                 :name name
                                 :operands (map #'make-operand operands))

                (setf (outer-nodes it)
                      (map-to 'hash-map #'make-outer-operand outer-nodes))

                (setf (attribute :strictness it)
                      (list* 'or strict-operands)))))
           (name
            (make-instance 'final-meta-node :name name)))))
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
   in OPERANDS is bound to the `IR:LINK' object of the
   dependency. If an element of OPERANDS is a CONS with the CAR being
   the symbol ASYNC an asynchronous `IR:LINK' is created and bound
   to the symbol in the CDR."

  (flet ((make-binding (operand)
           (match operand
             ((or (cons 'async dep)
                  dep)

              `(,dep (setf (get ',dep (operands ,g!context))
                               (ir:link ',operand)))))))

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


;;; Test Value Function Utilities

(defun test-context-function (context &rest expected)
  "Tests that the function generated for context consists of the
   statements EXPECTED

   Some nodes have a special meaning:

   A `META-NODE' is replaced with the name of the function
   implementing the meta-node, obtained by META-NODE-ID.

   A CONS with the CAR being the symbol D references the dependency
   node in the CDR. It is replaced with an expression that references
   the value of the dependency node.

   A CONS with the car being the symbol $ creates an alias, with the
   identifier in the CDR, for the corresponding AST node of the
   generated compute function. The first occurrence of ($ a) always
   compares equal to the corresponding AST node of the generated
   function and establishes an establishes an alias, with identifier
   'a', for that node. Subsequent occurrences are replaced with the
   aliased AST node."

  (let ((operands (make-hash-map))
        (*ast-aliases* (make-hash-map))
        (counter 0))

    (flet ((get-input (link)
             (ematch link
               ((ir:link (ir:node node))
                (ensure-get node operands
                  (aprog1
                      (make-value-block :expression (mkstr "dep" counter))

                    (setf (get (list 'd node) *ast-aliases*) (mkstr "dep" counter))
                    (incf counter)))))))

      (let ((code
             (-> (value-function context)
                 (compile-function #'get-input :protect nil)
                 strip-redundant)))

        (is (ast-list= code expected)
            "Expected:~%~%~a~%~%~2TGot:~%~%~a"
            expected code)))))


(defun functor (meta-node &rest arguments)
  "Create a `FUNCTOR-EXPRESSION' with META-NODE applied to ARGUMENTS."

  (ir:functor meta-node arguments))

(defmacro object (&rest entries)
  "Create an `IR:OBJECT' with entries ENTRIES."

  `(ir:object (list ,@(map #`(list ',(id-symbol (first a1)) ,(second a1)) entries))))

(defun resolve (expression)
  "Creates an expression which resolves the value of EXPRESSION."

  (js-call +resolve-function+ expression))

(defun thunk (&rest statements)
  "Creates a thunk expression with STATEMENTS being the statements
   making up the body of the thunk."

  (js-new +thunk-class+ (list (js-lambda nil statements))))


;;; Test Compiling Expressions

(test expression-string
  "Test compiling literal strings"

  (mock-backend-state
    (mock-contexts
        ((context () "hello"))

      (test-context-function
       context

       (js-return (js-string "hello"))))))

(test expression-symbol
  "Test compiling literal symbols"

  (mock-backend-state
    (mock-contexts
        ((context () '|some-key|))

      (test-context-function
       context
       (js-return (js-call "Tridash.get_symbol" (js-string "some-key")))))))

(test expression-integer
  "Test compiling literal integers"

  (mock-backend-state
    (mock-contexts
        ((context () 1))

      (test-context-function
       context

       (js-return 1)))))

(test expression-character
  "Test compiling literal characters"

  (mock-backend-state
    (mock-contexts
        ((context () #\h))

      (test-context-function
       context

       (js-return (js-new "Tridash.Char" (list (js-string "h"))))))))

(test expression-function-call-positional-arguments
  "Test compiling function calls with positional arguments"

  (mock-backend-state
    (mock-meta-nodes ((f (a b) (a b)))
      (mock-contexts
          ((context (a b) (functor f a b)))

        (test-context-function
         context

         (js-return (js-call f (d a) (d b))))))))

(test expression-function-call-optional-arguments
  "Test compiling function calls with optional arguments with no default"

  (mock-backend-state
    (mock-meta-nodes ((f (a (optional b)) nil))
      (mock-contexts
          ((context (a) (functor f a :none)))

        (test-context-function
         context

         (js-return
          (->>
           (js-call "Tridash.NoValue")
           (js-call "Tridash.fail")
           js-return
           thunk
           (js-call f (d a)))))))))

(test expression-function-call-rest-arguments
  "Test compiling function calls with rest arguments"

  (mock-backend-state
    (mock-meta-nodes ((f (a (rest b)) nil))
      (mock-contexts
          ((context (a b c) (functor f a (ir:argument-list (list b c)))))

        (test-context-function
         context

         (js-return
          (js-call f (d a) (thunk (js-return (js-array (list (d b) (d c))))))))))))

(test expression-function-call-empty-rest-arguments
  "Test compiling function calls with empty rest argument list"

  (mock-backend-state
    (mock-meta-nodes ((f (a (rest b)) nil))
      (mock-contexts
          ((context (a) (functor f a (ir:argument-list nil))))

        (test-context-function
         context

         (js-return
          (js-call f (d a) (thunk (js-return (js-call "Tridash.Empty"))))))))))

(test expression-function-call-outer-node-reference
  "Test compiling function calls with simple outer node reference"

  (mock-backend-state
    (mock-meta-nodes ((f (a :outer b) (a b)))

      (mock-contexts
          ((context (a b)
                    (->> (alist-hash-map `((b . ,b)))
                         (ir:functor f (list a) :outer-nodes))))

        (test-context-function
         context

         (js-return
          (js-call f (d a) (d b))))))))

(test expression-function-call-outer-node-reference-strict
  "Test compiling function call with strict outer node reference"

  (mock-backend-state
    (mock-meta-nodes ((f (a :outer b) (a b))
                      (+ (a b) (a b)))

      (mock-contexts
          ((context (a b c)
                    (->> (alist-hash-map `((b . ,(functor + b c))))
                         (ir:functor f (list a) :outer-nodes))))

        (test-context-function
         context

         (js-return
          (js-call f (d a) (js-call + (d b) (d c)))))))))

(test expression-function-call-outer-node-reference-lazy
  "Test compiling function call with lazy outer node reference"

  (mock-backend-state
    (mock-meta-nodes ((f (a :outer b) (a))
                      (+ (a b) (a b)))

      (mock-contexts
          ((context (a b c)
                    (->> (alist-hash-map `((b . ,(functor + b c))))
                         (ir:functor f (list a) :outer-nodes))))

        (test-context-function
         context

         (js-return
          (js-call f (d a) (thunk (js-return (js-call + (d b) (d c)))))))))))

(test expression-if
  "Test simple if statements"

  (mock-backend-state
    (mock-meta-nodes ((< (a b) (a b))
                      (- (a b) (a b)))
      (mock-contexts
          ((context (a b)
                    (if-expression (functor < a b)
                                   (functor - b a)
                                   (functor - a b))))

        (test-context-function
         context

         (js-if (js-call "Tridash.check_bool" (resolve (js-call < (d a) (d b))))
                (js-return
                 (thunk (js-return (js-call - (d b) (d a)))))

                (js-return
                 (thunk (js-return (js-call - (d a) (d b)))))))))))

(test expression-if-in-function-call-strict
  "Test if statement nested in strict argument of function call"

  (mock-backend-state
    (mock-meta-nodes ((f (x y) (x))
                      (< (a b) (a b))
                      (+ (a b) (a b)))
      (mock-contexts
          ((context (a b c d)
                    (-<> (if-expression (functor < a 3)
                                        (functor + b c)
                                        d)
                         (functor f <> a))))

        (test-context-function
         context

         (js-var ($ arg1))
         (js-catch
          (list
           (js-if (js-call "Tridash.check_bool" (resolve (js-call < (d a) 3)))
                  (js-call "="
                           ($ arg1)
                           (thunk (js-return (js-call + (d b) (d c)))))

                  (js-call "="
                           ($ arg1)
                           (d d))))

          ($ e)
          (list
           (js-call "=" ($ arg1) (thunk (js-throw ($ e))))))

         (js-return (js-call f ($ arg1) (d a))))))))

(test expression-if-in-function-call-lazy
  "Test if statement nested in lazy argument of function call"

  (mock-backend-state
    (mock-meta-nodes ((f (a b) nil)
                      (< (a b) (a b))
                      (+ (a b) (a b)))
      (mock-contexts
          ((context (a b c d)
                    (-<> (if-expression (functor < a 3)
                                        (functor + b c)
                                        d)
                         (functor f <> a))))

        (test-context-function
         context

         (-<>
          (js-if (js-call "Tridash.check_bool" (resolve (js-call < (d a) 3)))
                 (js-return
                  (thunk (js-return (js-call + (d b) (d c)))))

                 (js-return
                  (d d)))
          thunk
          (js-call f <> (d a))
          js-return))))))

(test expression-if-in-if-condition
  "Test if statement nested in if condition"

  (mock-backend-state
    (mock-meta-nodes ((< (a b) (a b))
                      (+ (a b) (a b)))
      (mock-contexts
          ((context (a b c d e)
                    (-<> (if-expression (functor < b c) b c)
                         (if-expression (functor + b c) d)
                         (if-expression a <> e))))

        (test-context-function
         context

         (js-if (js-call "Tridash.check_bool" (resolve (d a)))

                (js-return
                 (thunk
                  (js-var ($ cond))
                  (js-if (js-call "Tridash.check_bool" (resolve (js-call < (d b) (d c))))
                         (js-call "="
                                  ($ cond)
                                  (d b))

                         (js-call "="
                                  ($ cond)
                                  (d c)))

                  (js-if (js-call "Tridash.check_bool" (resolve ($ cond)))
                         (js-return
                          (thunk (js-return (js-call + (d b) (d c)))))

                         (js-return
                          (d d)))))

                (js-return (d e))))))))

(test expression-object-simple-fields
  "Test object creation with simple field value expressions"

  (mock-backend-state
    (mock-meta-nodes ((+ (a b) (a b))
                      (- (a b) (a b)))
      (mock-contexts
          ((context (x y) (object ("sum" (functor + x y))
                                  ("difference" (functor - x y))
                                  ("x" x)
                                  ("y" y))))

        (test-context-function
         context

         (js-return
          (js-object
           (list
            (list (js-string "sum") (thunk (js-return (js-call + (d x) (d y)))))
            (list (js-string "difference") (thunk (js-return (js-call - (d x) (d y)))))
            (list (js-string "x") (d x))
            (list (js-string "y") (d y))))))))))

(test expression-object-field-if-expressions
  "Test object creation with if expressions in field values"

  (mock-backend-state
    (mock-meta-nodes ((< (a b) (a b)))
      (mock-contexts
          ((context (x y) (object ("min" (if-expression (functor < x y) x y))
                                  ("max" (if-expression (functor < y x) x y)))))

        (test-context-function
         context

         (js-return
          (js-object
           (list
            (list (js-string "min")
                  (thunk
                   (js-if (js-call "Tridash.check_bool" (resolve (js-call < (d x) (d y))))
                          (js-return (d x))
                          (js-return (d y)))))

            (list (js-string "max")
                  (thunk
                   (js-if (js-call "Tridash.check_bool" (resolve (js-call < (d y) (d x))))
                          (js-return (d x))
                          (js-return (d y)))))))))))))

(test expression-object-member-access-strict
  "Test object member access of strict argument"

  (with-module-table modules
    (build-core-module modules)

    (mock-backend-state
      (mock-meta-nodes ((f (x) (x)))
        (mock-contexts
            ((context (object) (functor f (member-expression object '|field|))))

          (test-context-function
           context

           (-<> (d object)
                (js-call "Tridash.member" <> (js-string "field"))
                (js-call f <>)
                js-return)))))))

(test expression-object-member-access-lazy
  "Test object member access of lazy argument"

  (with-module-table modules
    (build-core-module modules)

    (mock-backend-state
      (mock-meta-nodes ((f (x) nil))
        (mock-contexts
            ((context (object) (functor f (member-expression object '|field|))))

          (test-context-function
           context

           (js-return
            (js-call f (-<> (d object)
                            (js-call "Tridash.member" <> (js-string "field"))
                            js-return
                            thunk)))))))))

(test expression-object-member-access-expression
  "Test object member access of expression returning object"

  (with-module-table modules
    (build-core-module modules)

    (mock-backend-state
      (mock-meta-nodes ((f (x) nil))
        (mock-contexts
            ((context (a) (member-expression (functor f a) '\x)))

          (test-context-function
           context

           (js-return
            (js-call "Tridash.member" (js-call f (d a)) (js-string "x")))))))))

(test expression-object-member-access-key
  "Test object member access with key expression"

  (with-module-table modules
    (build-core-module modules)

    (mock-backend-state
      (mock-meta-nodes ((f (x) nil))
        (mock-contexts
            ((context (a) (member-expression a (functor f a))))

          (test-context-function
           context

           (js-return
            (js-call "Tridash.member" (d a) (js-call f (d a))))))))))

(test expression-object-member-access-if-expression
  "Test object member access of if expression"

  (with-module-table modules
    (build-core-module modules)

    (mock-backend-state
      (mock-contexts
          ((context (cond a b) (member-expression (if-expression cond a b) '\z)))

        (test-context-function
         context

         (js-var ($ object))
         (js-catch
          (list
           (js-if (js-call "Tridash.check_bool" (resolve (d cond)))
                  (js-call "=" ($ object) (d a))
                  (js-call "=" ($ object) (d b))))

          ($ e)
          (list
           (js-call "=" ($ object) (thunk (js-throw ($ e))))))

         (js-return
          (js-call "Tridash.member" ($ object) (js-string "z"))))))))

(test expression-conditional-bindings-throw-fail
  "Test throwing fail exception in conditional bindings"

  (mock-backend-state
    (mock-meta-nodes ((< (a b) (a b)))
      (mock-contexts
          ((context (a) (ir:expression-block
                         (if-expression (functor < a 0) a (fail-expression)))))

        (test-context-function
         context

         (js-if (js-call "Tridash.check_bool" (resolve (js-call < (d a) 0)))
                (js-return (d a))
                (js-return
                 (thunk
                  (js-return (js-call "Tridash.fail"))))))))))

(test expression-catch
  "Test catch expressions"

  (mock-backend-state
    (mock-meta-nodes ((< (a b) (a b))
                      (+ (a b) (a b))
                      (- (a b) (a b)))
      (mock-contexts
          ((context (a b) (catch-expression
                           (ir:expression-block
                            (if-expression (functor < a b)
                                           (functor + a b)
                                           (fail-expression)))

                           (ir:expression-block (functor - a b)))))

        (test-context-function
         context

         (js-var ($ try))
         (js-catch
          (list
           (js-if (js-call "Tridash.check_bool" (resolve (js-call < (d a) (d b))))

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
                     (js-call - (d a) (d b)))))))))))

(test expression-meta-node-reference
  "Test meta-node reference without outer nodes"

  (mock-backend-state
    (mock-meta-nodes ((map (f l) nil)
                      (f (x) (x)))
      (mock-contexts
          ((context (a)
                    (functor map (ir:meta-node-ref f) a)))

        (test-context-function
         context

         (js-return
          (js-call
           map

           (js-lambda
            '(($ x))

            (list
             (js-if
              (js-call "!==" (js-member "arguments" "length") 1)
              (js-return (js-call "Tridash.fail" (js-call "Tridash.ArityError"))))

             (js-return (js-call f ($ x)))))

           (d a))))))))

(test expression-meta-node-reference-optional-arguments
  "Test meta-node reference with optional arguments"

  (mock-backend-state
    (mock-meta-nodes ((map (f l) nil)
                      (1+ (n (optional d)) (n d)))
      (mock-contexts
          ((context (a b)
                    (functor map (ir:meta-node-ref 1+ :optional (list b)) a)))

        (test-context-function
         context

         (js-return
          (js-call
           map

           (js-lambda
            (list ($ n) (js-call "=" ($ d) (d b)))
            (list
             (js-if
              (js-call
               "||"
               (js-call "<" (js-member "arguments" "length") 1)
               (js-call ">" (js-member "arguments" "length") 2))
              (js-return (js-call "Tridash.fail" (js-call "Tridash.ArityError"))))

             (js-return
              (js-call 1+ ($ a) ($ d)))))

           (d a))))))))

(test expression-meta-node-reference-rest-arguments
  "Test meta-node reference with rest arguments"

  (mock-backend-state
    (mock-meta-nodes ((map (f l) nil)
                      (list (x (rest xs)) (x)))
      (mock-contexts
          ((context (a) (functor map (ir:meta-node-ref list) a)))

        (test-context-function
         context

         (js-return
          (js-call
           map

           (js-lambda
            (list ($ x) (js-call "..." ($ xs)))
            (list
             (js-if
              (js-call "<" (js-member "arguments" "length") 1)
              (js-return (js-call "Tridash.fail" (js-call "Tridash.ArityError"))))

             (js-if (js-call "===" (js-member ($ xs) "length") 0)
                    (js-call "=" ($ xs) (js-call "Tridash.Empty")))

             (js-return
              (js-call list ($ x) ($ xs)))))
           (d a))))))))

(test expression-meta-node-reference-outer-nodes
  "Test meta-node-reference with outer nodes"

  (mock-backend-state
    (mock-meta-nodes ((map (f l) nil)
                      (f (a :outer x) (a x)))
      (mock-contexts
          ((context (a b)
                    (functor map (ir:meta-node-ref f :outer-nodes (alist-hash-map `((x . ,b)))) a)))

        (test-context-function
         context

         (js-return
          (js-call
           map
           (js-lambda
            '(($ a))
            (list
             (js-if
              (js-call "!==" (js-member "arguments" "length") 1)
              (js-return (js-call "Tridash.fail" (js-call "Tridash.ArityError"))))

             (js-return
              (js-call f ($ a) (d b)))))
           (d a))))))))

(test expression-meta-node-reference-optional-rest-outer-nodes
  "Test meta-node reference with optional rest and outer node operands"

  (mock-backend-state
    (mock-meta-nodes ((map (f l) nil)
                      (f (a (optional b) (rest c) :outer d) (a d)))
      (mock-contexts
          ((context (a b)
                    (functor map
                             (ir:meta-node-ref f
                                               :optional (list 1)
                                               :outer-nodes (alist-hash-map `((d . ,b))))
                             a)))

        (test-context-function
         context

         (js-return
          (js-call
           map

           (js-lambda
            (list
             ($ a) (js-call "=" ($ b) 1) (js-call "..." ($ c)))

            (list
             (js-if
              (js-call "<" (js-member "arguments" "length") 1)
              (js-return (js-call "Tridash.fail" (js-call "Tridash.ArityError"))))

             (js-if (js-call "===" (js-member ($ c) "length") 0)
                    (js-call "=" ($ c) (js-call "Tridash.Empty")))

             (js-return
              (js-call f ($ a) ($ b) ($ c) (d b)))))

           (d a))))))))

(test expression-raw-meta-node-reference
  "Test raw meta-node references"

  (mock-backend-state
    (mock-meta-nodes (f)
      (mock-contexts
          ((context () (ir:node-ref f)))

        (test-context-function
         context

         (js-return f))))))

(test expression-call-node-in-return
  "Test calling node in return value"

  (mock-backend-state
    (mock-contexts
        ((context (f x) (functor f x)))

      (test-context-function context
                             (js-return
                              (js-call
                               (js-call "Tridash.check_function" (resolve (d f)))
                               (d x)))))))

(test expression-call-node-strict-argument
  "Test calling node in strict argument"

  (mock-backend-state
    (mock-meta-nodes ((func (x y) (x)))
      (mock-contexts
          ((context (f x y) (functor func (functor f x) y)))

        (test-context-function context
                               (js-var ($ f))
                               (js-catch
                                (list
                                 (js-call
                                  "="
                                  ($ f)

                                  (js-call
                                   (js-call "Tridash.check_function" (resolve (d f)))
                                   (d x))))

                                ($ e)
                                (list
                                 (js-call "=" ($ f) (thunk (js-throw ($ e))))))

                               (js-return
                                (js-call func ($ f) (d y))))))))

(test expression-call-node-lazy-argument
  "Test calling node in lazy argument"

  (mock-backend-state
    (mock-meta-nodes ((func (x y) nil))
      (mock-contexts
          ((context (f x y) (functor func (functor f x) y)))

        (test-context-function context
                               (-<> (js-call (js-call "Tridash.check_function" (resolve (d f))) (d x))
                                    js-return
                                    thunk
                                    (js-call func <> (d y))
                                    js-return))))))

(test expression-call-node-expression-operands
  "Test calling node with expression operands"

  (mock-backend-state
    (mock-meta-nodes ((+ (a b) (a b)))
      (mock-contexts
          ((context (f x y) (functor f (functor + x y))))

        (test-context-function context
                               (->> (js-call + (d x) (d y))
                                    js-return
                                    thunk
                                    (js-call (js-call "Tridash.check_function" (resolve (d f))))
                                    js-return))))))


;;; Test Compiling Meta-Nodes

(defun test-function% (got expected)
  "Tests that the generated code GOT is equal to EXPECTED. The test is
   performed with *AST-ALIASES* bound to a new empty alias table."

  (let ((*ast-aliases* (make-hash-map)))

    (is (ast-list= got expected)
        "Expected:~%~%~a~%~%~2TGot:~%~%~a"
        expected got)))

(defun test-meta-node-function% (meta-node expected)
  (let ((*ast-aliases* (make-hash-map))
        (code (strip-redundant (create-meta-node meta-node))))

    (is (ast-list= code expected)
        "Expected:~%~%~a~%~%~2TGot:~%~%~a"
        expected code)))

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
               (is (ast= it ,expr)
                   "Expected:~%~%~a~%~%~2TGot:~%~%~a"
                   ,expr it)))))

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


(test meta-node-simple-function
  "Test meta-node which compiles to simple function"

  (with-module-table modules
    (build-core-module)
    (build "/import(core)")
    (build "add(x, y) : x + y"
           "/attribute(add, removable, 0)")
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

(test meta-node-recursive
  "Test compiling recursive meta-nodes"

  (with-module-table modules
    (build-core-module)
    (build "/import(core)")
    (build "fact(n) : case{n < 1 : 1; n * fact(n - 1)}")
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
               (js-call "Tridash.check_bool"
                        (resolve
                         (js-call
                          "<"
                          (js-call "Tridash.check_number" (resolve ($ n)))
                          (js-call "Tridash.check_number" (resolve 1)))))

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

(test meta-node-tail-recursive
  "Test compiling tail recursive meta-nodes"

  (with-module-table modules
    (build-core-module)
    (build "/import(core)")
    (build "fact(n) : { iter(n, acc) : case{n < 1 : acc; iter(n - 1, n * acc)}; iter(n, 1) }")
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
                 (js-call "Tridash.check_bool"
                          (resolve
                           (js-call
                            "<"
                            (js-call "Tridash.check_number" (resolve ($ n2)))
                            (js-call "Tridash.check_number" (resolve 1)))))

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

(test meta-node-mutual-recursion
  "Test compiling mutually recursive meta-nodes"

  (with-module-table modules
    (build-core-module)
    (build "/import(core)")
    (build "fib(n) : case{n > 1 : fib1(n) + fib2(n); 1}")
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
               (js-call "Tridash.check_bool"
                        (resolve
                         (js-call
                          ">"
                          (js-call "Tridash.check_number" (resolve ($ n)))
                          (js-call "Tridash.check_number" (resolve 1)))))

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

(test meta-node-fail-expression
  "Test compiling meta-nodes with fail expressions"

  (with-module-table modules
    (build-core-module)
    (build "/import(core)")
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
              (js-if (js-call "Tridash.check_bool"
                              (resolve
                               (js-call
                                ">"
                                (js-call "Tridash.check_number" (resolve ($ x)))
                                (js-call "Tridash.check_number" (resolve 0)))))

                     (js-return ($ x))
                     (js-return
                      (thunk
                       (js-return
                        (js-call "Tridash.fail"))))))

             ($ e)
             (list
              (js-return
               (thunk (js-throw ($ e)))))))))))))

(test meta-node-common-sub-expression-strict-argument
  "Test compiling meta-nodes with common sub-expressions in strict arguments"

  (with-module-table modules
    (build-core-module)
    (build "/import(core)")
    (build "func(a, b) : (a + b) - (a + b)"
           "/attribute(func, removable, 0)")

    (with-nodes ((func "func")) (finish-build modules)

      (mock-backend-state
        (test-meta-node-function func
          (js-function
           (meta-node-id func)
           '(($ a) ($ b))

           (list
            (js-var ($ g1))
            (js-catch
             (list
              (->> (js-call
                    "+"
                    (js-call "Tridash.check_number" (resolve ($ a)))
                    (js-call "Tridash.check_number" (resolve ($ b))))
                   (js-call "=" ($ g1))))

             ($ e)
             (list
              (->> (js-throw ($ e))
                   thunk
                   (js-call "=" ($ g1)))))

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

(test meta-node-optional-arguments
  "Test compiling meta-node with optional arguments"

  (with-module-table modules
    (build-core-module)
    (build "/import(core)")
    (build "1+(n, d : 1) : n + d")
    (build "f(a) : 1+(a)"
           "/attribute(f, removable, 0)")

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

(test meta-node-rest-arguments
  "Test compiling meta-node with rest arguments"

  (with-module-table modules
    (build-core-module)
    (build "/import(core)")
    (build "f(x, .. xs) : x + xs")
    (build "g(x, y, z) : f(x, y, z)")
    (build "h(x) : f(x)"

           "/attribute(h, removable, 0)"
           "/attribute(g, removable, 0)")

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

(test meta-node-outer-nodes
  "Test compiling meta-nodes with outer nodes"

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"
           "inc(x) : x + delta"

           "/attribute(inc, removable, 0)"
           "/attribute(delta, input, 1)")

    (with-nodes ((inc "inc")) (finish-build)
      (mock-backend-state
        (test-meta-node-function inc
          (js-function
           (meta-node-id inc)
           '(($ x) ($ delta))

           (list
            (protected
             (js-return
              (js-call
               "+"

               (js-call "Tridash.check_number" (resolve ($ x)))
               (js-call "Tridash.check_number" (resolve ($ delta)))))))))))))

(test primitive-arithmetic-add
  "Test compiling arithmetic `+` expression"

  (with-module-table modules
    (build-core-module)
    (build "/import(core)")
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

(test primitive-arithmetic-subtract
  "Test compiling arithmetic `-`"

  (with-module-table modules
    (build-core-module)
    (build "/import(core)")
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

(test primitive-arithmetic-negate
  "Test compiling arithmetic negate expression"

  (with-module-table modules
    (build-core-module)
    (build "/import(core)")
    (build "neg(x) : - x")
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

(test primitive-arithmetic-multiply
  "Test compiling arithmetic `*`"

  (with-module-table modules
    (build-core-module)
    (build "/import(core)")
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

(test primitive-arithmetic-divide
  "Test compiling arithmetic `/` expression"

  (with-module-table modules
    (build-core-module)
    (build "/import(core)")
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
               (js-call "Tridash.check_number" (resolve ($ y)))))))))))))

(test primitive-comparison-less-than
  "Test compiling `<` comparison"

  (with-module-table modules
    (build-core-module)
    (build "/import(core)")
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

(test primitive-comparison-less-than-equal
  "Test compiling `<=` comparison"

  (with-module-table modules
    (build-core-module)
    (build "/import(core)")
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

(test primitive-comparison-greater-than
  "Test compiling `>` comparison"

  (with-module-table modules
    (build-core-module)
    (build "/import(core)")
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

(test primitive-comparison-greater-than-equal
  "Test compiling `>=` comparison"

  (with-module-table modules
    (build-core-module)
    (build "/import(core)")
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

(test primitive-comparison-equal
  "Test compiling `=` comparison"

  (with-module-table modules
    (build-core-module)
    (build "/import(core)")
    (build "equal?(x, y) : x = y")
    (finish-build)

    (with-nodes ((equal? "equal?")) modules
      (mock-backend-state
        (test-meta-node-function equal?
          (js-function
           (meta-node-id equal?)
           '(($ x) ($ y))

           (list
            (js-return
             (js-call "Tridash.eq" ($ x) ($ y))))))))))

(test primitive-comparison-not-equal
  "Test compiling `!=` comparison"

  (with-module-table modules
    (build-core-module)
    (build "/import(core)")
    (build "not-equal?(x, y) : x != y")
    (finish-build)

    (with-nodes ((not-equal? "not-equal?")) modules
      (mock-backend-state
        (test-meta-node-function not-equal?
          (js-function
           (meta-node-id not-equal?)
           '(($ x) ($ y))

           (list
            (js-return
             (js-call "Tridash.neq" ($ x) ($ y))))))))))

(test primitive-bool-and
  "Test Boolean `and` expression."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)")
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

(test primitive-bool-or
  "Test Boolean `or` expression."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)")
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

(test primitive-bool-not
  "Test Boolean `not expression."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)")
    (build "my-not(x) : not x")
    (finish-build)

    (with-nodes ((my-and "my-not")) modules
      (mock-backend-state
        (test-meta-node-function my-and
          (js-function
           (meta-node-id my-and)
           '(($ x))

           (list
            (protected
             (js-return (js-call "!" (js-call "Tridash.check_bool" (resolve ($ x)))))))))))))

(test primitive-cast-int
  "Test converting to type `int`"

  (with-module-table modules
    (build-core-module)
    (build "/import(core)")
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

(test primitive-cast-real
  "Test converting to type `real`"

  (with-module-table modules
    (build-core-module)
    (build "/import(core)")
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

(test primitive-type-check-int
  "Test `int` type check"

  (with-module-table modules
    (build-core-module)
    (build "/import(core)")
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

(test primitive-type-check-real
  "Test `real` type check"

  (with-module-table modules
    (build-core-module)
    (build "/import(core)")
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

(test primitive-type-check-string
  "Test `string` type check"

  (with-module-table modules
    (build-core-module)
    (build "/import(core)")
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

(test primitive-check-infinity
  "Test checking for infinity"

  (with-module-table modules
    (build-core-module)
    (build "/import(core)")
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

(test primitive-check-nan
  "Test checking for NaN"

  (with-module-table modules
    (build-core-module)
    (build "/import(core)")
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
             (js-call "Tridash.is_nan" ($ x))))))))))

(test external-meta-node-reference-+
  "Test compiling external meta-node reference to `+`"

  (with-module-table modules
    (build-core-module)
    (build "/import(core, map, +)"
           "f(l) : map(+, l)"

           "f(a)"
           "/attribute(a, input, 1)")

    (with-nodes ((f "f") (map "map")) (finish-build)
      (mock-backend-state
        (test-meta-node-function f
          (js-function
           (meta-node-id f)
           '(($ l))

           (list
            (js-return
             (js-call
              map

              (js-lambda
               '(($ a) ($ b))

               (list
                (js-if
                 (js-call "!==" (js-member "arguments" "length") 2)
                 (js-return (js-call "Tridash.fail" (js-call "Tridash.ArityError"))))

                (js-return
                 (js-call "+"
                          (js-call "Tridash.check_number" (resolve ($ a)))
                          (js-call "Tridash.check_number" (resolve ($ b)))))))

              ($ l))))))))))

(test external-meta-node-reference--
  "Test compiling external meta-node reference to `-`"

  (with-module-table modules
    (build-core-module)
    (build "/import(core, map, -)"
           "f(l) : map(-, l)"

           "f(a)"
           "/attribute(a, input, 1)")
    (finish-build)

    (with-nodes ((f "f") (map "map")) modules
      (mock-backend-state
        (test-meta-node-function f
          (js-function
           (meta-node-id f)
           '(($ l))

           (list
            (js-return
             (js-call map "Tridash.sub_neg" ($ l))))))))))

(test external-meta-node-reference-*
  "Test compiling external meta-node reference to `*`"

  (with-module-table modules
    (build-core-module)
    (build "/import(core, map, *)"
           "f(l) : map(*, l)"

           "f(a)"
           "/attribute(a, input, 1)")
    (finish-build)

    (with-nodes ((f "f") (map "map")) modules
      (mock-backend-state
        (test-meta-node-function f
          (js-function
           (meta-node-id f)
           '(($ l))

           (list
            (js-return
             (js-call
              map

              (js-lambda
               '(($ a) ($ b))

               (list
                (js-if
                 (js-call "!==" (js-member "arguments" "length") 2)
                 (js-return (js-call "Tridash.fail" (js-call "Tridash.ArityError"))))

                (js-return
                 (js-call "*"
                          (js-call "Tridash.check_number" (resolve ($ a)))
                          (js-call "Tridash.check_number" (resolve ($ b)))))))

              ($ l))))))))))

(test external-meta-node-reference-/
  "Test compiling external meta-node reference to `/`"

  (with-module-table modules
    (build-core-module)
    (build "/import(core, map, /)"
           "f(l) : map(/, l)"

           "f(a)"
           "/attribute(a, input, 1)")
    (finish-build)

    (with-nodes ((f "f") (map "map")) modules
      (mock-backend-state
        (test-meta-node-function f
          (js-function
           (meta-node-id f)
           '(($ l))

           (list
            (js-return
             (js-call
              map

              (js-lambda
               '(($ a) ($ b))

               (list
                (js-if
                 (js-call "!==" (js-member "arguments" "length") 2)
                 (js-return (js-call "Tridash.fail" (js-call "Tridash.ArityError"))))

                (js-return
                 (js-call "/"
                          (js-call "Tridash.check_number" (resolve ($ a)))
                          (js-call "Tridash.check_number" (resolve ($ b)))))))

              ($ l))))))))))

(test external-meta-node-reference-%
  "Test compiling external meta-node reference to `%`"

  (with-module-table modules
    (build-core-module)
    (build "/import(core, map, %)"
           "f(l) : map(%, l)"

           "f(a)"
           "/attribute(a, input, 1)")
    (finish-build)

    (with-nodes ((f "f") (map "map")) modules
      (mock-backend-state
        (test-meta-node-function f
          (js-function
           (meta-node-id f)
           '(($ l))

           (list
            (js-return
             (js-call
              map

              (js-lambda
               '(($ a) ($ b))

               (list
                (js-if
                 (js-call "!==" (js-member "arguments" "length") 2)
                 (js-return (js-call "Tridash.fail" (js-call "Tridash.ArityError"))))

                (js-return
                 (js-call "%"
                          (js-call "Tridash.check_number" (resolve ($ a)))
                          (js-call "Tridash.check_number" (resolve ($ b)))))))

              ($ l))))))))))

(test external-meta-node-reference-<
  "Test compiling external meta-node reference to `<`"

  (with-module-table modules
    (build-core-module)
    (build "/import(core, map, <)"
           "f(l) : map(<, l)"

           "f(a)"
           "/attribute(a, input, 1)")
    (finish-build)

    (with-nodes ((f "f") (map "map")) modules
      (mock-backend-state
        (test-meta-node-function f
          (js-function
           (meta-node-id f)
           '(($ l))

           (list
            (js-return
             (js-call
              map

              (js-lambda
               '(($ a) ($ b))

               (list
                (js-if
                 (js-call "!==" (js-member "arguments" "length") 2)
                 (js-return (js-call "Tridash.fail" (js-call "Tridash.ArityError"))))

                (js-return
                 (js-call "<"
                          (js-call "Tridash.check_number" (resolve ($ a)))
                          (js-call "Tridash.check_number" (resolve ($ b)))))))

              ($ l))))))))))

(test external-meta-node-reference->
  "Test compiling external meta-node reference to `>`"

  (with-module-table modules
    (build-core-module)
    (build "/import(core, map, >)"
           "f(l) : map(>, l)"

           "f(a)"
           "/attribute(a, input, 1)")
    (finish-build)

    (with-nodes ((f "f") (map "map")) modules
      (mock-backend-state
        (test-meta-node-function f
          (js-function
           (meta-node-id f)
           '(($ l))

           (list
            (js-return
             (js-call
              map

              (js-lambda
               '(($ a) ($ b))

               (list
                (js-if
                 (js-call "!==" (js-member "arguments" "length") 2)
                 (js-return (js-call "Tridash.fail" (js-call "Tridash.ArityError"))))

                (js-return
                 (js-call ">"
                          (js-call "Tridash.check_number" (resolve ($ a)))
                          (js-call "Tridash.check_number" (resolve ($ b)))))))

              ($ l))))))))))

(test external-meta-node-reference-<=
  "Test compiling external meta-node reference to `<=`"

  (with-module-table modules
    (build-core-module)
    (build "/import(core, map, <=)"
           "f(l) : map(<=, l)"

           "f(a)"
           "/attribute(a, input, 1)")
    (finish-build)

    (with-nodes ((f "f") (map "map")) modules
      (mock-backend-state
        (test-meta-node-function f
          (js-function
           (meta-node-id f)
           '(($ l))

           (list
            (js-return
             (js-call
              map

              (js-lambda
               '(($ a) ($ b))

               (list
                (js-if
                 (js-call "!==" (js-member "arguments" "length") 2)
                 (js-return (js-call "Tridash.fail" (js-call "Tridash.ArityError"))))

                (js-return
                 (js-call "<="
                          (js-call "Tridash.check_number" (resolve ($ a)))
                          (js-call "Tridash.check_number" (resolve ($ b)))))))

              ($ l))))))))))

(test external-meta-node-reference->=
  "Test compiling external meta-node reference to `>=`"

  (with-module-table modules
    (build-core-module)
    (build "/import(core, map, >=)"
           "f(l) : map(>=, l)"

           "f(a)"
           "/attribute(a, input, 1)")
    (finish-build)

    (with-nodes ((f "f") (map "map")) modules
      (mock-backend-state
        (test-meta-node-function f
          (js-function
           (meta-node-id f)
           '(($ l))

           (list
            (js-return
             (js-call
              map

              (js-lambda
               '(($ a) ($ b))

               (list
                (js-if
                 (js-call "!==" (js-member "arguments" "length") 2)
                 (js-return (js-call "Tridash.fail" (js-call "Tridash.ArityError"))))

                (js-return
                 (js-call ">="
                          (js-call "Tridash.check_number" (resolve ($ a)))
                          (js-call "Tridash.check_number" (resolve ($ b)))))))

              ($ l))))))))))

(test external-meta-node-reference-!
  "Test compiling external meta-node reference to `!`"

  (with-module-table modules
    (build-core-module)
    (build "/import(core, map, not)"
           "f(l) : map(not, l)"

           "f(a)"
           "/attribute(a, input, 1)")
    (finish-build)

    (with-nodes ((f "f") (map "map")) modules
      (mock-backend-state
        (test-meta-node-function f
          (js-function
           (meta-node-id f)
           '(($ l))

           (list
            (js-return
             (js-call
              map

              (js-lambda
               '(($ x))

               (list
                (js-if
                 (js-call "!==" (js-member "arguments" "length") 1)
                 (js-return (js-call "Tridash.fail" (js-call "Tridash.ArityError"))))

                (js-return
                 (js-call "!" (js-call "Tridash.check_bool" (resolve ($ x)))))))

              ($ l))))))))))

(test error-undefined-external-meta-node
  "Test error signalled when using meta-node not defined for JS backend"

  (with-module-table modules
    (build-core-module)
    (build "/external(f, a)"
           "g(x) : f(x)")
    (finish-build)

    (with-nodes ((g "g")) modules
      (mock-backend-state
        (signals undefined-external-meta-node-error
          (create-meta-node g))))))
