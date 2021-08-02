;;;; macros.lisp
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

;;;; User-Defined Macro Tests

(defpackage :tridash/test.macros
  (:use :generic-cl
        :alexandria
        :anaphora
        :arrows
        :iterate
        :optima
        :named-readtables

        :tridash.parser
        :tridash.frontend

        :fiveam
        :tridash/test
        :tridash/test.util)

  (:shadowing-import-from :generic-cl
                          :emptyp
                          :multiply
                          :accumulate)

  (:shadowing-import-from :fiveam :fail)

  (:import-from :lol
                :defmacro!
                :lol-syntax)

  (:import-from :tridash.frontend
                :tridash->cl-function
                :call-meta-node
                :call-tridash-meta-node
                :call-node

                :thunk
                :resolve
                :resolve%
                :tridash-fail

                :fail-thunk
                :+empty-list+
                :group-rest-args

                :check-arity
                :correct-arity?%
                :fail-arity-error

                :+optional-argument+
                :+rest-argument+)

  (:local-nicknames (:ir :tridash.ir)))

(in-package :tridash/test.macros)

(in-readtable lol-syntax)


;;; Test Suite Definition

(def-suite macros
    :description "Test user-defined Tridash macros."
    :in frontend)

(in-suite macros)


;;; Utilities

(defun functor (operator &rest arguments)
  "Creates a `FUNCTOR-EXPRESSION' with operator OPERATOR and arguments
   ARGUMENTS."

  (ir:functor operator arguments))

(defun expression= (expected got)
  "Checks that the CL expression GOT is equal to EXPECTED. Symbols in
   EXPECTED, beginning with $, are replaced with the symbol in GOT
   corresponding to the first occurrence."

  (let ((aliases (make-hash-map)))
    (flet ((equal? (got expected)
             (match* (got expected)
               (((type symbol) (type symbol))
                (= got
                   (cond
                     ((starts-with #\$ (symbol-name expected))
                      (ensure-get expected aliases got))

                     ((starts-with #\! (symbol-name expected))
                      (id-symbol (subseq (symbol-name expected) 1)))

                     (t
                      expected))))

               ((_ _)
                (= got expected)))))

      (tree-equal got expected :test #'equal?))))

(defmacro with-external-meta-nodes ((&rest names) &body body)
  "Creates `EXTERNAL-META-NODE's with names NAMES and binds to
   variables with the same identifiers as the names upcased."

  `(let ,(map #`(,(intern (string-upcase a1)) (make-instance 'external-meta-node :name (id-symbol ,a1))) names)
     ,@body))

(defmacro! with-core-nodes ((&rest names) &body body)
  "Builds the core module and binds the node with names NAMES to
   variables with the same identifiers as the names, upcased."

  `(with-module-table ,g!modules
     (build-core-module)

     (with-nodes ,(map #`(,(intern (string-upcase a1)) ,a1) names) ,g!modules
       ,@body)))

(defmacro mock-meta-node ((&rest operands) expression)
  "Creates a `META-NODE' which takes operands OPERANDS and has a value
   function consisting of EXPRESSION. OPERANDS is a list of symbols
   naming the dependency nodes. EXPRESSION is evaluated in an
   environment where each symbol in OPERANDS is bound to the
   `IR:LINK' object corresponding to the operand, and the symbol
   SELF is bound to the `META-NODE' object."

  (flet ((make-operand (operand)
           (match operand
             ((or (list 'optional symb value)
                  (list 'optional symb))
              (list +optional-argument+ (make-instance 'node :name symb) value))

             ((list 'rest symb)
              (list +rest-argument+ (make-instance 'node :name symb)))

             (_ (make-instance 'node :name operand))))

         (operand-node (operand)
           (match operand
             ((list* 'optional symb _)
              symb)

             ((list 'rest symb)
              symb)

             (_ operand))))

    `(let ((self (make-instance 'final-meta-node
                                :name 'test-meta-node
                                :operands ',(map #'make-operand operands)))
           ,@(map #`(,a1 (ir:link (make-instance 'node :name ',a1)))
                  (map #'operand-node operands)))

       ;; Create an empty `FLAT-NODE-TABLE' to mark meta-node as
       ;; already built
       (setf (definition self) (make-instance 'flat-node-table :nodes (make-hash-set)))
       (setf (value-function (context self nil))
             ,expression)

       ,@(map #`(setf (get ',a1 (operands (context self nil))) ,a1)
              (map #'operand-node operands))
       ,@(map #`(setf (get ',a1 (dependencies self)) ,a1)
              (map #'operand-node operands))

       self)))

(defmacro test-compile-meta-node ((&rest operands) expression args body)
  "Creates and compiles a `META-NODE' to a CL LAMBDA expression and
   checks that it has arguments ARGS and body BODY, by EXPRESSION=.

   OPERANDS and EXPRESSION correspond to the OPERANDS and EXPRESSION
   arguments of MOCK-META-NODE.

   ARGS (not evaluated) is the expected lambda-list of the function.

   BODY is the expected body expression within the BLOCK, TAGBODY,
   RETURN expression. The symbol $recur, occurring in BODY is
   substituted with the TAGBODY tag for tail-recursive self
   calls. BODY is evaluated in an environment in which the symbol SELF
   is bound to the `META-NODE' object."

  (flet ((lambda-args (lambda-list)
           (->> (remove-if (rcurry #'memberp lambda-list-keywords) lambda-list)
                (map #'ensure-car)
                (map (compose #'gensym #'symbol-name)))))

    `(let ((self (mock-meta-node ,operands ,expression)))
       (is (expression=
            `(lambda ,',args
               (declare (ignorable ,@',(lambda-args args)))
               ,,body)

            (tridash->cl-function self))))))


;;; Tridash to CL Compilation Tests

(test compile-functor-expression
  "Test compilation of functor expressions to CL."

  (with-core-nodes ("if" "<" "-")
    (test-compile-meta-node

     (a b)
     (functor if (functor < a b) (functor - b a) (functor - a b))

     ($a $b)
     '(let nil
       (!|if| (!< $a $b)
        (thunk (!- $b $a))
        (thunk (!- $a $b)))))))

(test compile-if-expression
  "Test compilation of if expressions to CL."

  (with-core-nodes ("<" "-")
    (test-compile-meta-node

     (a b)
     (if-expression (functor < a b) (functor - b a) (functor - a b))

     ($a $b)
     '(let nil
       (!|if| (!< $a $b)
        (thunk (!- $b $a))
        (thunk (!- $a $b)))))))

(test compile-object-expression
  "Test compilation of object expressions to CL."

  (with-core-nodes ("+" "-")
    (test-compile-meta-node

     (x y)
     (ir:object
      `((sum ,(functor + x y))
        (diff ,(functor - x y))))

     ($x $y)
     '(let nil
       (alist-hash-map
        (list
         (cons 'sum (thunk (!+ $x $y)))
         (cons 'diff (thunk (!- $x $y)))))))))

(test compile-member-expression
  "Test compilation of member expressions to CL."

  (test-compile-meta-node

   (object)
   (member-expression
    (member-expression object 'key1) 'key2)

   ($obj)
   '(let nil
     (!|member| (!|member| $obj 'key1) 'key2))))

(test compile-catch-expression
  "Test compilation of catch expressions to CL."

  (with-core-nodes ("/" "*")
    (test-compile-meta-node

     (a b)
     (catch-expression
      (functor / a b)
      (functor * a b))

     ($a $b)

     '(let nil
       (!|catch| (!/ $a $b) (thunk (!* $a $b)))))))

(test compile-fail-expression
  "Test compilation of fail expressions to CL."

  (test-compile-meta-node
   ()
   (fail-expression)

   ()
   '(let nil
     (!|fail|))))

(test compile-expression-block
  "Test compilation of expression blocks, with reference count = 1, to CL."

  (with-core-nodes ("+")
    (test-compile-meta-node

     (a)
     (ir:expression-block
      (functor + a 1))

     ($a)
     '(let nil
       (!+ $a 1)))))

(test compile-expression-block-muliple-references
  "Test compilation of expression blocks, with reference count > 1, to CL."

  (with-core-nodes ("+")
    (test-compile-meta-node

     (a)
     (let ((block (ir:expression-block (functor + a 1) :count 2)))
       (functor + block block))

     ($a)
     '(let ($a+1)
       (setf $a+1 (thunk (!+ $a 1)))

       (!+ $a+1 $a+1)))))

(test compile-meta-node-call
  "Test compilation of calls to other meta-nodes, to CL."

  (with-core-nodes ("-")
    (let ((meta-node (mock-meta-node (a) a)))
      (test-compile-meta-node

       (a)
       (functor meta-node (functor - a))

       ($a)
       `(let nil
          (call-tridash-meta-node ,meta-node (list (!- $a))))))))

(test compile-higher-order-external-meta-node
  "Test compilation of higher order external meta-node."

  (with-core-nodes ("not")
    (let ((apply (mock-meta-node (f x) (functor f x))))
      (test-compile-meta-node

       (x)
       (functor apply (ir:meta-node-ref not) x)

       ($x)
       `(let nil
          (call-tridash-meta-node
           ,apply

           (list
            #'(lambda (&rest $args)
                (if (correct-arity?% '(1 . 1) (length $args))
                    (apply #'!|not| $args)
                    (fail-arity-error)))
            $x)))))))

(test compile-higher-order-if-meta-node
  "Test compilation of higher order if meta-node."

  (with-core-nodes ("if")
    (let ((apply (mock-meta-node (f x y z) (functor f x y z))))
      (test-compile-meta-node

       (x y z)
       (functor apply (ir:meta-node-ref if) x y z)

       ($x $y $z)
       `(let nil
          (call-tridash-meta-node
           ,apply

           (list
            #'(lambda (&rest $args)
                (if (correct-arity?% '(2 . 3) (length $args))
                    (apply #'!|if| $args)
                    (fail-arity-error)))
            $x $y $z)))))))

(test compile-higher-order-and-meta-node
  "Test compilation of higher order `and` meta-node."

  (with-core-nodes ("and")
    (let ((apply (mock-meta-node (f x y) (functor f x y))))
      (test-compile-meta-node

       (x y)
       (functor apply (ir:meta-node-ref and) x y)

       ($x $y)
       `(let nil
          (call-tridash-meta-node
           ,apply

           (list
            #'(lambda (&rest $args)
                (if (correct-arity?% '(2 . 2) (length $args))
                    (apply #'!|and| $args)
                    (fail-arity-error)))
            $x $y)))))))

(test compile-higher-order-or-meta-node
  "Test compilation of higher order `or` meta-node."

  (with-core-nodes ("or")
    (let ((apply (mock-meta-node (f x y) (functor f x y))))
      (test-compile-meta-node

       (x y)
       (functor apply (ir:meta-node-ref or) x y)

       ($x $y)
       `(let nil
          (call-tridash-meta-node
           ,apply

           (list
            #'(lambda (&rest $args)
                (if (correct-arity?% '(2 . 2) (length $args))
                    (apply #'!|or| $args)
                    (fail-arity-error)))
            $x $y)))))))

(test compile-higher-order-meta-node
  "Test compilation of higher-order user defined meta-node."

  (let ((apply (mock-meta-node (f x) (functor f x)))
        (f (mock-meta-node (x) x)))

    (test-compile-meta-node

     (x)
     (functor apply (ir:meta-node-ref f) x)

     ($x)
     `(let nil
        (call-tridash-meta-node
         ,apply
         (list
          #'(lambda (&rest $args)
              (if (correct-arity?% '(1 . 1) (length $args))
                  (destructuring-bind ($x2) $args
                    (call-tridash-meta-node ,f (list $x2)))
                  (fail-arity-error)))
          $x))))))

(test compile-higher-order-meta-node-optional-arguments
  "Test compilation of higher-order meta-node with optional arguments."

  (let ((apply (mock-meta-node (f x) (functor f x)))
        (f (mock-meta-node (x (optional y) (optional z)) x)))

    (test-compile-meta-node
     (x)
     (functor apply (ir:meta-node-ref f :optional (list 1 2)) x)

     ($x)
     `(let nil
        (call-tridash-meta-node
         ,apply
         (list
          #'(lambda (&rest $args)
              (if (correct-arity?% '(1 . 3) (length $args))
                  (destructuring-bind ($x2 &optional ($y 1) ($z 2)) $args
                    (call-tridash-meta-node ,f (list $x2 $y $z)))
                  (fail-arity-error)))
          $x))))))

(test compile-higher-order-meta-node-rest-arguments
  "Test compilation of higher-order meta-node with rest arguments."

  (let ((apply (mock-meta-node (f x) (functor f x)))
        (f (mock-meta-node (x y (rest xs)) xs)))

    (test-compile-meta-node
     (x)
     (functor apply (ir:meta-node-ref f) x)

     ($x)
     `(let nil
        (call-tridash-meta-node
         ,apply
         (list
          #'(lambda (&rest $args)
              (if (correct-arity?% '(2) (length $args))
                  (destructuring-bind ($x2 $y &rest $xs &aux ($rest (or $xs +empty-list+)))
                      $args
                    (call-tridash-meta-node ,f (list $x2 $y $rest)))
                  (fail-arity-error)))
          $x))))))

(test compile-invoke-higher-order-node
  "Test compilation of invoking value nodes."

  (test-compile-meta-node

   (f x y)
   (functor f x y)

   ($f $x $y)
   `(let nil
      (call-node $f (list $x $y)))))

(test compile-literals
  "Test compilation of literal values."

  (with-core-nodes ("and")
    (test-compile-meta-node

     ()
     (functor and "hello" (functor and 1 (functor and 2.3 'symbol)))

     ()
     '(let nil
       (!|and| "hello"
        (thunk
         (!|and| 1
           (thunk
            (!|and| 2.3 'symbol)))))))))

(test compile-core-arithmetic
  "Test compilation of core arithmetic meta-nodes."

  (with-core-nodes ("/" "*" "+" "-")
    (test-compile-meta-node

     (a b c d)
     (functor
      /
      (functor * (functor + a b) (functor - c d))
      (functor - d))

     ($a $b $c $d)
     '(let nil
       (!/
        (!* (!+ $a $b) (!- $c $d))
        (!- $d))))))

(test compile-core-comparison-and-logical
  "Test compilation of core comparison and logical meta-nodes."

  (with-core-nodes ("not" "or" "and" "=" "!=" "<" "<=" ">" ">=")
    (test-compile-meta-node

     (x y)
     (functor
      not

      (functor
       or
       (functor and (functor < x y) (functor = y x))

       (functor
        or
        (functor <= x 10)

        (functor
         or
         (functor > 1 y)

         (functor
          or
          (functor >= 8 y)
          (functor != x y))))))

     ($x $y)
     '(let nil
       (!|not|
        (!|or|
         (!|and| (!< $x $y) (thunk (!= $y $x)))
         (thunk
          (!|or|
            (!<= $x 10)

            (thunk
             (!|or|
               (!> 1 $y)

               (thunk
                (!|or|
                  (!>= 8 $y)
                  (thunk (!!= $x $y))))))))))))))

(test compile-core-type-checks
  "Test compilation of core type checking meta-nodes."

  (with-core-nodes ("or" "int?" "real?" "string?")
    (test-compile-meta-node

     (x y z)
     (functor
      or
      (functor int? x)

      (functor
       or
       (functor real? y)
       (functor string? z)))

     ($x $y $z)
     '(let nil
       (!|or|
        (!|int?| $x)

        (thunk
         (!|or|
           (!|real?| $y)
           (thunk (!|string?| $z)))))))))

(test compile-tail-recursive-if
  "Test compilation of if expression in recursive tail position."

  (with-core-nodes ("-" "*" "<")
    (test-compile-meta-node

     (n acc)
     (if-expression (functor < n 2)
                    acc
                    (functor self (functor - n 1) (functor * n acc)))

     ($n $acc)
     `(let nil
        (!|if| (!< $n 2)
          $acc
          (thunk
           (call-tridash-meta-node ,self (list (!- $n 1) (!* $n $acc)))))))))

(test compile-tail-recursive-if-functor
  "Test compilation of if functor in recursive tail position."

  (with-core-nodes ("if" "-" "*" "<")
    (test-compile-meta-node

     (n acc)
     (functor if
              (functor < n 2)
              acc
              (functor self (functor - n 1) (functor * n acc)))

     ($n $acc)
     `(let nil
        (!|if| (!< $n 2)
          $acc
          (thunk
           (call-tridash-meta-node ,self (list (!- $n 1) (!* $n $acc)))))))))

(test compile-tail-recursive-expression-block
  "Test compilation of expression blocks in recursive tail position."

  (with-core-nodes ("if" "-" "*" "<")
    (test-compile-meta-node

     (n acc)
     (functor if
              (functor < n 2)
              acc
              (ir:expression-block
               (functor self (functor - n 1) (functor * n acc))))

     ($n $acc)
     `(let nil
        (!|if| (!< $n 2)
          $acc
          (thunk
           (call-tridash-meta-node ,self (list (!- $n 1) (!* $n $acc)))))))))

(test compile-tail-recursive-or-functor
  "Test compilation of `or` functor in recursive tail position."

  (with-core-nodes ("or" "=" "!=" "-")
    (test-compile-meta-node

     (n)
     (functor or (functor = n 0) (functor self (functor - n 1)))

     ($n)
     `(let nil
        (!|or| (!= $n 0)
          (thunk
           (call-tridash-meta-node ,self (list (!- $n 1)))))))))

(test compile-tail-recursive-and-functor
  "Test compilation of `and` functor in recursive tail position."

  (with-core-nodes ("and" "=" "!=" "-")
    (test-compile-meta-node

     (n)
     (functor and (functor = n 0) (functor self (functor - n 1)))

     ($n)
     `(let nil
        (!|and| (!= $n 0)
          (thunk
           (call-tridash-meta-node ,self (list (!- $n 1)))))))))

(test compile-tail-recursive-catch-expression
  "Test compilation of catch expressions in recursive tail position."

  (with-core-nodes ("-" "+")
    (test-compile-meta-node

     (n)
     (catch-expression (functor self (functor + n 1))
                       (functor self (functor - n 1)))

     ($n)
     `(let nil
        (!|catch| (call-tridash-meta-node ,self (list (!+ $n 1)))
          (thunk
           (call-tridash-meta-node ,self (list (!- $n 1)))))))))

(test compile-meta-node-optional-arguments
  "Test compilation of meta-node with optional arguments."

  (with-core-nodes ("+")
    (test-compile-meta-node
     (n (optional d 1))
     (functor + n d)

     ($n &optional ($d 1))
     '(let nil
       (!|+| $n $d)))))

(test compile-meta-node-multiple-optional-arguments
  "Test compilation of meta-node with multiple optional arguments."

  (with-core-nodes ("+")
    (test-compile-meta-node
     (n (optional d 1) (optional e 2))
     (functor + n (functor + d e))

     ($n &optional ($d 1) ($e 2))
     '(let nil
       (!|+| $n (!|+| $d $e))))))

(test compile-meta-node-rest-argument
  "Test compilation of meta-node with rest argument."

  (with-core-nodes ("cons")
    (test-compile-meta-node
     (x (rest xs))
     (functor cons x xs)

     ($x &optional ($xs +empty-list+))
     '(let nil
       (!|cons| $x $xs)))))

(test compile-meta-node-optional-and-rest-arguments
  "Test compilation of meta-node with optional and rest arguments."

  (with-core-nodes ("cons")
    (test-compile-meta-node
     (x (optional y 2) (rest xs))
     (functor cons x (functor cons y xs))

     ($x &optional ($y 2) ($xs +empty-list+))
     '(let nil
       (!|cons| $x (thunk (!|cons| $y $xs)))))))

(test compile-cyclic-references
  "Test compilation of cyclic references."

  (with-core-nodes ("cons")
    (test-compile-meta-node
     (a b)
     (aprog1 (ir:expression-block nil :count 2)
       (setf (ir:expression it)
             (functor cons a (functor cons b (ir:cyclic-reference it)))))

     ($a $b)
     '(let ($block)
       (setf $block
        (thunk (!|cons| $a (thunk (!|cons| $b $block)))))
       $block))))

(test compile-error-usupported-external-meta-node
  "Test that compiling an unsupported external-meta-node results in an error."

  (with-external-meta-nodes ("not-a-function")
    (signals
     unsupported-meta-node-error

     (tridash->cl-function
      (mock-meta-node
       (arg)
       (functor not-a-function arg))))))


;;; Test Calling Tridash Meta-Nodes from CL

(test call-meta-node-single-expression
  "Test calling a single expression meta-node from CL."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"
           "min(x,y) : case{x < y : x; y}")

    (with-nodes ((min "min")) modules
      (is (= 2 (call-meta-node min '(2 10))))
      (is (= 2 (call-meta-node min '(10 2))))
      (is (= -5.3 (call-meta-node min '(-5.3 7.6))))
      (is (= 1 (call-meta-node min '(1 1)))))))

(test call-meta-node-with-if-expression
  "Test calling a meta-node with if expressions from CL."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"
           "f(cond, x) : if(cond, x, 0)")

    (with-nodes ((f "f")) modules
      (is (= 10 (call-meta-node f '(t 10))))
      (is (= 0 (call-meta-node f '(nil 5))))

      (signals tridash-fail (call-meta-node f '(1 5))))))

(test call-meta-node-with-and-expression
  "Test calling a meta-node with `and` expressions from CL."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"
           "f(cond, x) : cond and x")

    (with-nodes ((f "f")) modules
      (is-true (call-meta-node f '(t t)))
      (is (= nil (call-meta-node f '(nil t))))
      (is (= nil (call-meta-node f '(t nil))))
      (is (= nil (call-meta-node f '(nil nil)))))))

(test call-meta-node-with-or-expression
  "Test calling a meta-node with `or` expressions from CL."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"
           "f(cond, x) : cond or x")

    (with-nodes ((f "f")) modules
      (is-true (call-meta-node f '(t t)))
      (is-true (call-meta-node f '(nil t)))
      (is-true (call-meta-node f '(t nil)))
      (is (= nil (call-meta-node f '(nil nil)))))))

(test call-meta-node-catch-fail-expression
  "Test calling a meta-node with multiple nodes and CATCH-FAIL expressions."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"
           "min(x,y) : { x < y -> (x -> /context(self,c)); y -> /context(self,c) }")

    (with-nodes ((min "min")) modules
      (is (= 2 (resolve (call-meta-node min '(2 10)))))
      (is (= 2 (resolve (call-meta-node min '(10 2)))))
      (is (= -5.3 (resolve (call-meta-node min '(-5.3 7.6)))))
      (is (= 1 (resolve (call-meta-node min '(1 1))))))))

(test call-meta-node-recursive
  "Test calling a recursive meta-node from CL."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"
           "fact(n) : { case{n < 2 : 1; n * fact(n - 1)} }")

    (with-nodes ((fact "fact")) modules
      (is (= 6 (call-meta-node fact '(3))))
      (is (= 120 (call-meta-node fact '(5))))
      (is (= 1 (call-meta-node fact '(0)))))))

(test call-meta-node-tail-recursive
  "Test calling a tail-recursive meta-node from CL."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"
           "fact(n) : { iter(n,acc) : case{n < 2 : acc; iter(n - 1, n * acc)}; iter(n, 1) }")

    (with-nodes ((fact "fact")) modules
      (is (= 6 (call-meta-node fact '(3))))
      (is (= 120 (call-meta-node fact '(5))))
      (is (= 1 (call-meta-node fact '(0)))))))

(test call-meta-node-with-meta-node-call
  "Test calling a meta-node which calls other meta-nodes."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"
           "1-(n) : n - 1"
           "1+(n) : n + 1"
           "f(a, b) : 1-(a) * 1+(b)")

    (with-nodes ((f "f")) modules
      (is (= 0 (call-meta-node f '(1 5))))
      (is (= 45 (call-meta-node f '(10 4))))
      (is (= 33 (call-meta-node f '(4 10)))))))

(test call-meta-node-nested-meta-nodes
  "Test calling a meta-node with nested meta-nodes."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"
           "f(x, y, z) : { g(n) : n - sum; x + y -> sum; g(z) }")

    (with-nodes ((f "f")) modules
      (is (= 0 (call-meta-node f '(1 2 3))))
      (is (= 2 (call-meta-node f '(2 3 7)))))))

(test call-meta-node-optional-arguments-no-default
  "Test calling a meta-node with optional arguments without default values."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, +, fail-type?)"
           "inc(n, :(d)) : n + d"

           "f(x) : inc(x)"
           "g(x) : inc(x, 2)"

           "h(x) : fail-type?(inc(x), &(No-Value%))")

    (with-nodes ((f "f") (g "g") (h "h")) modules
      (signals tridash-fail (call-meta-node f (list 3)))
      (is (= 7 (call-meta-node g (list 5))))

      (is-true (call-meta-node h (list 2))))))

(test call-meta-node-optional-arguments-with-default
  "Test calling a meta-node with optional arguments without default values."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, +)"
           "inc(n, d : 1) : n + d"

           "f(x) : inc(x)"
           "g(x) : inc(x, 2)")

    (with-nodes ((f "f") (g "g")) modules
      (is (= 4 (call-meta-node f (list 3))))
      (is (= 7 (call-meta-node g (list 5)))))))

(test call-meta-node-keyword-arguments
  "Test calling a meta-node with keyword arguments"

  (with-module-table modules
    (build-core-module)
    (build "/import(core, +)"
           "add(a, b, c : 3, d : 4) : a + b + c + d"
           "f(x, y) : add(x, d : 10, b : y)")

    (with-nodes ((f "f")) modules
      (is (= 16 (call-meta-node f '(1 2))))
      (is (= 45 (call-meta-node f '(15 17)))))))

(test call-meta-node-rest-argument
  "Test calling a meta-node with rest argument."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, and, =, Empty)"
           "check(..(xs)) : xs = Empty"

           "f(x) : x and check()"
           "g(x) : check(x)"
           "h(x) : check(x, 1, 2, 3)")

    (with-nodes ((f "f") (g "g") (h "h")) modules
      (is-true (call-meta-node f '(t)))
      (is (= nil (call-meta-node g '(2))))
      (is (= nil (call-meta-node h '(2)))))))

(test call-higher-order-meta-node
  "Test calling meta-node with higher order meta-nodes."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, +, not)"
           "apply(f, x) : f(x)"
           "1+(n) : n + 1"

           "f(a) : apply(..(not), a)"
           "g(a) : apply(..(1+), a)")

    (with-nodes ((f "f") (g "g")) modules
      (is (= t (call-meta-node f '(nil))))
      (is (= nil (call-meta-node f '(t))))

      (is (= 2 (call-meta-node g '(1))))
      (is (= 4 (call-meta-node g '(3)))))))

(test call-higher-order-meta-node-optional-arguments
  "Test calling meta-node with higher-order meta-node with optional arguments."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, +, fail-type?)"
           "apply(f, x) : f(x)"
           "apply2(f, x, y) : f(x, y)"
           "1+(n, :(d)) : n + d"

           "f(a) : apply(1+, a)"
           "g(a, b) : apply2(1+, a, b)"
           "h(x) : fail-type?(apply(1+, x), &(No-Value%))")

    (with-nodes ((f "f") (g "g") (h "h")) modules
      (signals tridash-fail (call-meta-node f '(0)))

      (is (= 3 (call-meta-node g '(1 2))))
      (is (= 8 (call-meta-node g '(5 3))))

      (is-true (call-meta-node h '(1))))))

(test call-higher-order-meta-node-optional-argument-with-default
  "Test calling meta-node with higher order meta-node with optional argument default values."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, +)"
           "apply(f, x) : f(x)"
           "apply2(f, x, y) : f(x, y)"
           "1+(n, d : 1) : n + d"

           "f(a) : apply(1+, a)"
           "g(a, b) : apply2(1+, a, b)")

    (with-nodes ((f "f") (g "g")) modules
      (is (= 1 (call-meta-node f '(0))))
      (is (= 2 (call-meta-node f '(1))))

      (is (= 3 (call-meta-node g '(1 2))))
      (is (= 8 (call-meta-node g '(5 3)))))))

(test call-higher-order-meta-node-rest-argument
  "Test calling meta-node with higher order meta-node with rest argument."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, +, cons)"
           "apply3(f, x, y, z) : f(x, y, z)"
           "apply(f, x) : f(x)"
           "l(x, ..(xs)) : cons(x + 1, xs)"

           "f(a, b, c) : apply3(l, a, b, c)"
           "g(x) : apply(l, x)")

    (with-nodes ((f "f") (g "g")) modules
      (is (= '(2 3 4) (call-meta-node f '(1 3 4))))
      (is (= '(2) (call-meta-node g '(1)))))))

(test call-higher-order-meta-node-rest-argument-empty
  "Test calling meta-node with higher order meta-node with empty rest argument."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, Empty, =)"

           "apply(f, x) : f(x)"
           "l(x, ..(xs)) : xs = Empty"

           "f(a) : apply(l, a)")

    (with-nodes ((f "f")) modules
      (is-true (bool-value (call-meta-node f '(1)))))))

(test call-higher-order-meta-node-optional-arguments-outer-nodes
  "Test calling higher order meta-node with optional arguments and outer node references."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, +)"
	   "apply(f, x) : f(x)"
	   "test(a, x) : { f(y, d : 1) : y + d + x; apply(f, a) }")

    (with-nodes ((test "test")) modules
      (is (= 6 (call-meta-node test '(2 3)))))))

(test call-higher-order-external-meta-node
  "Test calling meta-node with higher-order external meta-node."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, -)"
           "apply(f, x) : f(x)"
           "apply2(f, x, y) : f(x, y)"

           "f(a) : apply(-, a)"
           "g(a, b) : apply2(-, a, b)")

    (with-nodes ((f "f") (g "g")) modules
      (is (= -1 (call-meta-node f '(1))))
      (is (= -2 (call-meta-node f '(2))))

      (is (= 1 (call-meta-node g '(3 2))))
      (is (= 2 (call-meta-node g '(5 3)))))))

(test call-higher-order-meta-node-error
  "Test error when calling a non-meta-node."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, +)"
           "apply(f, x) : f(x)"

           "x+(n) : n + ..(x)"
           "x"

           "f(a) : apply(..(x+), a)")

    (with-nodes ((f "f")) modules
      (signals semantic-error (call-meta-node f '(1))))))

(test call-primitive-function-subtract-and-negate
  "Test calling `-` meta-node with 2 arguments and 1 argument."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, -)"
           "sub(a, b) : a - b"
           "neg(x) : -(x)")

    (with-nodes ((sub "sub") (neg "neg")) modules
      (is (= 3 (call-meta-node sub '(5 2))))
      (is (= -5 (call-meta-node neg '(5)))))))

(test call-meta-node-object-expressions
  "Test calling meta-node with object expressions."

  (with-module-table modules
    (build "Person(first, last) : { first -> self.first; last -> self.last }"
           "get-first(p) : p.first"
           "get-last(p) : p.last")

    (with-nodes ((person "Person") (get-first "get-first") (get-last "get-last"))
        modules

      (let ((p (call-meta-node person '("John" "Doe"))))
        (is (= "John" (call-meta-node get-first (list p))))
        (is (= "Doe" (call-meta-node get-last (list p))))))))

(test call-meta-node-catch-fail-operand
  "Test catching failures in functor operand."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, !=)"
           "fails(x) : { x != x -> /context(self, catch); True -> /context(self, catch) }")

    (with-nodes ((fails "fails")) modules
      (is-false (bool-value (call-meta-node fails '(1))))

      (is-true
       (->> (thunk (error 'tridash-fail))
            list
            (call-meta-node fails))))))

(test call-meta-node-catch-fail-operator
  "Test catching failures in functor operator."

  ;; Test that failures in the operator of a functor are caught.
  (with-module-table modules
    (build-core-module)
    (build "/import(core, !=, >, -)"
           "neg(x) : -(x)"

           "getf(f, x) : { x > 0 -> (f -> self) }"
           "test(x) : fails((getf(neg, x))(x))"

           "fails(x) : { x != x -> /context(self, catch); True -> /context(self, catch) }")

    (with-nodes ((test "test")) modules
      (is (= nil (call-meta-node test '(1))))
      (is-true (call-meta-node test '(-1))))))

(test call-meta-node-fail-types
  "Test failure types."

  (with-module-table modules
    (build-core-module modules)
    (build-source-file "./test/inputs/macros/failure-types.trd" modules)

    (with-nodes ((check-range "check-range")) modules
      (is (= "" (call-meta-node check-range '(2 1 3))))
      (is (= "Error: below minimum!" (call-meta-node check-range '(0 1 3))))
      (is (= "Error: above maximum!" (call-meta-node check-range '(10 2 7)))))))

(test call-meta-node-expression-block
  "Test calling meta-node with one expression-block."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, +)"
           "f(x) : (x + 1) + (x + 1)")

    (with-nodes ((f "f")) modules
      (is-true (call-meta-node f '(1)) 4)
      (is-true (call-meta-node f '(2)) 6))))

(test call-meta-node-expression-block-multiple-references
  "Test calling meta-node with expression-block with multiple references."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, *, +, -)"
           "f(x, y) : { x + 1 -> x1; y + 2 -> y2; (x1 + y2) * (x1 - y2) }")

    (with-nodes ((f "f")) modules
      (is-true (call-meta-node f '(3 7)) -65)
      (is-true (call-meta-node f '(5 2)) 20))))

(test call-meta-node-cyclic-references
  "Test calling a meta-node with cyclic references."

  (with-module-table modules
    (build-core-module)
    (build-source-file "./test/inputs/macros/cyclic-references.trd" modules)

    (with-nodes ((f "f")) modules
      (is-true (call-meta-node f '(1 2)) '(1 2 1 2 1)))))

(test call-meta-node-type-error-arithmetic-functions
  "Test type errors in arithmetic functions."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, +, !=)"
           "1+(x) : fails(x + 1)"
           "fails(x) : { x != x -> /context(self, catch); True -> /context(self, catch) }")

    (with-nodes ((1+ "1+")) modules
      (is (= nil (call-meta-node 1+ '(1))))
      (is-true (call-meta-node 1+ '("hello"))))))

(test call-meta-node-type-error-objects
  "Test type errors in objects."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, !=)"
           "test(x) : fails(x.key)"
           "fails(x) : { x != x -> /context(self, catch); True -> /context(self, catch) }")

    (with-nodes ((test "test")) modules
      (is-true (bool-value (call-meta-node test '(1))))

      (is-true (bool-value (call-meta-node test (list (make-hash-map)))))

      (is (= nil
             (->> (list (cons (id-symbol "key") 1))
                  alist-hash-map
                  list
                  (call-meta-node test)
                  bool-value))))))


;;; Test Actual Macros

(test macro-compile-time-computation
  "Test macro which performs computation at compile-time."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"

           "square(x) : x * x"
           "/attribute(square, macro, 1)"

           "a * square(3) -> b")

    (test-not-nodes modules
                    '(("/in" "core" "*") "a" ("square" 3))
                    '("square" 3))

    (with-nodes ((a "a") (a*9 (("/in" "core" "*") "a" 9))
                 (b "b")
                 (* "*"))
        modules

      (has-value-function (a) a*9 `(,* ,a 9))
      (test-simple-binding a*9 b))))

(test macro-quoted-expression
  "Test macro which returns quoted expression."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"

           "square(x) : list(/quote(*), x, x)"
           "/attribute(square, macro, 1)"

           "square(a) -> b")

    (test-not-nodes modules '("square" "a"))

    (with-nodes ((a "a") (b "b")
                 (a*a (("/in" "core" "*") "a" "a"))
                 (* "*"))
        modules

      (has-value-function (a) a*a `(,* ,a ,a))
      (test-simple-binding a*a b))))

(test macro-meta-node-reference
  "Test macro which returns expression with meta-node references."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"

           "square(x) : list(& *, x, x)"
           "/attribute(square, macro, 1)"

           "square(a) -> b")

    (test-not-nodes modules '("square" "a"))

    (with-nodes ((a "a") (b "b")
                 (a*a (("/in" "core" "*") "a" "a"))
                 (* "*"))
        modules

      (has-value-function (a) a*a `(,* ,a ,a))
      (test-simple-binding a*a b))))

(test macro-with-macros
  "Test expansion of macros in macro meta-nodes."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, ->, list, *)"

           "'(x) : list(/quote(/quote), x)"
           "/attribute(', macro, 1)"

           "square(x) : list('(*), x, x)"
           "/attribute(square, macro, 1)"

           "square(a) -> b")

    (test-not-nodes modules '("square" "a"))

    (with-nodes ((a "a") (b "b")
                 (a*a (("/in" "core" "*") "a" "a"))
                 (* "*"))
        modules

      (has-value-function (a) a*a `(,* ,a ,a))
      (test-simple-binding a*a b))))

(test macro-multiple-arguments
  "Test macros with multiple arguments."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, list, ->, if)"

           "'(x) : list(/quote(/quote), x)"
           "/attribute(', macro, 1)"

           "!-(a, b) : list('(if), a, b)"
           "/attribute(!-, macro, 1)"
           "/operator(!-, 25, left)"

           "a !- b -> out")

    (test-not-nodes modules '("!-" "a" "b"))

    (with-nodes ((a "a") (b "b") (out "out")
                 (a!-b (("/in" "builtin" "if") "a" "b"))
                 (if "if"))
        modules

      (has-value-function (a b) a!-b `(,if ,a ,b :none))
      (test-simple-binding a!-b out))))

(test macro-keyword-arguments
  "Test passing macro arguments by keyword"

  (with-module-table modules
    (build-core-module)
    (build "/import(core, list)"

           "f(x) : x"

           "call(operator, operand) : list(operator, operand)"
           "/attribute(call, macro, True)"

           "call(operand : in1, operator : f) -> out1")

    (test-not-nodes modules '("call" (":" "operand" "in1") (":" "operator" "f")))

    (with-nodes ((in1 "in1") (out1 "out1")
                 (f "f")
                 (f-in1 ("f" "in1")))
        modules

      (has-value-function (in1) f-in1
        `(,f ,in1))

      (test-simple-binding f-in1 out1))))

(test macro-arity-check-required-only
  "Test macro arity checks with required arguments only."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, *, list)"

           "square(x) : list(/quote(*), x, x)"
           "/attribute(square, macro, 1)")

    (signals arity-error (build "square(x, y) -> out"))))

(test macro-arity-check-optional-not-enough
  "Test macro optional argument arity checks with not enough arguments."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, +, list)"

           "add3(x, y, z : 1) : list(/quote(+), x, list(/quote(+), y, z))"
           "/attribute(add3, macro, 1)")

    (signals arity-error (build "add3(x)"))))

(test macro-arity-check-optional-too-many
  "Test macro optional argument arity checks with too many arguments."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, +, list)"

           "1+(n, d : 1) : list(/quote(+), x, d)"
           "/attribute(1+, macro, 1)")

    (signals arity-error (build "1+(x, y, z)"))))

(test macro-arity-check-rest-arguments
  "Test macro rest argument arity checks."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, cons, list)"
           "make-list(x, ..(xs)) : cons(/quote(list), cons(x, xs))"
           "/attribute(make-list, macro, 1)"

           "make-list(x, y, z) -> output"

           "/attribute(x, input, 1)"
           "/attribute(y, input, 1)"
           "/attribute(z, input, 1)")

    (with-nodes ((x "x") (y "y") (z "z")
                 (list "list")
                 (output "output"))
        (finish-build)

      (has-value-function
          (x y z)
          output

        `(,list ,(ir:argument-list (list x y z)))))))

(test macro-arity-check-keyword-missing-required
  "Test macro keyword argument arity check with missing required argument"

  (with-module-table modules
    (build-core-module)
    (build "/import(core, +, list)"

           "1+(n, d : 1) : list(/quote(+), x, d)"
           "/attribute(1+, macro, 1)")

    (signals arity-error (build "1+(d : 2)"))))

(test macro-arity-check-keyword-unknown
  "Test macro keyword argument arity check with unknown keyword"

  (with-module-table modules
    (build-core-module)
    (build "/import(core, +, list)"

           "1+(n, d : 1) : list(/quote(+), x, d)"
           "/attribute(1+, macro, 1)")

    (signals arity-error (build "1+(d : 2, n : 1, delta : 100)"))))

(test macro-rest-argument-outer-nodes
  "Test macros with rest arguments and outer nodes."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, cons, list)"
           "make-list(x, ..(xs)) : cons(/quote(list), cons(x, cons(y, xs)))"
           "/attribute(make-list, macro, 1)"

           "/attribute(a, input, 1)"
           "/attribute(b, input, 1)"
           "/attribute(c, input, 1)"
           "/attribute(y, input, 1)")

    (signals macro-outer-node-error (build "make-list(a, b, c) -> output"))))

(test macro-build-meta-node-multiple-times
  "Test building a meta-node multiple times when building macro."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, if, -, +, *, <)"
	   "fact(n) : { 1 -> start; iter(n, acc) : if(n < start, acc, iter(n - 1, acc * n)); iter(n,1) }"

	   "eval-fact(n) : fact(n)"
	   "/attribute(eval-fact, macro, 1)"

	   "fact(in) + eval-fact(3) -> output"
	   "/attribute(in, input, 1)")

    (with-nodes ((in "in") (output "output")
		 (fact "fact") (+ "+"))
	(finish-build)

      (has-value-function (in) output
        `(,+ (,fact ,in) 6))

      (with-nodes ((iter "iter") (n "n")) (definition fact)
        (has-value-function (n) fact
          `(,iter ,n 1))))))

(test macro-error-compile-loop
  "Test error when compilation loop detected in macro compilation."

  (with-module-table modules
    (build-core-module)
    (build "/import(core, list)"
           "test(x,y) : list(&(->), x, test(x,y))"
           "/attribute(test, macro, 1)")

    (with-nodes ((test "test")) modules
      (signals compile-meta-node-loop-error (call-meta-node test '(1 2))))))

(test macro-error-malformed-list
  "Test error when macro returns a malformed list."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"

	   "mac(x, y) : cons(x, y)"
	   "/attribute(mac, macro, 1)"

           "f(x) : x"
           "target-f(s, expr) : cons(s, head(tail(expr)))"
           "/attribute(f, target-transform, target-f)")

    (signals tridash-fail (build "mac(1, 2)"))
    (signals tridash-fail (build "a -> f(b)"))))

(test macro-error-return-empty-list-failure
  "Test error when macro returns empty list failure."

  (with-module-table modules
    (build-core-module)
    (build "/import(core)"

           "mac(x) : list(x, Empty!)"
           "/attribute(mac, macro, 1)"

           "f(x) : x"
           "target-f(s, expr) : list(s, Empty!)"
           "/attribute(f, target-transform, target-f)")

    (signals tridash-fail (build "mac(a)"))
    (signals tridash-fail (build "x -> f(y)"))))


;;; Test Target Node Transform Macros

(test target-transform-single-argument
  "Test target transformation with single argument."

  (with-module-table modules
    (build-core-module)
    (build-source-file #p"./test/inputs/macros/target-transform-1.trd" modules)

    (with-nodes ((in "in")
                 (out "out")
                 (int "int"))

        (finish-build)

      (has-value-function (in) out `(,int ,in)))))

(test target-transform-multiple-arguments
  "Test target transformation with multiple arguments."

  (with-module-table modules
    (build-core-module)
    (build-source-file #p"./test/inputs/macros/target-transform-2.trd" modules)

    (with-nodes ((in "in") (a "a") (b "b")
                 (- "-"))
        (finish-build)

      (has-value-function (in a) b `(,- ,in ,a)))))

(test target-transform-arity-check-not-enough
  "Test arity checks in target transform with not enough arguments."

  (with-module-table modules
    (build-core-module)
    (signals arity-error
      (build-source-file #p"./test/inputs/macros/target-transform-3.trd" modules))))

(test target-transform-arity-check-too-many
  "Test arity checks in target transform with too many arguments."

  (with-module-table modules
    (build-core-module)
    (signals arity-error
      (build-source-file #p"./test/inputs/macros/target-transform-4.trd" modules))))

(test target-transform-arity-check-rest-argument
  "Test arity checks in target transform with rest arguments."

  (with-module-table modules
    (build-core-module)
    (build-source-file #p"./test/inputs/macros/target-transform-5.trd" modules)

    (with-nodes ((in "in") (a "a") (b "b")
                 (- "-"))
        (finish-build)

      (has-value-function (in a) b `(,- ,in ,a)))))

(test target-transform-arity-check-optional-and-rest
  "Test arity checks in target transform with optional and rest arguments."

  (with-module-table modules
    (build-core-module)
    (build-source-file #p"./test/inputs/macros/target-transform-6.trd" modules)

    (with-nodes ((in "in") (a "a") (b "b")
                 (- "-"))
        (finish-build)

      (has-value-function (in a) b `(,- ,in ,a)))))

(test target-transform-arity-check-optional-extra
  "Test arity checks in target transform with optional extra arguments."

  (with-module-table modules
    (build-core-module)
    (build-source-file #p"./test/inputs/macros/target-transform-7.trd" modules)

    (with-nodes ((in "in") (a "a") (b "b")
                 (- "-"))
        (finish-build)

      (has-value-function (in a) b `(,- ,in ,a)))))


;;; Test Attribute Processor Nodes

(test attribute-processor-meta-node
  "Test attribute processor with meta-node."

  (with-module-table modules
    (build-source-file #p"./test/inputs/macros/attribute-processor-1.trd" modules)

    (with-nodes ((f "f") (match-f "match-f"))
        modules

      (is (eq match-f (attribute :matcher f))))))

(test attribute-processor-external-meta-node
  "Test attribute processor with external meta-node"

  (with-module-table modules
    (build-source-file #p"./test/inputs/macros/attribute-processor-2.trd" modules)

    (with-nodes ((f "f") (match-f "match-f"))
        modules

      (is (eq match-f (attribute :matcher f))))))
