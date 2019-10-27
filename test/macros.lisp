;;;; macros.lisp
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

;;;; User-Defined Macro Tests

(defpackage :tridash.test.macros
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

        :tridash.test.util
        :tridash.test.builder)

  (:shadowing-import-from :generic-cl
                          :emptyp
                          :multiply
                          :accumulate)

  (:shadowing-import-from :prove :fail)

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
                :empty-list
                :group-rest-args

                :check-arity
                :correct-arity?%
                :fail-arity-error

                :+optional-argument+
                :+rest-argument+))

(in-package :tridash.test.macros)

(in-readtable lol-syntax)

(defun functor (operator &rest arguments)
  "Creates a `FUNCTOR-EXPRESSION' with operator OPERATOR and arguments
   ARGUMENTS."

  (functor-expression operator arguments))

(defun expression= (got expected)
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
   `NODE-LINK' object corresponding to the operand, and the symbol
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
           ,@(map #`(,a1 (node-link (make-instance 'node :name ',a1)))
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
       (is (tridash->cl-function self)
           `(lambda ,',args
              (declare (ignorable ,@',(lambda-args args)))
              ,,body)
           :test #'expression=))))


(plan 6)

(subtest "Tridash to CL Compilation"
  (subtest "Functor Expressions"
    (with-core-nodes ("if" "<" "-")
      (test-compile-meta-node

       (a b)
       (functor if (functor < a b) (functor - b a) (functor - a b))

       ($a $b)
       '(let nil
         (!|if| (!< $a $b)
          (thunk (!- $b $a))
          (thunk (!- $a $b)))))))

  (subtest "If Expressions"
    (with-core-nodes ("<" "-")
      (test-compile-meta-node

       (a b)
       (if-expression (functor < a b) (functor - b a) (functor - a b))

       ($a $b)
       '(let nil
         (!|if| (!< $a $b)
          (thunk (!- $b $a))
          (thunk (!- $a $b)))))))

  (subtest "Object Expressions"
    (with-core-nodes ("+" "-")
      (test-compile-meta-node

       (x y)
       (object-expression
        `((sum ,(functor + x y))
          (diff ,(functor - x y))))

       ($x $y)
       '(let nil
         (alist-hash-map
          (list
           (cons 'sum (thunk (!+ $x $y)))
           (cons 'diff (thunk (!- $x $y)))))))))

  (subtest "Member Expressions"
    (test-compile-meta-node

     (object)
     (member-expression
      (member-expression object 'key1) 'key2)

     ($obj)
     '(let nil
       (!|member| (!|member| $obj 'key1) 'key2))))

  (subtest "Catch Expressions"
    (with-core-nodes ("/" "*")
      (test-compile-meta-node

       (a b)
       (catch-expression
        (functor / a b)
        (functor * a b))

       ($a $b)

       '(let nil
         (!|catch| (!/ $a $b) (thunk (!* $a $b)))))))

  (subtest "Fail Expressions"
    (test-compile-meta-node
     ()
     (fail-expression nil)

     ()
     '(let nil
       (!|fail|))))

  (subtest "Expression Blocks"
    (with-core-nodes ("+")
      (subtest "Count = 1"
        (test-compile-meta-node

         (a)
         (expression-block
          (functor + a 1))

         ($a)
         '(let nil
           (!+ $a 1))))

      (subtest "Count > 1"
        (test-compile-meta-node

         (a)
	 (let ((block (expression-block (functor + a 1) :count 2)))
           (functor + block block))

         ($a)
         '(let ($a+1)
           (setf $a+1 (thunk (!+ $a 1)))

           (!+ $a+1 $a+1))))))

  (subtest "Calling Other Meta-Nodes"
    (with-core-nodes ("-")
      (let ((meta-node (mock-meta-node (a) a)))
        (test-compile-meta-node

         (a)
         (functor meta-node (functor - a))

         ($a)
         `(let nil
            (call-tridash-meta-node ,meta-node (list (!- $a))))))))

  (subtest "Higher-Order Meta-Nodes"
    (subtest "External Meta-Node"
      (with-core-nodes ("not")
        (let ((apply (mock-meta-node (f x) (functor f x))))
          (test-compile-meta-node

           (x)
           (functor apply (meta-node-ref not) x)

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

    (subtest "If Meta-Node"
      (with-core-nodes ("if")
        (let ((apply (mock-meta-node (f x y z) (functor f x y z))))
          (test-compile-meta-node

           (x y z)
           (functor apply (meta-node-ref if) x y z)

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

    (subtest "And Meta-Node"
      (with-core-nodes ("and")
        (let ((apply (mock-meta-node (f x y) (functor f x y))))
          (test-compile-meta-node

           (x y)
           (functor apply (meta-node-ref and) x y)

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

    (subtest "Or Meta-Node"
      (with-core-nodes ("or")
        (let ((apply (mock-meta-node (f x y) (functor f x y))))
          (test-compile-meta-node

           (x y)
           (functor apply (meta-node-ref or) x y)

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

    (subtest "Tridash Meta-Node"
      (let ((apply (mock-meta-node (f x) (functor f x)))
            (f (mock-meta-node (x) x)))

        (test-compile-meta-node

         (x)
         (functor apply (meta-node-ref f) x)

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

    (subtest "Meta-Node with Optional Arguments"
      (let ((apply (mock-meta-node (f x) (functor f x)))
            (f (mock-meta-node (x (optional y) (optional z)) x)))

        (test-compile-meta-node
         (x)
         (functor apply (meta-node-ref f :optional (list 1 2)) x)

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

    (subtest "Meta-Node with Rest Arguments"
      (let ((apply (mock-meta-node (f x) (functor f x)))
            (f (mock-meta-node (x y (rest xs)) xs)))

        (test-compile-meta-node
         (x)
         (functor apply (meta-node-ref f) x)

         ($x)
         `(let nil
            (call-tridash-meta-node
             ,apply
             (list
              #'(lambda (&rest $args)
                  (if (correct-arity?% '(2) (length $args))
                      (destructuring-bind ($x2 $y &rest $xs &aux ($rest (or $xs (empty-list))))
                          $args
                        (call-tridash-meta-node ,f (list $x2 $y $rest)))
                      (fail-arity-error)))
              $x))))))

    (subtest "Invoking Nodes"
      (test-compile-meta-node

       (f x y)
       (functor f x y)

       ($f $x $y)
       `(let nil
          (call-node $f (list $x $y))))))

  (subtest "Literals"
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

  (subtest "Primitive Functions"
    (with-core-nodes
        ("+" "-" "*" "/"
             "<" "<=" ">" ">=" "=" "!="
             "and" "or" "not"
             "int?" "real?" "string?")

      (subtest "Arithmetic"
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
            (!- $d)))))

      (subtest "Comparison and Logical"
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
                      (thunk (!!= $x $y)))))))))))))

      (subtest "Type Checks"
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
               (thunk (!|string?| $z))))))))))

  (subtest "Tail Recursion"
    (subtest "If Expressions"
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

    (subtest "If Functor"
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

    (subtest "Expression Blocks"
      (with-core-nodes ("if" "-" "*" "<")
        (test-compile-meta-node

         (n acc)
         (functor if
                  (functor < n 2)
                  acc
                  (expression-block
                   (functor self (functor - n 1) (functor * n acc))))

         ($n $acc)
         `(let nil
            (!|if| (!< $n 2)
              $acc
              (thunk
               (call-tridash-meta-node ,self (list (!- $n 1) (!* $n $acc)))))))))

    (subtest "And/Or Functors"
      (with-core-nodes ("and" "or" "=" "-")
        (subtest "Or"
          (test-compile-meta-node

           (n)
           (functor or (functor = n 0) (functor self (functor - n 1)))

           ($n)
           `(let nil
              (!|or| (!= $n 0)
                (thunk
                 (call-tridash-meta-node ,self (list (!- $n 1))))))))

        (subtest "And"
          (test-compile-meta-node

           (n)
           (functor and (functor = n 0) (functor self (functor - n 1)))

           ($n)
           `(let nil
              (!|and| (!= $n 0)
                (thunk
                 (call-tridash-meta-node ,self (list (!- $n 1))))))))))

    (subtest "Catch Expressions"
      (with-core-nodes ("-" "+")
        (test-compile-meta-node

         (n)
         (catch-expression (functor self (functor + n 1))
                           (functor self (functor - n 1)))

         ($n)
         `(let nil
            (!|catch| (call-tridash-meta-node ,self (list (!+ $n 1)))
              (thunk
               (call-tridash-meta-node ,self (list (!- $n 1))))))))))

  (subtest "Optional and Rest Arguments"
    (subtest "Optional Arguments"
      (with-core-nodes ("+")
        (test-compile-meta-node
         (n (optional d 1))
         (functor + n d)

         ($n &optional ($d 1))
         '(let nil
           (!|+| $n $d)))))

    (subtest "Multiple Optional Arguments"
      (with-core-nodes ("+")
        (test-compile-meta-node
         (n (optional d 1) (optional e 2))
         (functor + n (functor + d e))

         ($n &optional ($d 1) ($e 2))
         '(let nil
           (!|+| $n (!|+| $d $e))))))

    (subtest "Rest Arguments"
      (with-core-nodes ("cons")
        (test-compile-meta-node
         (x (rest xs))
         (functor cons x xs)

         ($x &optional ($xs (empty-list)))
         '(let nil
           (!|cons| $x $xs)))))

    (subtest "Rest and Optional"
      (with-core-nodes ("cons")
        (test-compile-meta-node
         (x (optional y 2) (rest xs))
         (functor cons x (functor cons y xs))

         ($x &optional ($y 2) ($xs (empty-list)))
         '(let nil
           (!|cons| $x (thunk (!|cons| $y $xs))))))))

  (subtest "Errors"
    (subtest "Unsupported `EXTERNAL-META-NODE'"
      (with-external-meta-nodes ("not-a-function")
        (is-error
         (tridash->cl-function
          (mock-meta-node
           (arg)
           (functor not-a-function arg)))

         'unsupported-meta-node-error)))))

(subtest "Calling Tridash Meta-Nodes from CL"
  (subtest "Single Expression Functions"
    (with-module-table modules
      (build-core-module)
      (build ":import(core)"
             "min(x,y) : case(x < y : x, y)")

      (with-nodes ((min "min")) modules
        (is (resolve (call-tridash-meta-node min '(2 10))) 2)
        (is (resolve (call-tridash-meta-node min '(10 2))) 2)
        (is (resolve (call-tridash-meta-node min '(-5.3 7.6))) -5.3)
        (is (resolve (call-tridash-meta-node min '(1 1))) 1))))

  (subtest "Boolean Expressions"
    (subtest "If Expressions"
      (with-module-table modules
        (build-core-module)
        (build ":import(core)"
               "f(cond, x) : if(cond, x, 0)")

        (with-nodes ((f "f")) modules
          (is (resolve (call-tridash-meta-node f '(1 10))) 10)
          (is (resolve (call-tridash-meta-node f '(0 5))) 0))))

    (subtest "And Expressions"
      (with-module-table modules
        (build-core-module)
        (build ":import(core)"
               "f(cond, x) : cond and x")

        (with-nodes ((f "f")) modules
          (ok (bool-value (resolve (call-tridash-meta-node f '(1 10)))))
          (is (bool-value (resolve (call-tridash-meta-node f '(0 5)))) nil)
          (is (bool-value (resolve (call-tridash-meta-node f '(5 0)))) nil)
          (is (bool-value (resolve (call-tridash-meta-node f '(0 0)))) nil))))

    (subtest "Or Expressions"
      (with-module-table modules
        (build-core-module)
        (build ":import(core)"
               "f(cond, x) : cond or x")

        (with-nodes ((f "f")) modules
          (ok (bool-value (resolve (call-tridash-meta-node f '(1 10)))))
          (ok (bool-value (resolve (call-tridash-meta-node f '(0 5)))))
          (ok (bool-value (resolve (call-tridash-meta-node f '(5 0)))))
          (is (bool-value (resolve (call-tridash-meta-node f '(0 0)))) nil)))))

  (subtest "Multiple Nodes with CATCH-FAIL Expressions"
    (with-module-table modules
      (build-core-module)
      (build ":import(core)"
             "min(x,y) : { x < y -> (x -> :context(self,c)); y -> :context(self,c) }")

      (with-nodes ((min "min")) modules
        (is (resolve (call-tridash-meta-node min '(2 10))) 2)
        (is (resolve (call-tridash-meta-node min '(10 2))) 2)
        (is (resolve (call-tridash-meta-node min '(-5.3 7.6))) -5.3)
        (is (resolve (call-tridash-meta-node min '(1 1))) 1))))

  (subtest "Recursive Meta-Nodes"
    (with-module-table modules
      (build-core-module)
      (build ":import(core)"
             "fact(n) : { case(n < 2 : 1, n * fact(n - 1)) }")

      (with-nodes ((fact "fact")) modules
        (is (resolve (call-tridash-meta-node fact '(3))) 6)
        (is (resolve (call-tridash-meta-node fact '(5))) 120)
        (is (resolve (call-tridash-meta-node fact '(0))) 1))))

  (subtest "Tail-Recursive Meta-Nodes"
    (with-module-table modules
      (build-core-module)
      (build ":import(core)"
             "fact(n) : { iter(n,acc) : case(n < 2 : acc, iter(n - 1, n * acc)); iter(n, 1) }")

      (with-nodes ((fact "fact")) modules
        (is (resolve (call-tridash-meta-node fact '(3))) 6)
        (is (resolve (call-tridash-meta-node fact '(5))) 120)
        (is (resolve (call-tridash-meta-node fact '(0))) 1))))

  (subtest "Calling Other Meta-Nodes"
    (with-module-table modules
      (build-core-module)
      (build ":import(core)"
             "1-(n) : n - 1"
             "1+(n) : n + 1"
             "f(a, b) : 1-(a) * 1+(b)")

      (with-nodes ((f "f")) modules
        (is (resolve (call-tridash-meta-node f '(1 5))) 0)
        (is (resolve (call-tridash-meta-node f '(10 4))) 45)
        (is (resolve (call-tridash-meta-node f '(4 10))) 33))))

  (subtest "Nested Meta-Nodes"
    (with-module-table modules
      (build-core-module)
      (build ":import(core)"
             "f(x, y, z) : { g(n) : n - sum; x + y -> sum; g(z) }")

      (with-nodes ((f "f")) modules
        (is (resolve (call-tridash-meta-node f '(1 2 3))) 0)
        (is (resolve (call-tridash-meta-node f '(2 3 7))) 2))))

  (subtest "Rest and Optional Arguments"
    (subtest "Optional Arguments"
      (subtest "Without Default Values"
        (with-module-table modules
          (build-core-module)
          (build ":import(core, +, fail-type?)"
                 "inc(n, :(d)) : n + d"

                 "f(x) : inc(x)"
                 "g(x) : inc(x, 2)"

                 "h(x) : fail-type?(inc(x), &(No-Value%))")

          (with-nodes ((f "f") (g "g") (h "h")) modules
            (is-error (call-meta-node f (list 3)) tridash-fail)
            (is (call-meta-node g (list 5)) 7)

            (ok (call-meta-node h (list 2))))))

      (subtest "With Default Values"
        (with-module-table modules
          (build-core-module)
          (build ":import(core, +)"
                 "inc(n, d : 1) : n + d"

                 "f(x) : inc(x)"
                 "g(x) : inc(x, 2)")

          (with-nodes ((f "f") (g "g")) modules
            (is (call-meta-node f (list 3)) 4)
            (is (call-meta-node g (list 5)) 7)))))

    (subtest "Rest Arguments"
      (with-module-table modules
        (build-core-module)
        (build ":import(core, and, fails?)"
               "check(..(xs)) : fails?(xs)"

               "f(x) : x and check()"
               "g(x) : check(x)"
               "h(x) : check(x, 1, 2, 3)")

        (with-nodes ((f "f") (g "g") (h "h")) modules
          (ok (bool-value (call-meta-node f '(1))))
          (is (bool-value (call-meta-node g '(2))) nil)
          (is (bool-value (call-meta-node h '(2))) nil)))))

  (subtest "Higher Order Meta-Nodes"
    (subtest "No Outer-Node references"
      (with-module-table modules
        (build-core-module)
        (build ":import(core)"
               "apply(f, x) : f(x)"
               "1+(n) : n + 1"

               "f(a) : apply(..(not), a)"
               "g(a) : apply(..(1+), a)")

        (with-nodes ((f "f") (g "g")) modules
          (is (resolve (call-tridash-meta-node f '(0))) 1)
          (is (resolve (call-tridash-meta-node f '(1))) 0)

          (is (resolve (call-tridash-meta-node g '(1))) 2)
          (is (resolve (call-tridash-meta-node g '(3))) 4))))

    (subtest "With Optional Arguments"
      (subtest "With Default Values"
        (with-module-table modules
          (build-core-module)
          (build ":import(core)"
                 "apply(f, x) : f(x)"
                 "apply2(f, x, y) : f(x, y)"
                 "1+(n, d : 1) : n + d"

                 "f(a) : apply(1+, a)"
                 "g(a, b) : apply2(1+, a, b)")

          (with-nodes ((f "f") (g "g")) modules
            (is (resolve (call-tridash-meta-node f '(0))) 1)
            (is (resolve (call-tridash-meta-node f '(1))) 2)

            (is (resolve (call-tridash-meta-node g '(1 2))) 3)
            (is (resolve (call-tridash-meta-node g '(5 3))) 8))))

      (subtest "Without Default Values"
        (with-module-table modules
          (build-core-module)
          (build ":import(core)"
                 "apply(f, x) : f(x)"
                 "apply2(f, x, y) : f(x, y)"
                 "1+(n, :(d)) : n + d"

                 "f(a) : apply(1+, a)"
                 "g(a, b) : apply2(1+, a, b)"
                 "h(x) : fail-type?(apply(1+, x), &(No-Value%))")

          (with-nodes ((f "f") (g "g") (h "h")) modules
            (is-error (resolve (call-tridash-meta-node f '(0))) tridash-fail)

            (is (resolve (call-tridash-meta-node g '(1 2))) 3)
            (is (resolve (call-tridash-meta-node g '(5 3))) 8)

            (ok (resolve (call-tridash-meta-node h '(1))))))))

    (subtest "With Rest Arguments"
      (with-module-table modules
        (build-core-module)
        (build ":import(core)"
               "apply3(f, x, y, z) : f(x, y, z)"
               "apply(f, x) : f(x)"
               "l(x, ..(xs)) : cons(x + 1, xs)"

               "f(a, b, c) : apply3(l, a, b, c)"
               "g(x) : apply(l, x)")

        (with-nodes ((f "f") (g "g")) modules
          (is (resolve (call-tridash-meta-node f '(1 3 4))) '(2 3 4))
          (is (resolve (call-tridash-meta-node g '(1))) '(2))))

      (subtest "Empty Rest Arguments"
        (with-module-table modules
          (build-core-module)
          (build "apply(f, x) : f(x)"
                 "l(x, ..(xs)) : xs"

                 "f(a) : apply(l, a)")

          (with-nodes ((f "f")) modules
            (is-error (call-meta-node f '(1)) tridash-fail)))))

    (subtest "With Optional Arguments and Outer Node References"
      (with-module-table modules
	(build-core-module)
	(build ":import(core, +)"
	       "apply(f, x) : f(x)"
	       "test(a, x) : { f(y, d : 1) : y + d + x; apply(f, a) }")

	(with-nodes ((test "test")) modules
	  (is (call-meta-node test '(2 3)) 6))))

    (subtest "External Meta-Nodes"
      (with-module-table modules
        (build-core-module)
        (build ":import(core)"
               "apply(f, x) : f(x)"
               "apply2(f, x, y) : f(x, y)"

               "f(a) : apply(-, a)"
               "g(a, b) : apply2(-, a, b)")

        (with-nodes ((f "f") (g "g")) modules
          (is (resolve (call-tridash-meta-node f '(1))) -1)
          (is (resolve (call-tridash-meta-node f '(2))) -2)

          (is (resolve (call-tridash-meta-node g '(3 2))) 1)
          (is (resolve (call-tridash-meta-node g '(5 3))) 2))))

    (subtest "Errors"
      (with-module-table modules
        (build-core-module)
        (build ":import(core)"
               "apply(f, x) : f(x)"

               "x+(n) : n + ..(x)"
               "x"

               "f(a) : apply(..(x+), a)")

        (with-nodes ((f "f")) modules
          (is-error (resolve (call-tridash-meta-node f '(1))) semantic-error)))))

  (subtest "Primitive Functions"
    (subtest "Subtraction and Negation"
      (with-module-table modules
        (build-core-module)
        (build ":import(core, -)"
               "sub(a, b) : a - b"
               "neg(x) : -(x)")

        (with-nodes ((sub "sub") (neg "neg")) modules
          (is (call-tridash-meta-node sub '(5 2)) 3)
          (is (call-tridash-meta-node neg '(5)) -5)))))

  (subtest "Objects"
    (with-module-table modules
      (build "Person(first, last) : { first -> self.first; last -> self.last }"
             "get-first(p) : p.first"
             "get-last(p) : p.last")

      (with-nodes ((person "Person") (get-first "get-first") (get-last "get-last"))
          modules

        (let ((p (call-tridash-meta-node person '("John" "Doe"))))
          (is (call-tridash-meta-node get-first (list p)) "John")
          (is (call-tridash-meta-node get-last (list p)) "Doe")))))

  (subtest "Catching Failures"
    (subtest "In Operand"
      (with-module-table modules
        (build-core-module)
        (build ":import(core, and)"
               "fails(x) : { x and 0 -> :context(self, catch); 1 -> :context(self, catch) }")

        (with-nodes ((fails "fails")) modules
          (is (bool-value (resolve (call-tridash-meta-node fails '(1)))) nil)

          (ok
           (->> (thunk (error 'tridash-fail))
                list
                (call-tridash-meta-node fails)
                resolve
                bool-value)))))

    (subtest "In Operator"
      ;; Test that failures in the operator of a functor are caught.
      (with-module-table modules
        (build-core-module)
        (build ":import(core, and, >, -)"
               "neg(x) : -(x)"

               "getf(f, x) : { x > 0 -> (f -> self) }"
               "test(x) : fails((getf(neg, x))(x))"

               "fails(x) : { x and 0 -> :context(self, catch); 1 -> :context(self, catch) }")

        (with-nodes ((test "test")) modules
          (is (bool-value (resolve (call-tridash-meta-node test '(1)))) nil)
          (ok (bool-value (resolve (call-tridash-meta-node test '(-1))))))))

    (subtest "Failure Types"
      (with-module-table modules
        (build-core-module modules)
        (build-source-file "./test/inputs/macros/failure-types.trd" modules)

        (with-nodes ((check-range "check-range")) modules
          (is (call-meta-node check-range '(2 1 3)) "")
          (is (call-meta-node check-range '(0 1 3)) "Error: below minimum!")
          (is (call-meta-node check-range '(10 2 7)) "Error: above maximum!")))))

  (subtest "Common Sub-Expressions"
    (subtest "One Expression-Block"
      (with-module-table modules
        (build-core-module)
        (build ":import(core, +)"
               "f(x) : (x + 1) + (x + 1)")

        (with-nodes ((f "f")) modules
          (is (call-meta-node f '(1)) 4)
          (is (call-meta-node f '(2)) 6))))

    (subtest "Two Expression-Block"
      (with-module-table modules
        (build-core-module)
        (build ":import(core, *, +, -)"
               "f(x, y) : { x + 1 -> x1; y + 2 -> y2; (x1 + y2) * (x1 - y2) }")

        (with-nodes ((f "f")) modules
          (is (call-meta-node f '(3 7)) -65)
          (is (call-meta-node f '(5 2)) 20)))))

  (subtest "Errors"
    (subtest "Type Errors"
      (subtest "Arithmetic Functions"
        (with-module-table modules
          (build-core-module)
          (build ":import(core, +, and)"
                 "1+(x) : fails(x + 1)"
                 "fails(x) : { x and 0 -> :context(self, catch); 1 -> :context(self, catch) }")

          (with-nodes ((1+ "1+")) modules
            (is (bool-value (resolve (call-tridash-meta-node 1+ '(1)))) nil)
            (ok (bool-value (resolve (call-tridash-meta-node 1+ '("hello"))))))))

      (subtest "Objects"
        (with-module-table modules
          (build-core-module)
          (build ":import(core, and)"
                 "test(x) : fails(x.key)"
                 "fails(x) : { x and 0 -> :context(self, catch); 1 -> :context(self, catch) }")

          (with-nodes ((test "test")) modules
            (subtest "Non-Object Type"
              (ok (bool-value (resolve (call-tridash-meta-node test '(1))))))

            (subtest "Non-Existent Entry"
              (ok (bool-value (resolve (call-tridash-meta-node test (list (make-hash-map)))))))

            (subtest "Object with existing Entry"
              (is (->> (list (cons (id-symbol "key") 1))
                       alist-hash-map
                       list
                       (call-tridash-meta-node test)
                       bool-value)
                  nil))))))))

(subtest "Actual Macros"
  (subtest "Compile-Time Computations"
    (with-module-table modules
      (build-core-module)
      (build ":import(core)"

             "square(x) : x * x"
             ":attribute(square, macro, 1)"

             "a * square(3) -> b")

      (test-not-nodes modules
                      '((":in" "core" "*") "a" ("square" 3))
                      '("square" 3))

      (with-nodes ((a "a") (a*9 ((":in" "core" "*") "a" 9))
                   (b "b")
                   (* "*"))
          modules

        (has-value-function (a) a*9 `(,* ,a 9))
        (test-simple-binding a*9 b))))

  (subtest "Generating Expressions"
    (subtest "Quoted Expressions"
      (with-module-table modules
        (build-core-module)
        (build ":import(core)"

               "square(x) : list(:quote(*), x, x)"
               ":attribute(square, macro, 1)"

               "square(a) -> b")

        (test-not-nodes modules '("square" "a"))

        (with-nodes ((a "a") (b "b")
                     (a*a ((":in" "core" "*") "a" "a"))
                     (* "*"))
            modules

          (has-value-function (a) a*a `(,* ,a ,a))
          (test-simple-binding a*a b))))

    (subtest "Meta-Node References"
      (with-module-table modules
        (build-core-module)
        (build ":import(core)"

               "square(x) : list(&(*), x, x)"
               ":attribute(square, macro, 1)"

               "square(a) -> b")

        (test-not-nodes modules '("square" "a"))

        (with-nodes ((a "a") (b "b")
                     (a*a ((":in" "core" "*") "a" "a"))
                     (* "*"))
            modules

          (has-value-function (a) a*a `(,* ,a ,a))
          (test-simple-binding a*a b)))))

  (subtest "Macros in Macros"
    (with-module-table modules
      (build-core-module)
      (build ":import(core, ->, list, *)"

             "'(x) : list(:quote(:quote), x)"
             ":attribute(', macro, 1)"

             "square(x) : list('(*), x, x)"
             ":attribute(square, macro, 1)"

             "square(a) -> b")

      (test-not-nodes modules '("square" "a"))

      (with-nodes ((a "a") (b "b")
                   (a*a ((":in" "core" "*") "a" "a"))
                   (* "*"))
          modules

        (has-value-function (a) a*a `(,* ,a ,a))
        (test-simple-binding a*a b))))

  (subtest "Macros with Multiple Arguments"
    (with-module-table modules
      (build-core-module)
      (build ":import(core, list, ->, if)"

             "'(x) : list(:quote(:quote), x)"
             ":attribute(', macro, 1)"

             "!-(a, b) : list('(if), a, b)"
             ":attribute(!-, macro, 1)"
             ":op(!-, 25, left)"

             "a !- b -> out")

      (test-not-nodes modules '("!-" "a" "b"))

      (with-nodes ((a "a") (b "b") (out "out")
                   (a!-b ((":in" "builtin" "if") "a" "b"))
                   (if "if"))
          modules

        (has-value-function (a b) a!-b `(,if ,a ,b nil))
        (test-simple-binding a!-b out))))

  (subtest "Arity Checks"
    (subtest "Required Only"
      (with-module-table modules
        (build-core-module)
        (build ":import(core, *, list)"

               "square(x) : list(:quote(*), x, x)"
               ":attribute(square, macro, 1)")

        (is-error (build "square(x, y) -> out") arity-error)))

    (subtest "Optional Arguments"
      (subtest "Not Enough"
        (with-module-table modules
          (build-core-module)
          (build ":import(core, +, list)"

                 "add3(x, y, z : 1) : list(:quote(+), x, list(:quote(+), y, z))"
                 ":attribute(add3, macro, 1)")

          (is-error (build "add3(x)") arity-error)))

      (subtest "Too Many"
        (with-module-table modules
          (build-core-module)
          (build ":import(core, +, list)"

                 "1+(n, d : 1) : list(:quote(+), x, d)"
                 ":attribute(1+, macro, 1)")

          (is-error (build "1+(x, y, z)") arity-error))))

    (subtest "Rest Arguments"
      (with-module-table modules
        (build-core-module)
        (build ":import(core, cons, list)"
               "make-list(x, ..(xs)) : cons(:quote(list), cons(x, xs))"
               ":attribute(make-list, macro, 1)"

               "make-list(x, y, z) -> output"

               ":attribute(x, input, 1)"
               ":attribute(y, input, 1)"
               ":attribute(z, input, 1)")

        (with-nodes ((x "x") (y "y") (z "z")
                     (list "list")
                     (output "output"))
            (finish-build)

          (has-value-function
              (x y z)
              output

            `(,list ,(argument-list (list x y z)))))))

    (subtest "Rest Arguments and Outer Nodes"
      (with-module-table modules
        (build-core-module)
        (build ":import(core, cons, list)"
               "make-list(x, ..(xs)) : cons(:quote(list), cons(x, cons(y, xs)))"
               ":attribute(make-list, macro, 1)"

               ":attribute(a, input, 1)"
               ":attribute(b, input, 1)"
               ":attribute(c, input, 1)"
               ":attribute(y, input, 1)")

        (is-error (build "make-list(a, b, c) -> output") macro-outer-node-error))))

  (subtest "Building a Meta-Node Multiple Times"
    (with-module-table modules
      (build-core-module)
      (build ":import(core, if, -, +, *, <)"
	     "fact(n) : { 1 -> start; iter(n, acc) : if(n < start, acc, iter(n - 1, acc * n)); iter(n,1) }"

	     "eval-fact(n) : fact(n)"
	     ":attribute(eval-fact, macro, 1)"

	     "fact(in) + eval-fact(3) -> output"
	     ":attribute(in, input, 1)")

      (with-nodes ((in "in") (output "output")
		   (fact "fact") (+ "+"))
	  (finish-build)

	(has-value-function (in) output
          `(,+ (,fact ,in) 6))

	(with-nodes ((iter "iter") (n "n")) (definition fact)
          (has-value-function (n) fact
            `(,iter ,n 1))))))

  (subtest "Errors"
    (subtest "Compilation Loops"
      (with-module-table modules
        (build-core-module)
        (build ":import(core, list)"
               "test(x,y) : list(&(->), x, test(x,y))"
               ":attribute(test, macro, 1)")

        (with-nodes ((test "test")) modules
          (is-error (call-meta-node test '(1 2)) compile-meta-node-loop-error))))))

(subtest "Target Node Transforms"
  (subtest "Single Argument"
    (with-module-table modules
      (build-core-module)
      (build-source-file #p"./test/inputs/macros/target-transform-1.trd" modules)

      (with-nodes ((in "in")
                   (out "out")
                   (int "int"))

          (finish-build)

        (has-value-function (in) out `(,int ,in)))))

  (subtest "Multiple Arguments"
    (with-module-table modules
      (build-core-module)
      (build-source-file #p"./test/inputs/macros/target-transform-2.trd" modules)

      (with-nodes ((in "in") (a "a") (b "b")
                   (- "-"))
          (finish-build)

        (has-value-function (in a) b `(,- ,in ,a)))))

  (subtest "Arity Checks"
    (subtest "Required Only"
      (subtest "Not Enough"
        (with-module-table modules
          (build-core-module)
          (is-error
           (build-source-file #p"./test/inputs/macros/target-transform-3.trd" modules)
           arity-error)))

      (subtest "Too Many"
        (with-module-table modules
          (build-core-module)
          (is-error
           (build-source-file #p"./test/inputs/macros/target-transform-4.trd" modules)
           arity-error))))

    (subtest "Rest Arguments"
      (with-module-table modules
        (build-core-module)
        (build-source-file #p"./test/inputs/macros/target-transform-5.trd" modules)

        (with-nodes ((in "in") (a "a") (b "b")
                     (- "-"))
            (finish-build)

          (has-value-function (in a) b `(,- ,in ,a)))))

    (subtest "Optional and Rest Arguments"
      (with-module-table modules
        (build-core-module)
        (build-source-file #p"./test/inputs/macros/target-transform-6.trd" modules)

        (with-nodes ((in "in") (a "a") (b "b")
                     (- "-"))
            (finish-build)

          (has-value-function (in a) b `(,- ,in ,a)))))

    (subtest "Optional Extra Arguments"
      (with-module-table modules
        (build-core-module)
        (build-source-file #p"./test/inputs/macros/target-transform-7.trd" modules)

        (with-nodes ((in "in") (a "a") (b "b")
                     (- "-"))
            (finish-build)

          (has-value-function (in a) b `(,- ,in ,a)))))))

(subtest "Attribute Processor Nodes"
  (subtest "Tridash Meta-Node"
    (with-module-table modules
      (build-source-file #p"./test/inputs/macros/attribute-processor-1.trd" modules)

      (with-nodes ((f "f") (match-f "match-f"))
          modules

        (is (attribute :matcher f) match-f :test #'eq))))

  (subtest "External Meta-Node"
    (with-module-table modules
      (build-source-file #p"./test/inputs/macros/attribute-processor-2.trd" modules)

      (with-nodes ((f "f") (match-f "match-f"))
          modules

        (is (attribute :matcher f) match-f :test #'eq)))))

(subtest "Core Module"
  (subtest "Utilities"
    (subtest "Meta-Node: fails?"
      (with-module-table modules
        (build-core-module)
        (build ":import(core, fails?)")

        (with-nodes ((fails? "fails?")) modules
          (is (bool-value (call-meta-node fails? '(1))) nil)
          (ok (bool-value (call-meta-node fails? (list (fail-thunk))))))))

    (subtest "Meta-Node: fail-type?"
      (with-module-table modules
        (build-core-module)

        (with-nodes ((fail-type? "fail-type?")) modules
          (is (bool-value (call-meta-node fail-type? '(1 x))) nil)
          (is (bool-value (call-meta-node fail-type? (list (fail-thunk 'y) 'x))) nil)
          (ok (bool-value (call-meta-node fail-type? (list (fail-thunk 'x) 'x)))))))

    (subtest "Meta-Node: ?"
      (with-module-table modules
        (build-core-module)
        (build ":import(core, ?)")

        (with-nodes ((? "?")) modules
          (ok (bool-value (call-meta-node ? '(1))))
          (is (bool-value (call-meta-node ? (list (fail-thunk)))) nil))))

    (subtest "Meta-Node: !!"
      (with-module-table modules
        (build-core-module)

        (with-nodes ((!! "!!")) modules
          (ok (bool-value (call-meta-node !! '(1))))
          (is-error (call-meta-node !! (list (fail-thunk))) tridash-fail))))

    (subtest "Meta-Node: !-"
      (with-module-table modules
        (build-core-module)

        (with-nodes ((!- "!-")) modules
          (is (call-meta-node !- '(1 2)) 2)
          (is-error (call-meta-node !- (list (fail-thunk) 10)) tridash-fail)))))

  (subtest "Lists"
    (with-module-table modules
      (build-core-module)

      (subtest "Meta-Node: list"
        (build ":import(core, list, +)")

        (with-nodes ((list "list")) modules
          (is (call-meta-node list '(1 2 3)) '(1 2 3))

          (ok (resolve% (call-tridash-meta-node list (list (list 1 2 (fail-thunk))))))
          (ok (resolve% (call-tridash-meta-node list (list (list 1 (fail-thunk) 2)))))
          (ok (resolve% (call-tridash-meta-node list (list (list (fail-thunk) 1 2)))))))

      (subtest "Meta-Node: list*"
        (build ":import(core, list*)")

        (with-nodes ((list* "list*")) modules
          (is (call-meta-node list* '(1 2 (3 4 5))) '(1 2 3 4 5))
          (is (call-meta-node list* '((1 2 3))) '(1 2 3))))

      (subtest "Meta-Node: list!"
        (build ":import(core, list!)")

        (with-nodes ((list! "list!")) modules
          (is (call-meta-node list! '(1 2 3)) '(1 2 3))

          (is-error
           (resolve% (call-tridash-meta-node list! (list (list 1 2 (fail-thunk)))))
           tridash-fail)

          (is-error
           (resolve% (call-tridash-meta-node list! (list (list 1 (fail-thunk) 2))))
           tridash-fail)

          (is-error
           (resolve% (call-tridash-meta-node list! (list (list (fail-thunk) 1 2))))
           tridash-fail)))

      (subtest "Meta-Node: nth"
        (build ":import(core, nth)")

        (with-nodes ((nth "nth")) modules
          (is (call-meta-node nth '((1 2 3) 0)) 1)
          (is (call-meta-node nth '((1 2 3) 1)) 2)
          (is (call-meta-node nth '((1 2 3) 2)) 3)
          (is-error (call-meta-node nth '((1 2 3) 3)) tridash-fail)))

      (subtest "Meta-Node: append"
        (build ":import(core, append)")

        (with-nodes ((append "append")) modules
          (is (call-meta-node append '((1 2 3) (4 5 6))) '(1 2 3 4 5 6))
          (is (call-meta-node append (list '(1 2 3) (empty-list))) '(1 2 3))
          (is (call-meta-node append (list (empty-list) '(1 2 3))) '(1 2 3))

          (is-error (call-meta-node append (list '(1 2 3) (fail-thunk))) tridash-fail)
          (is-error (call-meta-node append (list (fail-thunk) '(1 2 3))) tridash-fail)))

      (subtest "Meta-Node: foldl'"
        (build ":import(core, foldl', /)")

        (with-nodes ((foldl "foldl'") (/ "/")) modules
          ;; Use division '/' as it is non-commutative. This ensures
          ;; that the result is passed in the first argument and list
          ;; element in the second argument.

          (is (call-meta-node foldl (list 24 / '(4 3 2))) 1)
          (is (call-meta-node foldl (list 24 / '(4 3))) 2)
          (is (call-meta-node foldl (list 24 / nil)) 24)
          (is (call-meta-node foldl (list 24 / (empty-list))) 24)

          (is-error (call-meta-node foldl (list 24 / (fail-thunk))) tridash-fail)))

      (subtest "Meta-Node: foldl"
        (build ":import(core, foldl, /)")

        (with-nodes ((foldl "foldl") (/ "/")) modules
          ;; Use division '/' as it is non-commutative. This ensures
          ;; that the result is passed in the first argument and list
          ;; element in the second argument.

          (is (call-meta-node foldl (list / '(24 4 3 2))) 1)
          (is (call-meta-node foldl (list / '(24 4 3))) 2)
          (is (call-meta-node foldl (list / '(24))) 24)

          (is-error (call-meta-node foldl (list / (empty-list))) tridash-fail)
          (is-error (call-meta-node foldl (list / (fail-thunk))) tridash-fail)))

      (subtest "Meta-Node: foldr"
        (build ":import(core, foldr, list)")

        (with-nodes ((foldr "foldr") (list "list")) modules
          (is (call-meta-node foldr (list list '(1 2 3 4 5))) '(1 (2 (3 (4 5)))))
          (is (call-meta-node foldr (list list '(1 2 3 4 5) 6)) '(1 (2 (3 (4 (5 6))))))

          (handler-bind
              ((tridash-fail
                (lambda (c)
                  (when (= (fail-type c) tridash.frontend::+empty-list+)
                    (replace-failure nil)))))
            (is (call-meta-node foldr (list list '(1 2 3 4 5) (empty-list)))
                '(1 (2 (3 (4 (5 ())))))))

          (is (call-meta-node foldr (list list '(1))) 1)
          (is (call-meta-node foldr (list list (empty-list) 1)) 1)
          (is-error (call-meta-node foldr (list list (empty-list))) tridash-fail)


          (is-error (call-meta-node foldr (list list (fail-thunk) 1)) tridash-fail)
          (is-error (call-meta-node foldr (list list (fail-thunk))) tridash-fail)))

      (subtest "Meta-Node: map"
        (build ":import(core, map, +)"
               "1+(n) : n + 1")

        (with-nodes ((map "map") (1+ "1+")) modules
          (is (call-meta-node map (list 1+ '(1 2 3 4))) '(2 3 4 5))
          (is (call-meta-node map (list 1+ '(1))) '(2))

          (is-error (call-meta-node map (list 1+ (empty-list))) tridash-fail)
          (is-error (call-meta-node map (list 1+ (fail-thunk))) tridash-fail)))

      (subtest "Meta-Node: filter"
        (build ":import(core, filter, >)"
               ">5(n) : n > 5")

        (with-nodes ((filter "filter") (>5 ">5")) modules
          (is (call-meta-node filter (list >5 '(2 7 1 3 9 0))) '(7 9))
          (is-error (call-meta-node filter (list >5 '(1 2 3))) tridash-fail)

          (is-error (call-meta-node filter (list >5 (empty-list))) tridash-fail)
          (is-error (call-meta-node filter (list >5 (fail-thunk))) tridash-fail)))

      (subtest "List Predicates"
        (build ":import(core, >)"
               ">3(n) : n > 3")

        (with-nodes ((>3 ">3")) modules

          (subtest "Meta-Node: every?"
            (build ":import(core, every?)")

            (with-nodes ((every? "every?")) modules
              (ok (bool-value (call-meta-node every? (list >3 '(4 5 6)))))
              (is (bool-value (call-meta-node every? (list >3 '(1 2 3 4 5 6)))) nil)
              (ok (bool-value (call-meta-node every? (list >3 (empty-list)))))

              (is-error (bool-value (call-meta-node every? (list >3 (fail-thunk)))) tridash-fail)))

          (subtest "Meta-Node: some?"
            (build ":import(core, some?)")

            (with-nodes ((some? "some?")) modules
              (ok (bool-value (call-meta-node some? (list >3 '(4 5 6)))))
              (ok (bool-value (call-meta-node some? (list >3 '(1 2 3 4 5 6)))) nil)
              (is (bool-value (call-meta-node some? (list >3 '(0 1 2)))) nil)
              (is (bool-value (call-meta-node some? (list >3 (empty-list)))) nil)

              (is-error (bool-value (call-meta-node some? (list >3 (fail-thunk)))) tridash-fail)))

          (subtest "Meta-Node: not-any?"
            (build ":import(core, not-any?)")

            (with-nodes ((not-any? "not-any?")) modules
              (is (bool-value (call-meta-node not-any? (list >3 '(4 5 6)))) nil)
              (is (bool-value (call-meta-node not-any? (list >3 '(1 2 3 4 5 6)))) nil)
              (ok (bool-value (call-meta-node not-any? (list >3 '(1 2 3)))))
              (ok (bool-value (call-meta-node not-any? (list >3 (empty-list)))))

              (is-error (bool-value (call-meta-node not-any? (list >3 (fail-thunk)))) tridash-fail)))

          (subtest "Meta-Node: not-every?"
            (build ":import(core, not-every?)")

            (with-nodes ((not-every? "not-every?")) modules
              (is (bool-value (call-meta-node not-every? (list >3 '(4 5 6)))) nil)
              (ok (bool-value (call-meta-node not-every? (list >3 '(1 2 3 4 5 6)))))
              (ok (bool-value (call-meta-node not-every? (list >3 '(0 1 2)))))
              (is (bool-value (call-meta-node not-every? (list >3 (empty-list)))) nil)

              (is-error (bool-value (call-meta-node not-every? (list >3 (fail-thunk)))) tridash-fail)))))))

  (subtest "Introspection"
    (with-module-table modules
      (build-core-module)
      (build ":import(core, node?, find-node, get-attribute, +)")

      (subtest "Meta-Node: node?"
        (build "x")
        (with-nodes ((node? "node?") (x "x")) modules
          (ok (bool-value (call-meta-node node? (list x))))
          (is (bool-value (call-meta-node node? '(x))) nil)))

      (subtest "Meta-Node: find-node"
        (build "x + y")

        (with-modules ((init :init)) modules
          (let ((tridash.frontend::*functor-module* init)
                (tridash.frontend::*current-module* init))
            (with-nodes ((find-node "find-node")
                         (x "x")
                         (x+y ((":in" "core" "+") "x" "y")))
                modules

              (is (call-meta-node find-node (list (id-symbol "x"))) x)
              (is (call-meta-node find-node (list (name x+y))) x+y)
              (is-error (call-meta-node find-node (list (id-symbol "z"))) tridash-fail)))))

      (subtest "Meta-Node: get-attribute"
        (build ":attribute(x, my-key, my-value)")
        (with-nodes ((get-attribute "get-attribute") (x "x"))
            modules

          (is (call-meta-node get-attribute (list x (id-symbol "my-key")))
              (id-symbol "my-value"))

          (is-error (call-meta-node get-attribute (list x (id-symbol "not-a-key")))
                    tridash-fail)))))

  (subtest "Pattern Matching"
    (with-module-table modules
      (build-core-module)

      (subtest "Match Integer"
        (build ":import(core, int)")
        (build "f-int(x) : { x -> int(y); y }")

        (with-nodes ((f-int "f-int")) modules
          (is (call-meta-node f-int '(1)) 1)
          (is-error (call-meta-node f-int '(2.3)) tridash-fail)
          (is-error (call-meta-node f-int '("hello")) tridash-fail)))

      (subtest "Match Real"
        (build ":import(core, real)")
        (build "f-real(x) : { x -> real(y); y }")

        (with-nodes ((f-real "f-real")) modules
          (is (call-meta-node f-real '(1)) 1)
          (is (call-meta-node f-real '(2.3)) 2.3)
          (is-error (call-meta-node f-real '("hello")) tridash-fail)))

      (subtest "Match String"
        (build ":import(core, string)")
        (build "f-string(x) : { x -> string(y); y }")

        (with-nodes ((f-string "f-string")) modules
          (is (call-meta-node f-string '("hello")) "hello")
          (is-error (call-meta-node f-string '(1)) tridash-fail)
          (is-error (call-meta-node f-string '(2.3)) tridash-fail)))

      (subtest "Match Cons"
        (build ":import(core, cons, list)"
               "f-cons(x) : { x -> cons(h,t); list(h,t) }")

        (with-nodes ((f-cons "f-cons")) modules
          (is (call-meta-node f-cons '((1 2 3))) '(1 (2 3)))
          (is-error (call-meta-node f-cons '(1)) tridash-fail)
          (is-error (call-meta-node f-cons '("hello")) tridash-fail)))

      (subtest "Match List"
        (build ":import(core, list, +)"
               "f-list(x) : { x -> list(a, b, c); a + b + c }")

        (with-nodes ((f-list "f-list")) modules
          (is (call-meta-node f-list '((1 2 3))) 6)
          (is-error (call-meta-node f-list '((1 2))) tridash-fail)
          (is-error (call-meta-node f-list '((1 2 3 4))) tridash-fail)

          (is-error (call-meta-node f-list '(1)) tridash-fail)
          (is-error (call-meta-node f-list '("hello")) tridash-fail)))

      (subtest "Match List*"
        (build ":import(core, list, list*, +)"
               "f-list*(x) : { x -> list*(a, b, xs); list*(a + b, xs) }")

        (with-nodes ((f-list* "f-list*")) modules
          (is (call-meta-node f-list* '((1 2 3))) '(3 3))
          (is (call-meta-node f-list* '((1 2))) '(3))
          (is (call-meta-node f-list* '((1 2 3 4))) '(3 3 4))

          (is-error (call-meta-node f-list* '((1))) tridash-fail)
          (is-error (call-meta-node f-list* '(1)) tridash-fail)
          (is-error (call-meta-node f-list* '("hello")) tridash-fail)))

      (subtest "Boolean Logic"
        (subtest "Match and"
          (build ":import(core, list, and, +)"
                 "f-and(xs) : { xs -> list(a, 1 and b); xs -> list(a and b); a + b }")

          (with-nodes ((f-and "f-and")) modules
            (is (call-meta-node f-and '((2 1))) 3)
            (is (call-meta-node f-and '((2))) 4)

            (is-error (call-meta-node f-and '((2 2))) tridash-fail)))

        (subtest "Match or"
          (build ":import(core, list, or)"
                 "f-or(xs) : { xs -> list(a) or list(1, a); a }")

          (with-nodes ((f-or "f-or")) modules
            (is (call-meta-node f-or '((1))) 1)
            (is (call-meta-node f-or '((2))) 2)
            (is (call-meta-node f-or '((1 10))) 10)

            (is-error (call-meta-node f-or '((1 2 3))) tridash-fail)
            (is-error (call-meta-node f-or '((2 10))) tridash-fail)))

        (subtest "Match not"
          (build ":import(core, list, not, and)"
                 "f-not(xs) : { xs -> list(1, not(2) and x); x }")

          (with-nodes ((f-not "f-not")) modules
            (is (call-meta-node f-not '((1 1))) 1)
            (is (call-meta-node f-not '((1 3))) 3)

            (is-error (call-meta-node f-not '((1 2))) tridash-fail)
            (is-error (call-meta-node f-not '((2 3))) tridash-fail))))

      (subtest "Nested and Multiple Patterns"
        (build-source-file #p"./test/inputs/macros/pattern-match-nested.trd" modules)

        (with-nodes ((calc "calc")) modules
          (is (call-meta-node calc (decls '(!|add| 1 2))) 3)
          (is (call-meta-node calc (decls '(!|sub| 3 0.5))) 2.5)
          (is (call-meta-node calc (decls '(!|neg| 5))) -5)

          (is-error (call-meta-node calc '((1 2 3))) tridash-fail)
          (is-error (call-meta-node calc (decls '(!|add| 1))) tridash-fail)
          (is-error (call-meta-node calc (decls '(!|add| "x" "y"))) tridash-fail)
          (is-error (call-meta-node calc '("x")) tridash-fail)))

      (subtest "Similar Patterns"
        (build-source-file #p"./test/inputs/macros/pattern-match-similar.trd" modules)

        (with-nodes ((calc3 "calc3")) modules
          (is (call-meta-node calc3 (decls '(!|add| 1 2))) 3)
          (is (call-meta-node calc3 (decls '(!|sub| 3 0.5))) 2.5)
          (is (call-meta-node calc3 (decls '(!|sub| 5))) 4)

          (is-error (call-meta-node calc3 '((1 2 3))) tridash-fail)
          (is-error (call-meta-node calc3 (decls '(!|add| 1))) tridash-fail)
          (is-error (call-meta-node calc3 (decls '(!|add| "x" "y"))) tridash-fail)
          (is-error (call-meta-node calc3 '("x")) tridash-fail)))

      (subtest "Constant Patterns"
        (build-source-file #p"./test/inputs/macros/pattern-match-constant.trd" modules)

        (with-nodes ((calc2 "calc2")) modules
          (is (call-meta-node calc2 '((0 1 2))) 3)
          (is (call-meta-node calc2 '((0.5 3 0.5))) 2.5)
          (is (call-meta-node calc2 '(("neg" 5))) -5)
          (is (call-meta-node calc2 '((#\- 9))) -9)

          (is-error (call-meta-node calc2 '((1 2 3))) tridash-fail)
          (is-error (call-meta-node calc2 (decls '(0 1))) tridash-fail)
          (is-error (call-meta-node calc2 (decls '(0 "x" "y"))) tridash-fail)
          (is-error (call-meta-node calc2 '("x")) tridash-fail)))))

  (subtest "Utility Macros"
    (with-module-table modules
      (build-core-module)

      (subtest "Case Macro"
        (build ":import(core, case, =, ')"
               "test-case(x) : case(x = 1 : '(a), x = 2 : '(b), '(other))")

        (with-nodes ((test-case "test-case")) modules
          (is (call-meta-node test-case '(1)) (id-symbol "a"))
          (is (call-meta-node test-case '(2)) (id-symbol "b"))
          (is (call-meta-node test-case '(3)) (id-symbol "other"))))

      (subtest "Case Macro Without Default"
        (build ":import(core, case, =, ')"
               "test-case2(x) : case(x = 1 : '(a), x = 2 : '(b))")

        (with-nodes ((test-case2 "test-case2")) modules
          (is (call-meta-node test-case2 '(1)) (id-symbol "a"))
          (is (call-meta-node test-case2 '(2)) (id-symbol "b"))
          (is-error (call-meta-node test-case2 '(3)) tridash-fail)))

      (subtest "! Macro"
        (build ":import(core, cons, !)"
               "cons!(x, y) : !(cons(x, y))")

        (with-nodes ((cons! "cons!")) modules
          (is (call-meta-node cons! '(1 (2 3))) '(1 2 3))

          (is-error
           (resolve% (call-tridash-meta-node cons! (list 1 (fail-thunk))))
           tridash-fail)

          (is-error
           (resolve% (call-tridash-meta-node cons! (list (fail-thunk) 1)))
           tridash-fail)))))

  (subtest "Strings"
    (with-module-table modules
      (build-core-module)

      (subtest "Meta-node: string->list"
        (with-nodes ((string->list "string->list")) modules
          (is (call-meta-node string->list '("hello")) '(#\h #\e #\l #\l #\o))

          (is-error (call-meta-node string->list '(1)) tridash-fail)
          (is-error (call-meta-node string->list '((#\h #\e #\l #\l #\o))) tridash-fail)))

      (subtest "Meta-node: list->string"
        (with-nodes ((list->string "list->string")) modules
          (is (call-meta-node list->string '((#\h #\e #\l #\l #\o))) "hello")
          (is (call-meta-node list->string '((#\a #\b #\c 1 2 3))) "abc123")
          (is (call-meta-node list->string '(("ab" #\c #\1 23))) "abc123")
          (is (call-meta-node list->string (list (empty-list))) "")

          (is-error (call-meta-node list->string '("hello")) tridash-fail)
          (is-error (call-meta-node list->string '(1)) tridash-fail)))

      (subtest "Meta-node: format"
        (with-nodes ((format "format")) modules
          (is (call-meta-node format '("hello")) "hello")
          (is (call-meta-node format '("Hello %s." "Bob")) "Hello Bob.")
          (is (call-meta-node format '("Hello %s, welcome to %s." "Bob" "Earth"))
              "Hello Bob, welcome to Earth.")

          (is (call-meta-node format '("Coverage: %s%% of %s." 15 200))
              "Coverage: 15% of 200.")

          (is (call-meta-node format '("%s" "abc")) "abc")
          (is (call-meta-node format '("%s%%" 35)) "35%"))))))

(finalize)
