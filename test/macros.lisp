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
                :call-tridash-meta-node
                :call-node

                :thunk
                :resolve
                :tridash-fail
                :group-rest-args

                :check-arity

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
             ((list 'optional symb value)
              (list +optional-argument+ symb value))

             ((list 'rest symb)
              (list +rest-argument+ symb))

             (_ operand)))

         (operand-node (operand)
           (match operand
             ((list* 'optional symb _)
              symb)

             ((list 'rest symb)
              symb)

             (_ operand))))

    `(let ((self (make-instance 'meta-node
                                :name 'test-meta-node
                                :operands ',(map #'make-operand operands)))
           ,@(map #`(,a1 (node-link (make-instance 'node :name ',a1)))
                  (map #'operand-node operands)))

       ;; Create an empty `FLAT-NODE-TABLE' to mark meta-node as
       ;; already built
       (setf (definition self) (make-instance 'flat-node-table))
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

  `(let ((self (mock-meta-node ,operands ,expression)))
     (is (tridash->cl-function self)
         `(lambda ,',args
            ,,body)
         :test #'expression=)))


(plan 3)

(subtest "Tridash to CL Compilation"
  (subtest "Functor Expressions"
    (with-core-nodes ("if" "<" "-")
      (test-compile-meta-node

       (a b)
       (functor if (functor < a b) (functor - b a) (functor - a b))

       ($a $b)
       '(!|if| (!< $a $b)
         (thunk (!- $b $a))
         (thunk (!- $a $b))))))

  (subtest "If Expressions"
    (with-core-nodes ("<" "-")
      (test-compile-meta-node

       (a b)
       (if-expression (functor < a b) (functor - b a) (functor - a b))

       ($a $b)
       '(!|if| (!< $a $b)
         (thunk (!- $b $a))
         (thunk (!- $a $b))))))

  (subtest "Object Expressions"
    (with-core-nodes ("+" "-")
      (test-compile-meta-node

       (x y)
       (object-expression
        `((sum ,(functor + x y))
          (diff ,(functor - x y))))

       ($x $y)
       '(alist-hash-map
         (list
          (cons 'sum (thunk (!+ $x $y)))
          (cons 'diff (thunk (!- $x $y))))))))

  (subtest "Member Expressions"
    (test-compile-meta-node

     (object)
     (member-expression
      (member-expression object 'key1) 'key2)

     ($obj)
     '(!|member| (!|member| $obj 'key1) 'key2)))

  (subtest "Catch Expressions"
    (with-core-nodes ("/" "*")
      (test-compile-meta-node

       (a b)
       (catch-expression
        (functor / a b)
        (functor * a b))

       ($a $b)

       '(!|catch| (!/ $a $b) (thunk (!* $a $b))))))

  (subtest "Fail Expressions"
    (test-compile-meta-node
     ()
     (fail-expression)

     ()
     '(!|fail|)))

  (subtest "Expression Blocks"
    (with-core-nodes ("+")
      (test-compile-meta-node

       (a)
       (expression-block
        (functor + a 1))

       ($a)
       '(!+ $a 1))))

  (subtest "Calling Other Meta-Nodes"
    (with-core-nodes ("-")
      (let ((meta-node (mock-meta-node (a) a)))
        (test-compile-meta-node

         (a)
         (functor meta-node (functor - a))

         ($a)
         `(call-tridash-meta-node ,meta-node (list (!- $a)))))))

  (subtest "Higher-Order Meta-Nodes"
    (subtest "External Meta-Node"
      (with-core-nodes ("not")
        (let ((apply (mock-meta-node (f x) (functor f x))))
          (test-compile-meta-node

           (x)
           (functor apply (meta-node-ref not) x)

           ($x)
           `(call-tridash-meta-node ,apply (list #'!|not| $x))))))

    (subtest "If Meta-Node"
      (with-core-nodes ("if")
        (let ((apply (mock-meta-node (f x y z) (functor f x y z))))
          (test-compile-meta-node

           (x y z)
           (functor apply (meta-node-ref if) x y z)

           ($x $y $z)
           `(call-tridash-meta-node
             ,apply

             (list #'!|if| $x $y $z))))))

    (subtest "And Meta-Node"
      (with-core-nodes ("and")
        (let ((apply (mock-meta-node (f x y) (functor f x y))))
          (test-compile-meta-node

           (x y)
           (functor apply (meta-node-ref and) x y)

           ($x $y)
           `(call-tridash-meta-node
             ,apply

             (list #'!|and| $x $y))))))

    (subtest "Or Meta-Node"
      (with-core-nodes ("or")
        (let ((apply (mock-meta-node (f x y) (functor f x y))))
          (test-compile-meta-node

           (x y)
           (functor apply (meta-node-ref or) x y)

           ($x $y)
           `(call-tridash-meta-node
             ,apply

             (list #'!|or| $x $y))))))

    (subtest "Tridash Meta-Node"
      (let ((apply (mock-meta-node (f x) (functor f x)))
            (f (mock-meta-node (x) x)))

        (test-compile-meta-node

         (x)
         (functor apply (meta-node-ref f) x)

         ($x)
         `(call-tridash-meta-node
           ,apply
           (list
            #'(lambda (&rest $args)
                (check-arity ,f $args)
                (call-tridash-meta-node ,f $args))
            $x)))))

    (subtest "Meta-Node with Optional Arguments"
      (let ((apply (mock-meta-node (f x) (functor f x)))
            (f (mock-meta-node (x) x)))

        (test-compile-meta-node
         (x)
         (functor apply (meta-node-ref f :optional (list 1 2)) x)

         ($x)
         `(call-tridash-meta-node
           ,apply
           (list
            #'(lambda (&rest $args)
                (check-arity ,f $args)
                (call-tridash-meta-node ,f $args))
            $x)))))

    (subtest "Meta-Node with Rest Arguments"
      (let ((apply (mock-meta-node (f x) (functor f x)))
            (f (mock-meta-node (x y (rest xs)) xs)))

        (test-compile-meta-node
         (x)
         (functor apply (meta-node-ref f) x)

         ($x)
         `(call-tridash-meta-node
           ,apply
           (list
            #'(lambda (&rest $args)
                (check-arity ,f $args)
                (call-tridash-meta-node ,f (group-rest-args $args 2)))
            $x)))))

    (subtest "Invoking Nodes"
      (test-compile-meta-node

       (f x y)
       (functor f x y)

       ($f $x $y)
       `(call-node $f (list $x $y)))))

  (subtest "Literals"
    (with-core-nodes ("and")
      (test-compile-meta-node

       ()
       (functor and "hello" (functor and 1 (functor and 2.3 'symbol)))

       ()
       '(!|and| "hello"
         (thunk
          (!|and| 1
           (thunk
            (!|and| 2.3 'symbol))))))))

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
         '(!/
           (!* (!+ $a $b) (!- $c $d))
           (!- $d))))

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
         '(!|not|
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
                     (thunk (!!= $x $y))))))))))))

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
         '(!|or|
           (!|int?| $x)

           (thunk
            (!|or|
             (!|real?| $y)
             (thunk (!|string?| $z)))))))))

  (subtest "Tail Recursion"
    (subtest "If Expressions"
      (with-core-nodes ("-" "*" "<")
        (test-compile-meta-node

         (n acc)
         (if-expression (functor < n 2)
                        acc
                        (functor self (functor - n 1) (functor * n acc)))

         ($n $acc)
         `(!|if| (!< $n 2)
            $acc
            (thunk
             (call-tridash-meta-node ,self (list (!- $n 1) (!* $n $acc))))))))

    (subtest "If Functor"
      (with-core-nodes ("if" "-" "*" "<")
        (test-compile-meta-node

         (n acc)
         (functor if
                  (functor < n 2)
                  acc
                  (functor self (functor - n 1) (functor * n acc)))

         ($n $acc)
         `(!|if| (!< $n 2)
            $acc
            (thunk
             (call-tridash-meta-node ,self (list (!- $n 1) (!* $n $acc))))))))

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
         `(!|if| (!< $n 2)
            $acc
            (thunk
             (call-tridash-meta-node ,self (list (!- $n 1) (!* $n $acc))))))))

    (subtest "And/Or Functors"
      (with-core-nodes ("and" "or" "=" "-")
        (subtest "Or"
          (test-compile-meta-node

           (n)
           (functor or (functor = n 0) (functor self (functor - n 1)))

           ($n)
           `(!|or| (!= $n 0)
              (thunk
               (call-tridash-meta-node ,self (list (!- $n 1)))))))

        (subtest "And"
          (test-compile-meta-node

           (n)
           (functor and (functor = n 0) (functor self (functor - n 1)))

           ($n)
           `(!|and| (!= $n 0)
              (thunk
               (call-tridash-meta-node ,self (list (!- $n 1)))))))))

    (subtest "Catch Expressions"
      (with-core-nodes ("-" "+")
        (test-compile-meta-node

         (n)
         (catch-expression (functor self (functor + n 1))
                           (functor self (functor - n 1)))

         ($n)
         `(!|catch| (call-tridash-meta-node ,self (list (!+ $n 1)))
            (thunk
             (call-tridash-meta-node ,self (list (!- $n 1)))))))))

  (subtest "Optional and Rest Arguments"
    (subtest "Optional Arguments"
      (with-core-nodes ("+")
        (test-compile-meta-node
         (n (optional d 1))
         (functor + n d)

         ($n &optional ($d 1))
         '(!|+| $n $d))))

    (subtest "Multiple Optional Arguments"
      (with-core-nodes ("+")
        (test-compile-meta-node
         (n (optional d 1) (optional e 2))
         (functor + n (functor + d e))

         ($n &optional ($d 1) ($e 2))
         '(!|+| $n (!|+| $d $e)))))

    (subtest "Rest Arguments"
      (with-core-nodes ("cons")
        (test-compile-meta-node
         (x (rest xs))
         (functor cons x xs)

         ($x &optional $xs)
         '(!|cons| $x $xs))))

    (subtest "Rest and Optional"
      (with-core-nodes ("cons")
        (test-compile-meta-node
         (x (optional y 2) (rest xs))
         (functor cons x (functor cons y xs))

         ($x &optional ($y 2) $xs)
         '(!|cons| $x (thunk (!|cons| $y $xs)))))))

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
          (is (resolve (call-tridash-meta-node g '(1))) '(2)))))

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
          (ok (bool-value (resolve (call-tridash-meta-node test '(-1))))))))))

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

  (subtest "Macros in Macros"
    (with-module-table modules
      (build-core-module)
      (build ":import(core)"

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
      (build ":import(core)"

             "'(x) : list(:quote(:quote), x)"
             ":attribute(', macro, 1)"

             "!-(a, b) : list('(if), a, b)"
             ":attribute(!-, macro, 1)"
             ":op(!-, 25, left)"

             "a !- b -> out")

      (test-not-nodes '("!-" "a" "b"))

      (with-nodes ((a "a") (b "b") (out "out")
                   (a!-b ((":in" "builtin" "if") "a" "b"))
                   (if "if"))
          modules

        (has-value-function (a b) a!-b `(,if ,a ,b nil))
        (test-simple-binding a!-b out)))))

(finalize)
