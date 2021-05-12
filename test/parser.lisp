;;;; parser.lisp
;;;;
;;;; Tridash Programming Language.
;;;; Copyright (C) 2018-2021  Alexander Gutev
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

;;;; Unit tests for parser

(defpackage :tridash/test.parser
  (:use :generic-cl
        :alexandria
        :anaphora
        :arrows
        :iterate
        :optima

        :fiveam
        :tridash.parser
        :tridash.frontend
        :tridash/test
        :tridash/test.util)

  (:shadowing-import-from :generic-cl
                          :emptyp
                          :multiply
                          :accumulate)

  (:shadowing-import-from :fiveam :fail)

  (:import-from :lol :defmacro!)

  (:import-from :tridash.parser
                :declaration-parse-error)

  (:import-from :tridash.frontend
                :+infix-operators+))


(in-package :tridash/test.parser)

(named-readtables:in-readtable :interpol-syntax)


;;; Test Suite Definition

(def-suite parser
    :description "Test parser."
    :in frontend)

(in-suite parser)


;;; Test Utilities

(defmacro! test-parser ((o!string &optional (o!operators '+infix-operators+)) &body declarations)
  "Checks that the node declarations parsed from STRING are equal to
   DECLS. OPERATORS is the operator table to pass to the parser."

  `(with-input-from-string (,g!in ,g!string)
     (let ((,g!parser (make-parser ,g!in)))
       ,@(loop
            for expected in (declaration-symbols declarations)
            collect `(is (decl= ',expected (funcall ,g!parser ,g!operators))))

       (let ((,g!last (funcall ,g!parser ,g!operators)))
         (is-false ,g!last "Expected EOF. Got: ~a" ,g!last)))))

(defun test-parse-error (string &optional (operators +infix-operators+))
  "Tests that parsing a single declaration from STRING results in a
   parse error. OPERATORS is the operator table to pass to the
   parser."

  (with-input-from-string (in string)
    (let ((parser (make-parser in)))
      (signals declaration-parse-error (funcall parser operators)
               "Expected a parse error when parsing: ~a"
               string))))


;;; Node Expression Tests

;; let ((*package* (find-package :tridash.symbols)))


(test atom-nodes
  "Test expressions consisting of just atom nodes."

  (test-parser
      (#?"node1; node2\nnode3")

    !|node1| !|node2| !|node3|))

(test functor-nodes-multiple-arguments
  "Test expressions consisting of functor nodes of multiple arguments."

  (test-parser
      (#?"fn(arg1, arg2)\narg")

    (!|fn| !|arg1| !|arg2|)
    !|arg|))

(test functor-nodes-single-argument
  "Test expressions consisting of functor nodes of a single argument."

  (test-parser
      (#?"fn(arg1)\narg")

    (!|fn| !|arg1|)
    !|arg|))

(test functor-nodes-no-arguments
  "Test expressions consisting of functor nodes of no arguments."

  (test-parser
      (#?"fn()\narg")

      (!|fn|)
      !|arg|))


;;; Infix Operator Tests

(defconstant +arithmetic-operators+
  (alist-hash-map
   `(((:infix ,(s "->")) 10 :right)
     ((:infix ,(s "+")) 20 :left)
     ((:infix ,(s "-")) 20 :left)
     ((:infix ,(s "*")) 50 :right)
     ((:infix ,(s "/")) 50 :left)
     (:open-paren 200)
     ((:infix ,(s ".")) 500 :left))))

(test infix-operator-precedence
  "Test infix operator expressions with multiple operators of varying precedence."

  (test-parser
      (#?"a + b * \nc - e / d" +arithmetic-operators+)

    (!-
     (!+ !\a (!* !\b !\c))
     (!/ !\e !\d))))

(test infix-operator-parenthesis
  "Test grouping using parenthesis in infix operator expressions."

  (test-parser
      (#?"(a + b\n) * (c - e / d)" +arithmetic-operators+)

    (!*
     (!+ !\a !\b)
     (!- !\c (!/ !\e !\d)))))

(test infix-operator-left-associativity
  "Test infix operator left associativity."

  (test-parser
      (#?"a - b - \nc - d" +arithmetic-operators+)

    (!-
     (!- (!- !\a !\b) !\c)
     !\d)))

(test infix-operator-right-associativity
  "Test infix operator right associativity."

  (test-parser
      (#?"a -> b -> \n c -> d\n" +arithmetic-operators+)

    (!-> !\a (!-> !\b (!-> !\c !\d)))))

(test infix-operator-and-functors
  "Test multi-line infix operator expressions with functor nodes."

  (test-parser
      (#?"mod.fn(arg1, arg2) + fn2(arg) -\n node1 * node2" +arithmetic-operators+)

    (!-
     (!+
      ((!\. !|mod| !|fn|) !|arg1| !|arg2|)
      (!|fn2| !|arg|))
     (!* !|node1| !|node2|))))


;;; Node List Test

(test node-lists
  "Test node lists."

  (test-parser
      (#?"fn(a, b) : {a;b\nc\n\nd}"

         (alist-hash-map
          `(((:infix ,(s ":")) 10 :right)
            (:open-paren 200))))

    (!\:
     (!|fn| !\a !\b)
     (!|/prog| !\a !\b !\c !\d))))


;;; Test Literals

(test mixed-literals
  "Test expressions with of mixed literal types."

  (let ((ops (alist-hash-map
              `(((:infix ,(s "+")) 20 :left)
                ((:infix ,(s "-")) 20 :left)
                ((:infix ,(s "*")) 50 :right)
                ((:infix ,(s "/")) 50 :left)
                (:open-paren 200)))))

    (test-parser
        (#?"a + 1 + 2.3 -\"hello\" " ops)

      (!- (!+ (!+ !\a 1) 2.3) "hello"))))

(test string-line-feed-tab-escape
  "Test strings with line feed and tab character escape sequences."

  (test-parser
      (" \"Hello\\n\\tWorld\" ")

    #?"Hello\n\tWorld"))

(test string-carriage-return-line-feed-escape
  "Test strings with carriage return and line feed escape sequences."

  (test-parser
      (" \"Hello\\r\\nWorld\" ")

    #?"Hello\r\nWorld"))

(test string-escape-quotes-backslashes
  "Test strings with escaped quotes and backslashes."

 (test-parser
     (" \"He\\\\she said \\\"Hello World\\\"\" ")

   "He\\she said \"Hello World\""))

(test string-unicode-escape
  "Test strings with unicode escape sequences"

  (test-parser
      (" \"x\\u{2265}5\" ")

    #?"x\x{2265}5"))

(test string-unicode-escape-unclosed
  "Test strings with unclosed unicode escape sequences"

  (test-parser
      (" \"x \\u{2265 5\" ")

    #?"x \x{2265} 5"))

(test string-escape-u-1
  "Test strings with escaped `u` character with unicode character code."

  (test-parser
      (" \"x \\ux1\" ")

    "x ux1"))

(test string-escape-u-2
  "Test strings with escaped `u` character with unicode character code."

  (test-parser
      (" \"x \\u10\" ")

    "x u10"))


;;; Parse Error Tests

(test infix-operator-errors
  "Test parse errors involving malformed infix operator expressions."

  (test-parse-error "a + b")
  (test-parse-error "a +")
  (test-parse-error "a ->")
  (test-parse-error #?"a.\n")
  (test-parse-error #?"a.;"))

(test functor-errors
  "Test parse errors involving malformed functor expressions."

  (test-parse-error "fn(a,b")
  (test-parse-error "fn(a,b,)")
  (test-parse-error "fn(a,b))"))

(test parenthesis-errors
  "Test parse errors involving malformed parenthesis expressions."

  (test-parse-error "(a -> b")
  (test-parse-error "a -> b)")
  (test-parse-error "(a -> b))")
  (test-parse-error "((a -> b)")
  (test-parse-error "(a , b)")
  (test-parse-error "{a b}"))

(test node-list-errors
  "Test parse errors involving node lists."

  (test-parse-error #?"{a -> b; c\nd")
  (test-parse-error #?"a -> b}")
  (test-parse-error #?"{a -> b; c\nd}}")
  (test-parse-error #?"{{a -> b;c\nd}"))
