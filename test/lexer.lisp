;;;; lexer.lisp
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

;;;; Unit tests for lexer

(defpackage :tridash/test.lexer
  (:use :generic-cl
        :alexandria
        :anaphora
        :arrows
        :iterate
        :optima

        :fiveam
        :tridash.parser
        :tridash/test
        :tridash/test.util)

  (:shadowing-import-from :generic-cl
                          :emptyp
                          :multiply
                          :accumulate)

  (:shadowing-import-from :fiveam :fail)

  (:import-from :lol :defmacro!)

  (:import-from :tridash.parser

                :*line-term*
                :make-lexer
                :next-token))

(in-package :tridash/test.lexer)

(named-readtables:in-readtable :interpol-syntax)


;;; Test Suite Definition

(def-suite lexer
    :description "Test lexical analysis"
    :in tridash)

(in-suite lexer)


;;; Test Utility Functions

(defmacro! is-tokens (o!string &body tokens)
  `(with-input-from-string (,g!in ,g!string)
     (let ((,g!lexer (make-lexer ,g!in)))
       ,@(loop
            for token in tokens
            collect `(is (= ',token (multiple-value-list (next-token ,g!lexer)))))

       (let ((,g!last-token (next-token ,g!lexer)))
         (is-false ,g!last-token "Expected EOF. Got: ~a" ,g!last-token)))))

(defun is-lexer-error (string)
  (with-input-from-string (in string)
    (signals
        tridash.parser::tridash-parse-error

      (loop
         with lexer = (make-lexer in)
         while (next-token lexer))

      "Expected ~a to result in a `TRIDASH-PARSE-ERROR'."
      string)))


;;; Node Identifier Token Tests

(test node-identifiers-tabs-spaces
  "Test tokenization of node identifiers separated by spaces and tabs."

  (is-tokens
      #?"node-1s    +\t2node - \t third-node"

      (:id "node-1s")
      (:id "+")
      (:id "2node")
      (:id "-")
      (:id "third-node")))

(test node-identifiers-tabs-spaces-form-feed
  "Test tokenization of node identifiers separated by spaces, tabs and form feeds."

  (is-tokens
      #?"node-1s    +\t2node - \f third-node"

      (:id "node-1s")
      (:id "+")
      (:id "2node")
      (:id "-")
      (:id "third-node")))


;;; Terminate Token Tests

(test terminate-token-line-terminate
  "Test terminate tokens with line-feed interpreted as a terminate token."

  (let ((*line-term* t))
    (is-tokens
        #?"node1 node2\nnode3;node4"

        (:id "node1")
        (:id "node2")
        (:terminate #?"\n")
        (:id "node3")
        (:terminate ";")
        (:id "node4"))))

(test terminate-token-line-skip
  "Test terminate tokens with line-feed not a terminate token."

  (let ((*line-term* nil))
    (is-tokens
        #?"node1 node2\nnode3;node4"

        (:id "node1")
        (:id "node2")
        (:id "node3")
        (:terminate ";")
        (:id "node4"))))


;;; Comment Tests

(test comment-line-start
  "Test comments beginning at first character of line."

  (is-tokens "# This is a comment"))

(test comment-after-whitespace
  "Test comments preceded by whitespace."

  (is-tokens "  #This is a comment"))

(test comment-double-hash
  "Test comments started with two `#` characters."

  (is-tokens "## Another comment"))

(test comment-after-node
  "Test comments preceded by node identifier."

  (is-tokens
      "node #followed by a comment"

    (:id "node")))

(test comment-line-terminate
  "Test that comment is tokenized as a terminate token when line-feed is."

  (let ((*line-term* t))
    (is-tokens
        #?"node-a # A comment\nnode-b"

        (:id "node-a")
        (:terminate #?"\n")
        (:id "node-b"))))

(test comment-line-skip
  "Test that comment is tokenized as a skip token when line-feed is."

  (let ((*line-term* nil))
    (is-tokens
        #?"node-a # A comment\nnode-b"

        (:id "node-a")
        (:id "node-b"))))


;;; Special Token Tests

(test special-token-comma
  "Test tokenization of comma `,` special token."

  (is-tokens
      ",a, b, c ,"

    (:comma ",")
    (:id "a")
    (:comma ",")
    (:id "b")
    (:comma ",")
    (:id "c")
    (:comma ",")))

(test special-token-parenthesis
  "Test tokenization of parenthesis `(...)`."

  (is-tokens
      "fn(a,b,((c,d)),e)"

    (:id "fn")
    (:open-paren "(")
    (:id "a")
    (:comma ",")
    (:id "b")
    (:comma ",")
    (:open-paren "(")
    (:open-paren "(")
    (:id "c")
    (:comma ",")
    (:id "d")
    (:close-paren ")")
    (:close-paren ")")
    (:comma ",")
    (:id "e")
    (:close-paren ")")))

(test special-token-braces
  "Test tokenization of braces `{...}`."

  (is-tokens
      "def{a, b({c}),}x"

    (:id "def")
    (:open-brace "{")
    (:id "a")
    (:comma ",")
    (:id "b")
    (:open-paren "(")
    (:open-brace "{")
    (:id "c")
    (:close-brace "}")
    (:close-paren ")")
    (:comma ",")
    (:close-brace "}")
    (:id "x")))


;;; Number Token Tests

(test number-mixed-special-id-integer-real-1
  "Test tokenization of numbers (integer and real) among special tokens and node ids."

  (is-tokens
      "{-123 + ,34.01, - (-12.8) - 7}"

    (:open-brace "{")
    (:integer "-123")
    (:id "+")
    (:comma ",")
    (:real "34.01")
    (:comma ",")
    (:id "-")
    (:open-paren "(")
    (:real "-12.8")
    (:close-paren ")")
    (:id "-")
    (:integer "7")
    (:close-brace "}")))

(test number-mixed-special-id-integer-real-2
  "Test tokenization of numbers (integer and real) among special tokens and node ids."

  (is-tokens
      "{-123.3 + ,34, - (-12) - 7.3}"

    (:open-brace "{")
    (:real "-123.3")
    (:id "+")
    (:comma ",")
    (:integer "34")
    (:comma ",")
    (:id "-")
    (:open-paren "(")
    (:integer "-12")
    (:close-paren ")")
    (:id "-")
    (:real "7.3")
    (:close-brace "}")))

(test number-real
  "Test tokenization of real number."

  (is-tokens "100.98" (:real "100.98")))

(test number-real-exponent-notation-1
  "Test tokenization of real numbers in exponent notation."

  (is-tokens
      "12d34 12d-34 0f12 0f-12 5e78 5e-78 99l01 99l-01 99l+2"

    (:real "12d34")
    (:real "12d-34")
    (:real "0f12")
    (:real "0f-12")
    (:real "5e78")
    (:real "5e-78")
    (:real "99l01")
    (:real "99l-01")
    (:real "99l+2")))

(test number-real-exponent-notation-2
  "Test tokenization of real numbers in exponent notation."

  (is-tokens
      "1.2d34 1.2d-34 0.5e78 0.5e-78 5.6e78 5.6e-78 0.1f2 0.1f-2 9.9l01 9.9l-01 0.9l1 0.9l+1"

    (:real "1.2d34")
    (:real "1.2d-34")
    (:real "0.5e78")
    (:real "0.5e-78")
    (:real "5.6e78")
    (:real "5.6e-78")
    (:real "0.1f2")
    (:real "0.1f-2")
    (:real "9.9l01")
    (:real "9.9l-01")
    (:real "0.9l1")
    (:real "0.9l+1")))

(test node-id-with-numbers
  "Test tokenization of node id's with leading numbers."

  (is-tokens
      "12dd34 12d34d 5e7e8 56e7e8 99l0d1 9l1l 7x8"

    (:id "12dd34")
    (:id "12d34d")
    (:id "5e7e8")
    (:id "56e7e8")
    (:id "99l0d1")
    (:id "9l1l")
    (:id "7x8")))

(test number-tokenization-errors
  "Test tokenization errors when tokenizing malformed numbers."

  (is-lexer-error "1.2e100.3")
  (is-lexer-error "1.2+3.4")
  (is-lexer-error "1.2.3")
  (is-lexer-error "2.4e10+5"))


;;; String Token Tests

(test string-simple
  "Test tokenization of basic quoted string (no escapes)."

  (is-tokens " \"Hello World\" "
    (:string "\"Hello World\"")))

(test string-escape-quote
  "Test tokenization of string containing escaped quotes."

  (is-tokens "\"Bob said \\\"Hello\\\" to Mary \""
    (:string "\"Bob said \\\"Hello\\\" to Mary \"")))

(test string-escape-quote-slash
  "Test tokenization of string containing escaped quotes and backslashes."

  (is-tokens "\"A \\\\\\\" regex\\\\\""
    (:string "\"A \\\\\\\" regex\\\\\"")))

(test string-in-node-ids
  "Test tokenization of string among node identifier tokens."

  (is-tokens
      "node1\"Hello\"node2"

    (:id "node1")
    (:string "\"Hello\"")
    (:id "node2")))

(test string-in-integers
  "Test tokenization of string among integer tokens."

  (is-tokens
      "1\"str\"2 "

    (:integer "1")
    (:string "\"str\"")
    (:integer "2")))

(test string-in-reals
  "Test tokenization of string among real number tokens."

  (is-tokens
      "1.01\"str\"2.33 "

    (:real "1.01")
    (:string "\"str\"")
    (:real "2.33")))

(test string-unclosed
  "Test tokenization errors when tokenizing unclosed string."

  (is-lexer-error "\"hello "))


;;; Dot Tokens

(test dot-single
  "Test tokenization of single `.` token."

  (is-tokens
      "object.field"

    (:id "object")
    (:id ".")
    (:id "field")))

(test dot-double
  "Test tokenization of multiple dot tokens, e.g. `..`."

  (is-tokens
      "object..field"

    (:id "object")
    (:id "..")
    (:id "field")))

(test dot-leading
  "Test tokenization of leading `.` token."

  (is-tokens
      ".object"

    (:id ".")
    (:id "object")))

(test dot-trailing
  "Test tokenization of trailing `.` token."

  (is-tokens
      "object.  "

    (:id "object")
    (:id ".")))
