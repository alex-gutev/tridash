;;;; parser.lisp
;;;;
;;;; Tridash Programming Language.
;;;; Copyright (C) 2018  Alexander Gutev
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

(in-package :tridash.test)

(cl-interpol:enable-interpol-syntax)

(plan nil)

(defun test-parser (string decls &optional (operators +infix-operators+))
  "Checks that the node declarations parsed from STRING are equal to
   DECLS. OPERATORS is the operator table to pass to the parser."

  (with-input-from-string (in string)
    (let ((parser (make-parser in)))
      (diag (format nil "Test String: ~s" string))
      (loop
         for got = (funcall parser operators)
         for expected in decls
         while got
         do (is got expected)
         finally (ok (null got))))))

(defmacro decls (&rest decls)
  "Utility macro for creating node declaration lists. Each form in
   DECLS is evaluated with the all symbols, which begin with a !
   replaced with the symbol with an equivalent symbol name in the
   TRIDASH.SYMBOLS package.

   Might not work across all implementations if the backquote syntax
   is used."

  (labels ((!-sym-p (sym)
             (when (char= #\! (char (symbol-name sym) 0))
               (id-symbol (subseq (symbol-name sym) 1))))

           (replace-!-syms (decl)
             (typecase decl
               (symbol
                (or (!-sym-p decl) decl))

               (cons
                (mapcar #'replace-!-syms decl))

               (otherwise decl))))
    `(list ,@(replace-!-syms decls))))

(defun parse-error-p (string &optional (operators +infix-operators+))
  "Tests that parsing a single declaration from STRING results in a
   parse error. OPERATORS is the operator table to pass to the
   parser."

  (with-input-from-string (in string)
    (let ((parser (make-parser in)))
      (diag (format nil "Test String: ~s" string))
      (is-error (funcall parser operators) 'tridash-parse-error))))

(subtest "Test Parser"
  (flet ((s (string)
           (id-symbol string)))

    (let ((*package* (find-package :tridash.symbols)))
      (subtest "Node Expressions"
        (test-parser
         #?"node1; node2\nnode3"
         (decls '!|node1| '!|node2| '!|node3|))

        (test-parser
         #?"fn(arg1, arg2)\narg"
         (decls
          '(!|fn| !|arg1| !|arg2|)
          '!|arg|))

        (test-parser
         #?"fn(arg1)\narg"
         (decls
          '(!|fn| !|arg1|)
          '!|arg|))

        (test-parser
         #?"fn()\narg"
         (decls
          '(!|fn|)
          '!|arg|))

        (let ((ops (alist-hash-table
                    `((,+bind-operator+ 10 :right)
                      (,(s "+") 20 :left)
                      (,(s "-") 20 :left)
                      (,(s "*") 50 :right)
                      (,(s "/") 50 :left)
                      (:open-paren 200)
                      (,+subnode-operator+ 500 :left)))))
          (test-parser
           #?"mod.fn(arg1, arg2) + fn2(arg) -\n node1 * node2"
           (decls
            '(!-
              (!+
               ((!\. !|mod| !|fn|) !|arg1| !|arg2|)
               (!|fn2| !|arg|))
              (!* !|node1| !|node2|)))
           ops)

          (test-parser
           #?"a + b * \nc - e / d"
           (decls
            '(!-
              (!+ !\a (!* !\b !\c))
              (!/ !\e !\d)))
           ops)

          (test-parser
           #?"(a + b\n) * (c - e / d)"
           (decls
            '(!*
              (!+ !\a !\b)
              (!- !\c (!/ !\e !\d))))
           ops)

          (test-parser
           #?"a - b - \nc - d"
           (decls
            '(!-
              (!- (!- !\a !\b) !\c)
              !\d))
           ops)

          (test-parser
           #?"a -> b -> \n c -> d\n"
           (decls
            '(!-> !\a (!-> !\b (!-> !\c !\d)))))))

      (subtest "Node Lists"
        (test-parser
         #?"fn(a, b) : {a;b\nc\n\nd}"
         (decls
          '(!\:
            (!|fn| !\a !\b)
            (list !\a !\b !\c !\d)))
         (alist-hash-table
          `((,(s ":") 10 :right)
            (:open-paren 200)))))

      (subtest "Literals"
        (let ((ops (alist-hash-table
                   `((,(s "+") 20 :left)
                     (,(s "-") 20 :left)
                     (,(s "*") 50 :right)
                     (,(s "/") 50 :left)
                     (:open-paren 200)))))

        (test-parser
         #?"a + 1 + 2.3 -\"hello\" "
         (decls
          '(!- (!+ (!+ !\a 1) 2.3) "hello"))
         ops))))

    (subtest "Parse Errors"
      (parse-error-p "a + b")
      (parse-error-p "a +")
      (parse-error-p "a ->")
      (parse-error-p #?"a.\n")
      (parse-error-p #?"a.;")

      (parse-error-p "fn(a,b")
      (parse-error-p "fn(a,b,)")
      (parse-error-p "fn(a,b))")

      (parse-error-p "(a -> b")
      (parse-error-p "a -> b)")
      (parse-error-p "(a -> b))")
      (parse-error-p "((a -> b)")
      (parse-error-p "(a , b)")
      (parse-error-p "{a b}")

      (parse-error-p #?"{a -> b; c\nd")
      (parse-error-p #?"a -> b}")
      (parse-error-p #?"{a -> b; c\nd}}")
      (parse-error-p #?"{{a -> b;c\nd}"))))

(finalize)

(cl-interpol:disable-interpol-syntax)
