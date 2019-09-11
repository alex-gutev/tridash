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

(defpackage :tridash.test.util
  (:use :generic-cl
        :tridash.parser
        :optima)

  (:import-from :lol :defmacro!)
  (:import-from :alexandria :length=)

  (:export :match*
           :shadow-function
           :previous

           :decls
           :node-id
           :decl=

	   :testf
	   :okf
	   :isf
	   :is-typef

	   :ok!
	   :is!
	   :is-type!)

  (:documentation
   "Contains utility functions and macros which are used in all test
    packages."))


(in-package :tridash.test.util)


;;;; General Utilities

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

(defun decl= (got expected)
  "Returns true if the node declaration GOT is equivalent to
   EXPECTED."

  (match* (got expected)
    (((or (atom-node- (identifier got))
          (and (type symbol) got))

      (and (type symbol) expected))

     (= got expected))

    (((or (functor-node- (operator got-op) (operands got-args))
          (list* got-op got-args))

      (list* exp-op exp-args))

     (and (decl= got-op exp-op)
          (length= got-args exp-args)
          (every #'decl= got-args exp-args)))

    (((or (literal-node- (value got)) got)
      expected)

     (= got expected))))


;;; Utility Test Macros

(defmacro testf ((&rest test) &rest desc)
  "Performs the test TEST with the description obtained by (FORMAT NIL
   ,@DESC)."

  `(,@test ,@(when desc `((format nil ,@desc)))))

(defmacro okf (test &rest desc)
  "Performs an OK with the description obtained by (FORMAT NIL
   ,@DESC)."

  `(testf (prove:ok ,test) ,@desc))

(defmacro isf (got expected &rest desc)
  "Performs an IS test with the description obtained by (FORMAT NIL
   ,@DESC)."

  `(testf (prove:is ,got ,expected) ,@desc))

(defmacro is-typef (got expected-type &rest desc)
  "Performs an IS-TYPE with the description obtained by (FORMAT NIL
   ,@DESC)."

  `(testf (prove:is-type ,got ,expected-type) ,@desc))


(define-condition abort-test (error) ()
  (:documentation
   "Condition raised to abort the current subtest (preventing the
    remaining tests from being run) when a test fails."))

(defmacro! ok! (o!test &rest desc)
  "Same as OKF but signals an ABORT-TEST condition if the test fails."

  `(progn
     (okf ,g!test ,@desc)
     (unless ,g!test
       (error 'abort-test))))

(defmacro! is! (o!got o!expected &rest desc)
  "Same as ISF but signals an ABORT-TEST condition if the test fails."

  `(progn
     (isf ,g!got ,g!expected ,@desc)
     (unless (funcall prove:*default-test-function* ,g!got ,g!expected)
       (error 'abort-test))))

(defmacro! is-type! (o!got o!type &rest desc)
  "Same as IS-TYPEF but signals an ABORT-TEST condition if the test fails."

  `(progn
     (is-typef ,g!got ,g!type ,@desc)
     (unless (typep ,g!got ,g!type)
       (error 'abort-test))))
