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
  (:use :cl
        :tridash.parser)

  (:export :decls
           :node-id)

  (:documentation
   "Contains utility functions and macros which are used in all test
    packages."))


(in-package :tridash.test.util)

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

(defun node-id (name)
  "Converts a string/list node identifier to an identifier symbol."

  (typecase name
    (cons (mapcar #'node-id name))
    (symbol name)
    (otherwise (id-symbol name))))
