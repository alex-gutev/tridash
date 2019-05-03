;;;; cut.lisp
;;;;
;;;; Tridash Programming Language.
;;;; Copyright (C) 2017, Alexander Gutev
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


;;;; Contains the implementation of the cut macro
;;;; and reader macro.
;;;;
;;;; The cut macro creates a lambda form, the arguments of which may
;;;; be referred to using %1, %2, %N, where N is the nth argument to
;;;; the function. The macro automatically determines the number of
;;;; required arguments.  A rest argument list is always created and
;;;; can be referred to with the %%.
;;;;
;;;; All arguments are placed in a (declare (ignorable ...)), thus
;;;; arguments can be ignored without generating compiler warnings.
;;;;
;;;; The name cut is a reference to Scheme's cut, which performs a
;;;; similar function.
;;;;
;;;; Example usage:
;;;;
;;;; (cut + 1 %1) ->
;;;;
;;;; (lambda (%1 &rest %%)
;;;;   (declare (ignorable %1 %%))
;;;;   (+ 1 %1))
;;;;

(in-package :tridash.util)

;;; Utility functions used by the macros

(defun replace-syms (tree fn)
  "Traverses the tree and calls FN on each symbol, the symbol is
   replaced with the result returned by FN."

  (when tree
    (if (listp tree)
        (cons (replace-syms (first tree) fn)
              (replace-syms (rest tree) fn))
        (if (symbolp tree)
            (funcall fn tree)
            tree))))

(defun parse-int (str start)
  "Parses STR as an integer, however returns NIL if STR is not a valid
   integer rather than signaling a condition. The argument START is
   the index of the first character in STR from which the integer is
   parsed."

  (multiple-value-bind (int pos) (parse-integer str :start start :junk-allowed t)
    (if (= (length str) pos) int)))


;;; Cut Macro Implementation

(defmacro cut (fn &rest args)
  "Generates a LAMBDA form consisting of a single expression, which
   is `(,FN ,@ARGS). The lambda argument functions can be referenced
   using %N where N is the position of the argument in the lambda
   list. The symbol %% references the rest argument."

  (let ((max-arg -1))
    (flet ((replace-arg (arg)
             (let ((sym (symbol-name arg)))
               (if (= (elt sym 0) #\%)
                   (aif (parse-int sym 1)
                        (setf max-arg (max max-arg it))))
               arg)))

      (let* ((args (replace-syms args #'replace-arg))
             (arg-list (loop for i from 1 to max-arg collect (symb '% i)))
             (rest-arg (intern "%%")))
        `(lambda (,@arg-list &rest ,rest-arg)
           (declare (ignorable ,@arg-list ,rest-arg))
           (,fn ,@args))))))


;;; Reader macro functions.

(defun cut-reader (stream subchar arg)
  "The macro character function for an arbitrary dispatch
   character. The object following the macro character, expected to be
   a list, is read and is spliced into the CUT/LAMBDA form. If the
   object is not a list an error is raised."

  (declare (ignore subchar arg))
  (let ((form (read stream t nil t)))
    (if (listp form)
        (process-cut-form form)
        (error "Expected list after cut reader macro character."))))

(defun process-cut-form (form)
  "Takes the form read by the reader macro function, and returns
   either a lambda form with lambda list being the first element of
   FORM, if the first element is a list, or a CUT macro form if the
   first element is not a list."

  (if (listp (first form))
      (cons 'lambda form)
      (cons 'cut form)))


;;; Named Readtable

(defreadtable cut-syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\L #'cut-reader))

(defreadtable cut+lol-syntax
  (:merge :standard
          lol-syntax
          cut-syntax))
