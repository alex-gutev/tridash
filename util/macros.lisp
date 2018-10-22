;;;; macros.lisp
;;;;
;;;; Tridash Programming Language.
;;;; Copyright (C) 2017  Alexander Gutev
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

(in-package :tridash.util)

(in-readtable cut+lol-syntax)


(defun gensyms (syms &key (key #'identity))
  (mapcar #L(gensym (symbol-name (funcall key %1))) syms))

(defmacro! let-if ((&rest bindings) condition &body body)
  "Allows variables to be initialized with different init-forms based
   on a condition. BINDINGS is a list of bindings where the first
   element is the symbol the second element is the init-form to be
   evaluated if CONDITION evaluates to true, the second element is the
   init-form to evaluate if CONDITION evaluates to false."

  (let ((gensyms (gensyms bindings :key #'car)))
    `(let ((,g!test ,condition) ,@gensyms)
       (if ,g!test
           (progn
             ,@(mapcar #2`(setf ,a1 ,(second a2)) gensyms bindings))
           (progn
             ,@(mapcar #2`(setf ,a1 ,(third a2)) gensyms bindings)))
       (let
           ,(mapcar #2`(,(first a1) ,a2) bindings gensyms)
         ,@body))))

(defmacro! let*-if ((&rest bindings) o!condition &body body)
  "Like LET-IF except the variables are bound sequentially."
  (reduce #2`(let ((,(first a1) (if ,g!condition ,(second a1) ,(third a1)))) ,a2)
          bindings
          :initial-value `(progn ,@body)
          :from-end t))

(defmacro! dohash ((key value hash &optional result) &body body)
  "Iterates over each element of HASH with the key bound to KEY and
   the value bound to VALUE. BODY is evaluated on each iteration in an
   implicit PROGN."

  (let ((key (or key g!key))
        (value (or value g!value)))
    `(with-hash-table-iterator (,g!next ,hash)
       (loop
          (multiple-value-bind (,g!next-p ,key ,value) (,g!next)
            (declare (ignorable ,key ,value))
            (unless ,g!next-p
              (return ,result))
            ,@body)))))

(defmacro! multiple-value-return ((&rest vars) expr &body body)
  "Returns all values which are the result of the evaluation of the
   form EXPR. The values are simultaneously bound to the variables in
   VARS and each form in BODY is evaluated (in the environment of the
   bindings) with its value discarded."

  `(let ((,g!values (multiple-value-list ,expr)))
     (multiple-value-prog1 (values-list ,g!values)
       (destructuring-bind ,vars ,g!values
         ,@body))))
