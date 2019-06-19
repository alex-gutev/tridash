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
  (map #L(gensym (symbol-name (funcall key %1))) syms))

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
             ,@(map #2`(setf ,a1 ,(second a2)) gensyms bindings))
           (progn
             ,@(map #2`(setf ,a1 ,(third a2)) gensyms bindings)))
       (let
           ,(map #2`(,(first a1) ,a2) bindings gensyms)
         ,@body))))

(defmacro! let*-if ((&rest bindings) o!condition &body body)
  "Like LET-IF except the variables are bound sequentially."
  (reduce #2`(let ((,(first a1) (if ,g!condition ,(second a1) ,(third a1)))) ,a2)
          bindings
          :initial-value `(progn ,@body)
          :from-end t))

(defmacro! multiple-value-return ((&rest vars) expr &body body)
  "Returns all values which are the result of the evaluation of the
   form EXPR. The values are simultaneously bound to the variables in
   VARS and each form in BODY is evaluated (in the environment of the
   bindings) with its value discarded."

  `(let ((,g!values (multiple-value-list ,expr)))
     (multiple-value-prog1 (values-list ,g!values)
       (destructuring-bind (&optional ,@vars &rest ,g!rest) ,g!values
         (declare (ignore ,g!rest))
         ,@body))))

(defmacro! with-hash-keys ((&rest keys) o!map &body body)
  "Creates symbol macros for accessing keys in the hash-map
   MAP. Each element in KEYS is a list where the first element
   is the symbol of the symbol-macro to create and the second element
   is the key.

   The symbol-macros are established by SYMBOL-MACROLET and are
   visible to the forms in BODY. The forms in BODY are evaluated with
   the return value of the last form in BODY being the return value of
   WITH-HASH-KEYS form."

  (flet ((make-macro (key)
           (destructuring-bind (sym &optional (key sym)) (ensure-list key)
             `(,sym (get ,key ,g!map)))))
    `(symbol-macrolet
         ,(map #'make-macro keys)
       ,@body)))

(defmacro with-struct-slots (conc-name (&rest slots) object &body body)
  "Same as WITH-SLOTS however for structures defined by
   DEFSTRUCT. CONC-NAME is the symbol, passed to the CONC-NAME option
   to DEFSTRUCT, which is prepended to each symbol in SLOTS to
   generate the name of the accessor function for the slot."

  (flet ((make-binding (slot)
           (ematch slot
             ((or (list var slot)
                  (and (type symbol) var slot))
              (list var (symb conc-name slot))))))

    `(with-accessors ,(map #'make-binding slots) ,object
       ,@body)))

(defmacro! with-retry-restart (&body forms)
  "Evaluates FORMS with the RETRY restart established, if no error is
   raised returns the value of the last form in FORMS. If the retry
   restart is invoked the forms are re-evaluated."

  `(loop named ,g!name
      do
        (with-simple-restart (retry "Retry")
          (return-from ,g!name (progn ,@forms)))))

(defun retry ()
  (invoke-restart 'retry))

(defpattern optional (arg)
  `(or ,arg nil))
