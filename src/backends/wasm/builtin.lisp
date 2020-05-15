;;;; builtin.lisp
;;;;
;;;; Tridash Programming Language.
;;;; Copyright (C) 2020  Alexander Gutev
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

;;;; Generating code corresponding to builtin meta-nodes

(in-package :tridash.backend.wasm)

(defgeneric compile-builtin (operator operands)
  (:documentation
   "Generate code corresponding to an expression of a builtin
    operator."))


;;; Boolean Operators

(defmethod compile-builtin ((operator (eql 'not)) operands)
  (let* ((operands (compile-operands operands '(t)))
         (label (first (map #'value-block-label operands)))
         (result (next-local)))

    (make-value-block
     :label result
     :operands operands
     :strict-p t
     :immediate-p t

     :instructions
     `((block $out
         (block $type-error
           (block $fail
             (block $false
               (block $true
                 (resolve ,label)
                 (unbox ,label (type boolean)))

               (constant ,result nil)
               (br $out))

             (constant ,result :true)
             (br $out))

           ;; Handle Failure
           (get-fail ,label)
           (box ,result (type fail))
           (br $out))

         ,@(make-type-error result))))))

(defmethod compile-builtin ((operator (eql 'and)) operands)
  (let ((operands (compile-operands operands '(t nil)))
        (result (next-local)))

    (destructuring-bind (l r)
        (map #'value-block-label operands)

      (make-value-block
       :label result
       :operands operands
       :strict-p t
       :immediate-p t

       :instructions
       `((block $out
           (block $type-error
             (block $fail
               (block $false
                 (block $true
                   (resolve ,l)
                   (unbox ,l (type boolean)))

                 (block $fail
                   (block $true
                     (resolve ,r)
                     (unbox ,r (type boolean)))

                   (constant ,result :true)
                   (br $out))

                 ;; Handle Failure in Right Operand
                 (get-fail ,r)
                 (box ,result (type fail))
                 (br $out))

               (constant ,result nil)
               (br $out))

             ;; Handle Failure
             (get-fail ,l)
             (box ,result (type fail))
             (br $out))

           ;; Type Error
           ,@(make-type-error result)))))))

(defmethod compile-builtin ((operator (eql 'or)) operands)
  (let ((operands (compile-operands operands '(t nil)))
        (result (next-local)))

    (destructuring-bind (l r)
        (map #'value-block-label operands)

      (make-value-block
       :label result
       :operands operands
       :strict-p t
       :immediate-p t

       :instructions
       `((block $out
           (block $type-error
             (block $fail
               (block $true
                 (block $false
                   (resolve ,l)
                   (unbox ,l (type boolean)))

                 (block $fail
                   (block $false
                     (resolve ,r)
                     (unbox ,r (type boolean)))

                   (constant ,result nil)
                   (br $out))

                 ;; Handle Failure in Right Operand
                 (get-fail ,r)
                 (box ,result (type fail))
                 (br $out))

               (constant ,result :true)
               (br $out))

             ;; Handle Failure
             (get-fail ,l)
             (box ,result (type fail))
             (br $out))

           ;; Type Error
           ,@(make-type-error result)))))))
