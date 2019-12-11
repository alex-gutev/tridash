;;;; failures.lisp
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

;;;; Optimizes Propagation of Failures

(defpackage :tridash.frontend.failures
  (:use :generic-cl
        :alexandria
        :anaphora
        :optima
        :cl-arrows
        :named-readtables

        :tridash.util
	:tridash.parser
        :tridash.frontend)

  (:shadowing-import-from :generic-cl
                          :emptyp
                          :accumulate
                          :multiply)

  (:import-from :tridash.frontend
                :process-attribute))

(in-package :tridash.frontend.failures)

(in-readtable cut-syntax)

(defun optimize-expression (expression)
  "Replaces failures and catch expressions with conditional expressions."

  (labels
      ((conditionalize (condition then else)
         "Returns an if expression with condition specified by the
          failure propagation expression CONDITION, returning THEN if
          true and ELSE if false."

         (aprog1
             (ematch condition
               (nil else)

               ((list t) then)

               ((list* 'or conditions)
                (reduce #L(conditionalize %1 then %2) conditions :from-end t :initial-value else))

               ((list* 'and conditions)
                (reduce #L(conditionalize %1 %2 else) conditions :from-end t :initial-value then))


               ((list 'if (list 'not condition) expression)
                (if-expression
                 condition
                 else
                 (conditionalize expression then else)))

               ((list 'if condition expression)
                (if-expression
                 condition

                 (conditionalize expression then else)
                 else))))))

    (with-struct-slots functor-expression- (meta-node arguments)
        expression

      (destructuring-bind (try catch &optional test)
          arguments

        (let* ((try (replace-failure-expressions try catch)))
          (-> try
              failure-propagation
              simplify-failure-propagation
              (conditionalize catch try)
              (catch-expression catch test)))))))

(defun simplify-failure-propagation (expression)
  "Simplifies a failure propagation expression, removing always true
   or always false expressions."

  (match expression
    ((list* (or 'or 'and) expressions)
     (let ((expressions
            (->> (map #'simplify-failure-propagation expressions)
                 (remove nil))))
       (when expressions
         (if (rest expressions)
             (list* 'or expressions)
             (first expressions)))))

    ((list* 'and expressions)
     (let ((expressions
            (->> (map #'simplify-failure-propagation expressions)
                 (remove t))))

       (when (every #'identity expressions)
         (if (rest expressions)
             (list* 'and expressions)
             (first expressions)))))

    ((list 'if condition expression)
     (awhen (simplify-failure-propagation expression)
       (list 'if condition it)))

    (_ expression)))


;;; Replacing Failures with Catch Values

(defgeneric replace-failure-expressions (expression catch-value)
  (:documentation
   "Returns an expression where each sub-expression, of EXPRESSION,
    which generates a failure is replaced with CATCH-VALUE.

    The second return value is T if EXPRESSION itself was replaced,
    NIL otherwise."))


;;;; Functor Expressions

(defmethod replace-failure-expressions ((functor functor-expression) catch-value)
  ;; Replace failure generating expressions only in those arguments in
  ;; which the meta-node returns failures.

  (labels ((map-arg (argument returns-fail?)
             (multiple-value-bind (argument fails?)
                 (->> (if returns-fail? catch-value (fail-expression))
                      (replace-failure-expressions argument))

               (cons argument fails?)))

           (map-outer-node (outer-node returns-fail?)
             (destructuring-bind (node . expression)
                 outer-node
               (cons node (map-arg expression returns-fail?)))))

    (with-struct-slots functor-expression- (meta-node arguments outer-nodes)
        functor

      (typecase meta-node
        (meta-node

         (let* ((arguments (map #'map-arg arguments (failure-return-operands meta-node)))
                (outer-nodes (map #'map-outer-node outer-nodes (failure-return-outer-nodes meta-node)))
                (fail-operands
                 (nconcatenate
                  (map #'cddr outer-nodes)
                  (pairlis (operand-node-names meta-node) (map #'cdr arguments))))

                (result
                 (functor-expression
                  meta-node
                  (map #'first arguments)

                  :outer-nodes
                  (map #L(cons (first %1) (second %1)) outer-nodes))))

           (if (returns-failure? (meta-node-failure-propagation meta-node) fail-operands)
               (values catch-value t)
               (values result nil))))

        (otherwise
         functor)))))


;;;; Literals

(defmethod replace-failure-expressions (literal catch-value)
  (declare (ignore catch-value))
  (values literal nil))


;;; Failure Return Analysis

(defvar *operand-failure-return* #'identity
  "Function of one argument (a `node' serving as the operand to an
   expression) which should return the failure return set for that
   node.")


;;;; Nodes

(defun node-failure-return (node)
  "Returns the failure return set for a `node'."

  (with-slots (dependencies contexts) node
    (if (emptyp dependencies)
        node
        (-<> (curry #'context-failure-return node)
             (map-to 'list <> contexts)
             (reduce #'union <>)))))

(defun context-failure-return (node context)
  "Returns the failure return set for the value-function of a
   `node-context'."

  (destructuring-bind (context-id . context) context
    (if (= context-id :input)
        node
        (failure-return (value-function context)))))


;;;; Expressions

(defgeneric failure-return (expression)
  (:documentation
   "Returns the set of the expression operands in which failures are
    returned directly."))


;;;; Operand Node References

(defmethod failure-return ((node node-link))
  "Returns the result of applying the function
   *OPERAND-FAILURE-RETURN* on the linked `node' object."

  (funcall *operand-failure-return* (node-link-node node)))


;;;; Functor Expressions

(defmethod failure-return ((functor functor-expression))
  (with-struct-slots functor-expression- (meta-node arguments outer-nodes)
      functor

    (typecase meta-node
      (meta-node
       (functor-failure-return
        meta-node
        (map #'failure-return arguments)

        (map
         (lambda (outer-node)
           (destructuring-bind (node . expression) outer-node
             (cons
              node
              (failure-return expression))))
         outer-nodes))))))

(defun functor-failure-return (meta-node arguments outer-nodes)
  "Returns the failure return set of a functor expression with
   operator META-NODE. ARGUMENTS and OUTER-NODES contain the failure
   return sets of the arguments and outer-nodes operands."

  (let ((values (alist-hash-map (pairlis (operand-node-names meta-node) arguments))))
    (foreach
     (lambda (node)
       (destructuring-bind (node . expression) node
         (-> (get node (outer-nodes meta-node))
             name
             (get values)
             (setf expression))))
     outer-nodes)

    (map-extend
     (lambda (operand)
       (get operand values))

     (meta-node-failure-return meta-node))))


(defgeneric meta-node-failure-return (meta-node)
  (:documentation
   "Returns the set of the operands of META-NODE (include outer-node
    operands), from which failures are returned directly."))

(defmethod meta-node-failure-return :around ((meta-node meta-node))
  "Returns the value of the FAILURE-RETURN attribute, or if not set,
   sets it to the result of calling the next method."

  (slet (attribute :failure-return meta-node)
    (or it (setf it (call-next-method)))))

(defmethod meta-node-failure-return ((meta-node final-meta-node))
  (let ((*operand-failure-return* #'name))
    (node-failure-return meta-node)))

(defmethod meta-node-failure-return ((meta-node external-meta-node))
  nil)

(defun failure-return-operands (meta-node)
  "Returns a list where each operand of META-NODE is substituted with
   true or false indicating whether META-NODE directly returns
   failures in that node."

  (let ((failure-return (meta-node-failure-return meta-node)))
    (map (rcurry #'memberp failure-return) (operand-node-names meta-node))))

(defun failure-return-outer-nodes (meta-node)
  "Returns a map where each outer node operand of META-NODE is mapped
   to true or false indicating whether META-NODE directly returns
   failures in that node."

  (with-accessors ((outer-nodes outer-nodes)) meta-node
    (let ((failure-return (meta-node-failure-return meta-node)))
      (map
       (lambda (outer-node)
         (let ((node (first outer-node)))
           (cons node (memberp (name node) failure-return))))
       outer-nodes))))



;;;; Expression Blocks

(defmethod failure-return ((block expression-block))
  (failure-return (expression-block-expression block)))


;;;; Literals

(defmethod failure-return ((expression t))
  "Returns NIL for literals as literals neither generate nor propagate
   failures."

  nil)


;;; Failure Propagation Analysis

(defvar *operand-failure-propagation* #'identity
  "Function of one argument (a `node' serving as the operand to an
   expression) which should return the failure propagation expression
   for that operand")


(defun returns-failure? (expression operands)
  "Returns true if the failure propagation expression (EXPRESSION)
   indicates that a failure is always returned for given values of the
   operands."

  (flet ((test? (expression)
           (returns-failure? expression operands)))

    (match expression
      ((list* 'or expressions)
       (some #'test? expressions))

      ((list* 'and expressions)
       (every #'test? expressions))

      ((list 'if _ expression)
       (test? expression))

      (operand
       (get operand operands operand)))))


;;;; Nodes

(defun node-failure-propagation (node)
  "Returns the failure propagation expression for a `node'."

  (with-slots (dependencies contexts) node
    (if (emptyp dependencies)
        node
        (-<> (curry #'context-failure-propagation node)
             (map-to 'list <> contexts)
             (list* 'or <>)))))

(defun context-failure-propagation (node context)
  "Returns the failure propagation expression for the value-function
   of a `node-context'."

  (destructuring-bind (context-id . context) context
    (if (= context-id :input)
        node
        (failure-propagation (value-function context)))))


;;;; Expressions

(defgeneric failure-propagation (expression)
  (:documentation
   "Returns a failure propagation expression for EXPRESSION."))


;;;; Operand Node References

(defmethod failure-propagation ((link node-link))
  "Returns the result of applying *OPERAND-FAILURE-PROPAGATION* on the
   linked `node' object."

  (funcall *operand-failure-propagation* (node-link-node link)))


;;;; Functor Expressions

(defmethod failure-propagation ((functor functor-expression))
  (with-struct-slots functor-expression- (meta-node arguments outer-nodes)
      functor

    (typecase meta-node
      (meta-node
       (functor-failure-propagation meta-node arguments outer-nodes))

      (otherwise
       (failure-propagation meta-node)))))

(defun functor-failure-propagation (meta-node arguments outer-nodes)
  "Returns the failure propagation expression for a functor expression
   in which the operator is a meta-node."

  (let ((values (alist-hash-map (pairlis (operand-node-names meta-node) arguments))))
    (foreach
     (lambda (node)
       (destructuring-bind (node . expression) node
         (-> (get node (outer-nodes meta-node))
             name
             (get values)
             (setf expression))))
     outer-nodes)

    (let ((operand-propagation
           (map #L(cons (car %1) (failure-propagation (cdr %1))) values)))

     (labels ((walk (expression)
                (match expression
                  ((list* (and (or 'or 'and) op) args)
                   (list* op (map #'walk args)))

                  ((list 'if (list 'not cond) expression)
                   `(if (not ,(get cond values)) ,(walk expression)))

                  ((list 'if cond expression)
                   `(if ,(get cond values) ,(walk expression)))

                  (operand
                   (get operand operand-propagation operand)))))

       (walk (meta-node-failure-propagation meta-node))))))

(defgeneric meta-node-failure-propagation (meta-node)
  (:documentation
   "Returns the failure propagation expression of the operands of
    META-NODE."))

(defmethod meta-node-failure-propagation :around ((meta-node meta-node))
  "Returns the value of the FAILURE-PROPAGATION attribute, or if not
   set, sets it to the result of calling the next method."

  (slet (attribute :failure-propagation meta-node)
    (or it (setf it (call-next-method)))))

(defmethod meta-node-failure-propagation ((meta-node final-meta-node))
  (let ((*operand-failure-propagation* #'name))
    (node-failure-propagation meta-node)))

(defmethod meta-node-failure-propagation ((meta-node external-meta-node))
  nil)


(defmethod failure-propagation ((list argument-list))
  (list* 'or (map #'failure-propagation (argument-list-arguments list))))


;;;; Expression Blocks

(defmethod failure-propagation ((block expression-block))
  (failure-propagation (expression-block-expression block)))


;;;; Literals

(defmethod failure-propagation ((expression t))
  "Returns NIL for literals as literals neither generate nor propagate
   failures."

  nil)
