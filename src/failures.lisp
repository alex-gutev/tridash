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
                :process-attribute
                :call-meta-node
                :resolve))

(in-package :tridash.frontend.failures)

(in-readtable cut-syntax)


;;; Failure Propagation Expressions

(defstruct
    (conditional-propagation
      (:constructor conditional-propagation (condition then)))

  "Represents a failure THEN which is propagated only when CONDITION
   is true."

  condition
  then)

(defstruct
    (or-propagation
      (:constructor or-propagation (operands)))

  "Represents a failure which is propagated if at least one of
   OPERANDS propagates a failure."

  operands)

(defstruct
    (and-propagation
      (:constructor and-propagation (operands)))

  "Represents a failure which is propagated if all OPERANDS propagated
   failures."

  operands)

(defstruct
    (fail-with-type (:constructor fail-with-type (operand)))

  "Represents a failure, which is always propagated, of a given
   type. OPERAND may either be a failure type or the name of a
   `meta-node' operand."

  operand)

(defstruct
    (operand-propagation (:constructor operand-propagation (operand)))

  "Represents a failure which is propagated from an operand OPERAND of
   a `meta-node'."

  operand)

(defgeneric map-propagation-operands (fn expression)
  (:documentation
   "Returns a new failure propagation expression with FN applied on
    each subexpression of EXPRESSION.")

  (:method (fn (expression conditional-propagation))
    (with-struct-slots conditional-propagation- (condition then)
        expression

      (conditional-propagation
       (funcall fn condition)
       (funcall fn then))))

  (:method (fn (expression or-propagation))
    (->> expression
         or-propagation-operands
         (map fn)
         or-propagation))

  (:method (fn (expression and-propagation))
    (->> expression
         and-propagation-operands
         (map fn)
         and-propagation))

  (:method (fn (expression fail-with-type))
    (->> expression
         fail-with-type-operand
         (funcall fn)
         fail-with-type))

  (:method (fn (expression operand-propagation))
    (->> expression
         operand-propagation-operand
         (funcall fn)
         operand-propagation))

  (:method (fn (n null))
    (declare (ignore fn))

    nil))

(defun parse-failure-propagation (expression)
  "Converts a list representation of a failure propagation expression
   to the structure representation."

  (ematch expression
    ((list 'if (list 'not (and (type symbol) condition)) then)
     (conditional-propagation
      (list 'not (operand-propagation condition))
      (parse-failure-propagation then)))

    ((list 'if (and (type symbol) condition) then)
     (conditional-propagation
      (operand-propagation condition)
      (parse-failure-propagation then)))

    ((list* 'or operands)
     (or-propagation (map #'parse-failure-propagation operands)))

    ((list* 'and operands)
     (and-propagation (map #'parse-failure-propagation operands)))

    ((list 'type (and (type symbol) operand))
     (fail-with-type operand))

    ((type symbol)
     (operand-propagation expression))))


;;; Optimization of Catch Expressions

(defun optimize-catch-expression (expression)
  "Replaces failure generating expression, within the TRY argument of
   a catch functor expression, with the CATCH argument itself.

   It is assumed that EXPRESSION is a `functor-expression' of the
   builtin catch meta-node."

  (with-struct-slots functor-expression- (meta-node arguments)
      expression

    (destructuring-bind (try catch &optional test)
        arguments

      (if-let (test-type (get-failure-test-function test))
        (catch-expression
         (optimize-fail-expressions try catch test-type)
         catch
         test)

        expression))))

(defun optimize-fail-expressions (expression catch test-type)
  "Replaces expressions, which generate a failure of a type for which
   the predicate TEST-TYPE returns true, with CATCH."

  (let ((try (optimize-catch-sub-expression expression catch test-type)))
    (let* ((catch (expression-block (uncatch-expression catch)))
           (try (replace-failure-expressions
                 try
                 catch
                 :type test-type)))

      (-> try
          failure-propagation
          (simplify-failure-propagation :type test-type)
          (conditionalize
           catch
           try)))))

(defun optimize-catch-sub-expression (expression catch test-type)
  "If EXPRESSION is a catch expression, replaces failure generating
   expressions, which generate failures of type satisfied by the
   predicate TEST-TYPE, with CATCH, within the catch argument of
   EXPRESSION."

  (match expression
    ((functor-expression-
      (meta-node (eql (get :catch tridash.frontend::*core-meta-nodes*)))
      (arguments (list sub-try sub-catch sub-test)))

     (optimize-catch-expression
      (catch-expression
       sub-try
       (optimize-fail-expressions sub-catch catch test-type)
       sub-test)))

    (_ expression)))

(defgeneric simplify-failure-propagation (expression &key type)
  (:documentation
   "Simplifies a failure propagation expression, removing always true
    or always false expressions.

    TYPE is an optional predicate function which is applied on a
    failure type, returned by a subexpression, to determine whether
    the type is handled by the enclosing catch expression.")

  (:method ((expression conditional-propagation) &key type)
    (with-struct-slots conditional-propagation- (condition then)
        expression

      (awhen (simplify-failure-propagation then :type type)
        (conditional-propagation condition it))))

  (:method ((expression or-propagation) &key type)
    (let ((expressions
           (->> expression
                or-propagation-operands
                (map (rcurry #'simplify-failure-propagation :type type))
                (remove nil))))
      (when expressions
        (if (rest expressions)
            (or-propagation expressions)
            (first expressions)))))

  (:method ((expression and-propagation) &key type)
    (let ((expressions
           (->> expression
                and-propagation-operands
                (map (rcurry #'simplify-failure-propagation :type type))
                (remove nil))))
      (when expressions
        (if (rest expressions)
            (and-propagation expressions)
            (first expressions)))))

  (:method ((expression fail-with-type) &key type)
    (funcall type (fail-with-type-operand expression)))

  (:method (expression &key type)
    (declare (ignore type))
    expression))

(defgeneric conditionalize (condition then else)
  (:documentation
   "Returns an if expression with condition specified by the failure
    propagation expression CONDITION, returning THEN if true and ELSE
    if false.")

  (:method ((expression conditional-propagation) then else)
    (match expression
      ((conditional-propagation
        (condition (list 'not condition))
        (then expression))

       (if-expression
        condition

        else
        (conditionalize expression then else)))

      ((conditional-propagation condition (then expression))
       (if-expression
        condition

        (conditionalize expression then else)
        else))))

  (:method ((expression or-propagation) then else)
    (-<> expression
         or-propagation-operands
         (reduce #L(conditionalize %1 then %2) <> :from-end t :initial-value else)))

  (:method ((expression and-propagation) then else)
    (-<> expression
         and-propagation-operands
         (reduce #L(conditionalize %1 %2 else) <> :from-end t :initial-value then)))

  (:method ((n null) then else)
    (declare (ignore then))
    else)

  (:method ((expression (eql t)) then else)
    (declare (ignore else))
    then))


(defun get-failure-test-function (test-argument)
  "Compiles the Tridash function designated by TEST-ARGUMENT to a CL
   function."

  (let ((test-function test-argument))
    (if test-function
        (build-test-function test-function)
        (constantly t))))

(defun build-test-function (expression)
  "Builds the failure type test function designated by EXPRESSION. If
   the function cannot be built, due to EXPRESSION not being a
   constant expression, NIL is returned."

  (labels ((extract-value (type)
             (match type
               ((expression-block- expression)
                (extract-value expression))

               ((node-ref node)
                node)

               (_ type))))

    (when (constant-expression? expression)
      (let ((tmp
             (make-instance 'built-meta-node
                            :name nil
                            :operands nil
                            :definition
                            (make-instance 'flat-node-table
                                           :nodes (hash-set)
                                           :meta-nodes (hash-set)
                                           :input-nodes (hash-set)))))
        (setf (value-function (context tmp nil)) expression)

        (let ((fn (call-meta-node tmp nil)))
          (lambda (type)
            (handler-case
                (when (constant-expression? type)
                  (resolve
                   (funcall fn (extract-value type))))
              (tridash-fail () nil))))))))


(defun constant-expression? (expression)
  "Returns true if EXPRESSION is a constant expression."

  (flet ((constant? (expression)
           (when (node-link-p expression)
             (return-from constant-expression? nil))
           t))

    (walk-expression #'constant? expression)
    t))


;;; Replacing Failures with Catch Values

(defgeneric replace-failure-expressions (expression catch-value &key type)
  (:documentation
   "Returns an expression where each sub-expression, of EXPRESSION,
    which generates a failure is replaced with CATCH-VALUE.

    The second return value is T if EXPRESSION itself was replaced,
    NIL otherwise.

    The TYPE function, if provided, is used to determine whether a
    failure type ( is handled by the enclosing catch expression."))


;;;; Functor Expressions

(defmethod replace-failure-expressions ((functor functor-expression) catch-value &key (type (constantly t)))
  ;; Replace failure generating expressions only in those arguments in
  ;; which the meta-node returns failures.

  (labels ((map-arg (argument returns-fail?)
             (multiple-value-bind (argument fail-type)
                 (-<> (if returns-fail? catch-value :fail)
                      (replace-failure-expressions argument <> :type type))

               (cons argument fail-type)))

           (map-outer-node (outer-node returns-fail?)
             (destructuring-bind (node . expression)
                 outer-node
               (cons node (map-arg expression returns-fail?)))))

    (with-struct-slots functor-expression- (meta-node arguments outer-nodes)
        functor

      (typecase meta-node
        (meta-node

         (let* ((arguments (map #'map-arg arguments (failure-return-operands meta-node)))
                (fail-operands
                 (nconcatenate
                  (map #'map-outer-node outer-nodes (failure-return-outer-nodes meta-node))
                  (pairlis (operand-node-names meta-node) arguments)))

                (result
                 (functor-expression meta-node
                                     (map #'first arguments)

                                     :outer-nodes
                                     (map #L(cons (first %1) (second %1)) outer-nodes))))

           (aif (returns-failure? (meta-node-failure-propagation meta-node) fail-operands :type type)
                (values
                 (if (= catch-value :fail)
                     (fail-expression (second it))
                     catch-value)
                 it)

                (values result nil))))

        (otherwise
         functor)))))


;;;; Literals

(defmethod replace-failure-expressions (literal catch-value &key type)
  (declare (ignore catch-value type))
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


(defgeneric returns-failure? (expression operands &key type)
  (:documentation
   "If the failure propagation expression (EXPRESSION) indicates that
    a failure is always returned, for the given values of the
    operands, a list is returned where the first element is T and the
    second element is the deduced failure type. Otherwise NIL is
    returned.

    The TYPE function, if provided, is used to determine whether a
    failure type ( is handled by the enclosing catch expression.")

  (:method ((conditional conditional-propagation) operands &key (type #'identity))
    (returns-failure? (conditional-propagation-then conditional) operands :type type))

  (:method ((expression or-propagation) operands &key (type #'identity))
    (some (rcurry #'returns-failure? operands :type type)
          (or-propagation-operands expression)))

  (:method ((expression and-propagation) operands &key (type #'identity))
    (every (rcurry #'returns-failure? operands :type type)
           (and-propagation-operands expression)))

  (:method ((expression fail-with-type) operands &key (type #'identity))
    (let ((operand-value
           (-<> expression
                fail-with-type-operand
                (get <> operands <>)
                first)))
      (and (funcall type operand-value)
           (list t operand-value))))

  (:method ((operand operand-propagation) operands &key (type #'identity))
    (aand (cdr (get (operand-propagation-operand operand) operands))
          (funcall type (second it))
          it))

  (:method ((n null) operands &key (type #'identity))
    (declare (ignore type))
    nil))


;;;; Nodes

(defun node-failure-propagation (node)
  "Returns the failure propagation expression for a `node'."

  (with-slots (dependencies contexts) node
    (if (emptyp dependencies)
        node
        (-<> (curry #'context-failure-propagation node)
             (map-to 'list <> contexts)
             or-propagation))))

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

      (labels ((replace-operands (expression)
                 (match expression
                   ((conditional-propagation-
                     (condition
                      (list 'not (operand-propagation- (operand condition))))
                     then)

                    (conditional-propagation
                     (list 'not (get condition values))
                     (replace-operands then)))

                   ((conditional-propagation-
                     (condition
                      (operand-propagation- (operand condition)))
                     then)

                    (conditional-propagation
                     (get condition values)
                     (replace-operands then)))

                   ((conditional-propagation- condition then)
                    (conditional-propagation
                     (replace-condition condition)
                     (replace-operands then)))

                   ((operand-propagation operand)
                    (get operand operand-propagation))

                   ((fail-with-type operand)
                    (fail-with-type (get operand values operand)))

                   (_
                    (map-propagation-operands #'replace-operands expression))))

               (replace-condition (expression)
                 (match expression
                   ((list 'not condition)
                    (list 'not (replace-expression condition)))

                   (_ (replace-expression expression))))

               (replace-expression (expression)
                 (match expression
                   ((node-link node)
                    (get (name node) values))

                   ((expression-block- expression)
                    (replace-expression expression))

                   (_
                    (map-expression! #'replace-expression expression)))))

        (replace-operands (meta-node-failure-propagation meta-node))))))

(defgeneric meta-node-failure-propagation (meta-node)
  (:documentation
   "Returns the failure propagation expression of the operands of
    META-NODE."))

(defmethod meta-node-failure-propagation :around ((meta-node meta-node))
  "Returns the value of the FAILURE-PROPAGATION attribute, or if not
   set, sets it to the result of calling the next method."

  (slet (attribute :failure-propagation meta-node)
    (if (consp it)
        (setf it (parse-failure-propagation it))
        (or it (setf it (call-next-method))))))

(defmethod meta-node-failure-propagation ((meta-node final-meta-node))
  (let ((*operand-failure-propagation* #'name))
    (node-failure-propagation meta-node)))

(defmethod meta-node-failure-propagation ((meta-node external-meta-node))
  nil)


(defmethod failure-propagation ((list argument-list))
  (->> list
       argument-list-arguments
       (map #'failure-propagation)
       or-propagation))


;;;; Expression Blocks

(defmethod failure-propagation ((block expression-block))
  (failure-propagation (expression-block-expression block)))


;;;; Literals

(defmethod failure-propagation ((expression t))
  "Returns NIL for literals as literals neither generate nor propagate
   failures."

  nil)
