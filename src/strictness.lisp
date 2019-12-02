;;;; strictness.lisp
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

;;;; Strictness analysis of nodes. Determines which nodes may be
;;;; evaluated strictly and the resulting values returned directly
;;;; rather than in 'thunks'.

(defpackage :tridash.frontend.strictness
  (:use :generic-cl
        :alexandria
        :anaphora
        :optima
        :cl-arrows

        :tridash.util
	:tridash.parser
        :tridash.frontend)

  (:shadowing-import-from :generic-cl
                          :emptyp
                          :accumulate
                          :multiply)

  (:import-from :tridash.frontend
                :process-attribute)

  (:export :*analyze-nodes*
           :*return-blocks*

           :analyze
           :analyze-meta-node
           :analyze-expression
           :meta-node-strictness
           :strict-arguments
           :strict-outer-operands
           :strict?))

(in-package :tridash.frontend.strictness)


;;;; Strictness Analysis of Nodes

(defun analyze (node)
  "Analyze the NODE for strictness. A strictness expression of the
   input nodes, reached from this node, is returned. If this node is
   an input node, it is returned."

  (with-slots (dependencies contexts) node
    (if (emptyp dependencies)
        node
        (list* 'or (map-to 'list (curry #'analyze-context node) contexts)))))

(defun analyze-context (node context)
  "Analyze the `NODE-CONTEXT' CONTEXT, of NODE, for strictness. If
   CONTEXT is an input context returns node otherwise creates a
   strictness expression which is a function of the strictness of the
   context's operands."

  (destructuring-bind (context-id . context) context
    (if (= context-id :input)
        node
        (analyze-expression (value-function context)))))


(defun strict-arguments (meta-node)
  "Returns a list where each item is TRUE if the corresponding operand
   of META-NODE can be strictly evaluated or false if it has to be
   passed as a thunk."

  (with-slots (operands) meta-node
    (-<>> (meta-node-strictness meta-node)
          (curry #'strict?)
          (map <> (operand-node-names meta-node)))))

(defun strict-outer-operands (meta-node)
  "Determines which outer-node operands, of META-NODE, should be
   evaluated strictly. Returns a map, mapping outer nodes to the
   values t, if the outer node should be evaluated strictly, or nil,
   if it should be evaluated lazily."

  (let ((strictness (meta-node-strictness meta-node)))
    (map
     (lambda (outer-node)
       (destructuring-bind (node . local-node) outer-node
         (cons
          node
          (strict? strictness (name local-node)))))
     (outer-nodes meta-node))))

(defun meta-node-strictness (meta-node)
  "Returns the strictness expression of META-NODE's operands. If the
   strictness has not yet been computed, computes it by
   ANALYZE-META-NODE and stores it in the :STRICTNESS attribute."

  (finish-build-meta-node meta-node)

  (or (attribute :strictness meta-node)
      (setf (attribute :strictness meta-node)
            (analyze-meta-node meta-node))))

(defgeneric analyze-meta-node (meta-node)
  (:documentation
   "Analyze strictness of META-NODE's operands. The strictness
    expression of the operands is returned.")

  (:method ((meta-node external-meta-node))
    nil))

(defmethod analyze-meta-node ((meta-node final-meta-node))
  (labels ((replace-operands (expression)
             (match expression
               ((eql meta-node)
                nil)

               ((type node)
                (name expression))

               ((list* op args)
                (list* op (map #'replace-operands args))))))

    ;; Initially set all arguments to strict in case of recursive
    ;; calls
    (setf (attribute :strictness meta-node)
          (list* 'or
                 (append
                  (operand-node-names meta-node)
                  (outer-node-operand-names meta-node))))

    (replace-operands (analyze meta-node))))


;;;; Strictness Analysis of Expressions

(defvar *analyze-nodes* t
  "If true linked nodes, by `NODE-LINK' objects, are analyzed for
   strictness.")

(defvar *return-blocks* nil
  "If true the strictness expression, returned by ANALYZE-EXPRESSION,
   includes each `EXPRESSION-BLOCK' object encountered.")


(defgeneric analyze-expression (expression)
  (:documentation
   "Analyze the expression for strictness. The expression tree is
    traversed and a strictness expression consisting of OR and AND
    expressions is returned. Linked operand nodes, by `NODE-LINK'
    objects, are traversed by ANALYZE."))

;;; Literals and `NODE-LINK's

(defmethod analyze-expression ((link node-link))
  "Analyzes a linked node for strictness by traversing the node by
   ANALYZE."

  (with-struct-slots node-link- (node) link
    (if *analyze-nodes*
        (analyze node)
        node)))

(defmethod analyze-expression ((expression t))
  "Returns NIL for literals, as a literal is never equal to an input
   node."

  nil)

(defmethod analyze-expression ((block expression-block))
  (let ((block-strictness
         (analyze-expression (expression-block-expression block))))

    (if *return-blocks*
        `(or ,block ,block-strictness)
        block-strictness)))


;;; Object and Member Expressions

(defmethod analyze-expression ((object object-expression))
  "Returns NIL since the fields of an `OBJECT-EXPRESSION' are never
   strictly evaluated."

  nil)


;;; Functor Expressions

(defmethod analyze-expression ((functor functor-expression))
  "Analyzes `FUNCTOR-EXPRESSION's for strictness. If the operator is a
   `META-NODE' obtains its strictness for the operand nodes. If the
   operator is not a `META-NODE', the arguments are assumed to be
   non-strict and NIL is returned."

  (with-struct-slots functor-expression- (meta-node arguments outer-nodes) functor
    (typecase meta-node
      (meta-node
       (analyze-functor
        meta-node
        (map #'analyze-expression arguments)

        (map
         (lambda (outer-node)
           (destructuring-bind (node . expression) outer-node
             (cons
              node
              (analyze-expression expression))))
         outer-nodes)))

      (otherwise
       (analyze-expression meta-node)))))

(defun analyze-functor (meta-node arguments outer-nodes)
  "Analyze the strictness of a `META-NODE' functor expression with
   operator META-NODE and operands ARGUMENTS. The strictness
   expression of META-NODE is returned with the operands replaced with
   the corresponding expressions in ARGUMENTS. The outer-node operands
   are replaced with the corresponding expressions in OUTER-NODES."

  (let ((values (alist-hash-map (pairlis (operand-node-names meta-node) arguments))))
    (foreach
     (lambda (node)
       (destructuring-bind (node . expression) node
         (-> (get node (outer-nodes meta-node))
             name
             (get values)
             (setf expression))))
     outer-nodes)

    (labels ((walk (expression)
               (match expression
                 ((list* (and (or 'or 'and) op) args)
                  (list* op (map #'walk args)))

                 (operand
                  (get operand values)))))

      (walk (meta-node-strictness meta-node)))))

(defmethod analyze-expression ((list argument-list))
  (->> list
       argument-list-arguments
       (map #'analyze-expression)
       (list* 'or)))


;;;; Evaluating Strictness Expressions

(defun strict? (expression node)
  "Evaluates the strictness expression EXPRESSION and returns true if
   NODE can be strictly evaluated."

  (match expression
    ((list* 'or expressions)
     (some (rcurry #'strict? node) expressions))

    ((list* 'and expressions)
     (every (rcurry #'strict? node) expressions))

    ((eql node) t)))


;;;; Strictness Attribute

(defmethod process-attribute ((node t) (attribute (eql (id-symbol "STRICTNESS"))) value (module t))
  "Process STRICTNESS attributes. Replaces the tridash or and and
   symbols with CL:OR and CL:AND."

  (labels ((replace-syms (x)
             (match x
               ((eql (id-symbol "or"))
                'or)

               ((eql (id-symbol "and"))
                'and)

               ((type list)
                (map #'replace-syms x))

               (_ x))))
    (replace-syms value)))
