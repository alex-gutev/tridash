;;;; expressions.lisp
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

;;;; Intermediate Representation for Value Function Expressions

(in-package :tridash.frontend)

(defstruct (node-link (:constructor node-link (node &key context)))
  "Represents a reference to the value of NODE. CONTEXT is the
   identifier of the context in which this expression appears."

  node
  context)

(defstruct (functor-expression (:constructor functor-expression (meta-node arguments)))
  "Represents a functor expression with a META-NODE applied to
   ARGUMENTS."

  meta-node
  arguments)

(defstruct (object-expression (:constructor object-expression (&optional entries)))
  "Represents a create dictionary object expression with entries
   ENTRIES."

  entries)

(defstruct (catch-expression (:constructor catch-expression (main catch)))
  "Represents a catch expression which evaluates to the MAIN
   expression or CATCH if the main expression fails to evaluate to a
   value."

  main catch)

(defstruct (fail-expression (:constructor fail-expression (&optional type)))
  "Represents a fail expression which signals an evaluation failure,
   with TYPE being the failure reason."

  type)

(defstruct
    (expression-group (:constructor expression-group (expression &key count save)))
  "Groups expressions which occur in multiple location within a value
   function.

   COUNT is the number of locations in which the expression
   occurs.

   SAVE is a flag which is true if the expression's value should be
   saved between value updates."

  expression
  (count 1)

  (save nil))

(defstruct (meta-node-ref (:constructor meta-node-ref (node &key outer-nodes)))
  "Represents a meta-node reference. NODE is the `META-NODE' object
   and OUTER-NODES is the list of outer nodes referenced by the
   meta-node which are append to it."

  node
  outer-nodes)


(defgeneric walk-expression (fn expression)
  (:documentation
   "Calls FN on each expression in the tree rooted at EXPRESSION. If
    FN returns NIL, FN is not called on the expressions contained in
    the expression passed as an argument.")

  (:method :around (fn expr)
    (when (funcall fn expr)
      (call-next-method)))

  (:method (fn (call functor-expression))
    (foreach (curry #'walk-expression fn) (functor-expression-arguments call)))

  (:method (fn (object object-expression))
    (foreach (compose (curry #'walk-expression fn) #'second)
             (object-expression-entries object)))

  (:method (fn (catch catch-expression))
    (walk-expression fn (catch-expression-main catch))
    (walk-expression fn (catch-expression-catch catch)))

  (:method (fn (fail fail-expression))
    (awhen (fail-expression-type fail)
      (walk-expression it fn)))

  (:method (fn (group expression-group))
    (walk-expression fn (expression-group-expression group)))

  (:method (fn (ref meta-node-ref))
    (foreach (curry #'walk-expression fn) (meta-node-ref-outer-nodes ref)))

  (:method ((fn t) (expr t))
    nil))

(defgeneric map-expression! (fn expression)
  (:documentation
   "Applies a transformation on an expression. Each sub-expression,
    contained in EXPRESSION, is replaced with the result of applying
    FN on it.

    CAUTION: Whilst the new expression tree is returned, this function
    is destructive when EXPRESSION is an `EXPRESSION-GROUP', since the
    identity of an expression group needs to be preserved." )

  (:method (fn (call functor-expression))
    (functor-expression
     (funcall fn (functor-expression-meta-node call))
     (map fn (functor-expression-arguments call))))

  (:method (fn (object object-expression))
    (flet ((map-entry (entry)
             (list (first entry)
                   (funcall fn (second entry)))))

      (object-expression
       (map #'map-entry (object-expression-entries object)))))

  (:method (fn (catch catch-expression))
    (catch-expression
     (funcall fn (catch-expression-main catch))
     (funcall fn (catch-expression-catch catch))))

  (:method (fn (fail fail-expression))
    (fail-expression
     (awhen (fail-expression-type fail)
       (funcall fn it))))

  (:method (fn (group expression-group))
    (with-accessors ((expression expression-group-expression)) group
      (setf expression (funcall fn expression))))

  (:method (fn (ref meta-node-ref))
    (meta-node-ref
     (meta-node-ref-node ref)

     :outer-nodes
     (map (curry #'map-expression! fn) (meta-node-ref-outer-nodes ref))))

  (:method ((fn t) expr)
    expr))
