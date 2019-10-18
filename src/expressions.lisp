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

(defstruct (node-ref (:constructor node-ref (node)))
  "Represents a direct reference to NODE. A direct reference is a
   reference which does not involve a binding between nodes."

  node)

(defstruct (functor-expression (:constructor functor-expression (meta-node arguments &key (outer-nodes (make-hash-map)))))
  "Represents a functor expression with a META-NODE applied to
   ARGUMENTS.

   OUTER-NODES is the map mapping the outer `NODE' objects, referenced
   by META-NODE, to the corresponding expressions which compute their
   values."

  meta-node
  arguments
  outer-nodes)

(defstruct (object-expression (:constructor object-expression (&optional entries)))
  "Represents a create dictionary object expression with entries
   ENTRIES."

  entries)

(defstruct
    (expression-block (:constructor expression-block (expression &key count)))

  "Represents expressions which occur in multiple locations within a
   value function.

   EXPRESSION is the actual expression and COUNT is the number of
   locations in which the expression occurs."

  expression
  (count 1))

(defstruct (meta-node-ref (:constructor meta-node-ref (node &key optional (outer-nodes (make-hash-map)))))
  "Represents a meta-node reference. NODE is the `META-NODE' object.

   OPTIONAL is the list of default values for the optional arguments.

   OUTER-NODES is the map mapping the outer `NODE' objects, referenced
   by META-NODE, to the corresponding expressions which compute their
   values."

  node
  optional
  outer-nodes)

(defstruct (argument-list (:constructor argument-list (arguments)))
  "Expression type representing a list used to pass the rest
   arguments."

  arguments)


(defgeneric walk-expression (fn expression)
  (:documentation
   "Calls FN on each expression in the tree rooted at EXPRESSION. If
    FN returns NIL, FN is not called on the expressions contained in
    the expression passed as an argument.")

  (:method :around (fn expr)
    (when (funcall fn expr)
      (call-next-method)))

  (:method (fn (call functor-expression))
    (with-struct-slots functor-expression- (meta-node arguments outer-nodes) call
      (walk-expression fn meta-node)
      (foreach (curry #'walk-expression fn) arguments)
      (foreach (curry #'walk-expression fn) (map-values outer-nodes))))

  (:method (fn (object object-expression))
    (foreach (compose (curry #'walk-expression fn) #'second)
             (object-expression-entries object)))

  (:method (fn (block expression-block))
    (walk-expression fn (expression-block-expression block)))

  (:method (fn (ref meta-node-ref))
    (with-struct-slots meta-node-ref- (optional outer-nodes) ref
      (foreach (curry #'walk-expression fn) optional)
      (foreach (curry #'walk-expression fn) (map-values outer-nodes))))

  (:method (fn (list argument-list))
    (foreach (curry #'walk-expression fn) (argument-list-arguments list)))

  (:method (fn (link node-link))
    (with-struct-slots node-link- (node) link
      (unless (node? node)
        (walk-expression fn node))))

  (:method ((fn t) (expr t))
    nil))

(defgeneric map-expression! (fn expression)
  (:documentation
   "Applies a transformation on an expression. Each sub-expression,
    contained in EXPRESSION, is replaced with the result of applying
    FN on it.

    CAUTION: Whilst the new expression tree is returned, this function
    is destructive when EXPRESSION is an `EXPRESSION-BLOCK', since the
    identity of an expression block needs to be preserved." )

  (:method (fn (call functor-expression))
    (with-struct-slots functor-expression- (meta-node arguments outer-nodes) call
      (functor-expression
       (funcall fn meta-node)
       (map fn arguments)

       :outer-nodes
       (map
        (lambda (ref)
          (cons (car ref) (funcall fn (cdr ref))))
        outer-nodes))))

  (:method (fn (object object-expression))
    (flet ((map-entry (entry)
             (list (first entry)
                   (funcall fn (second entry)))))

      (object-expression
       (map #'map-entry (object-expression-entries object)))))

  (:method (fn (block expression-block))
    (with-accessors ((expression expression-block-expression)) block
      (setf expression (funcall fn expression))))

  (:method (fn (ref meta-node-ref))
    (with-struct-slots meta-node-ref- (node optional outer-nodes) ref
      (meta-node-ref
       node

       :optional
       (map fn optional)

       :outer-nodes
       (map
        (lambda (ref)
          (cons (car ref) (funcall fn (cdr ref))))
        outer-nodes))))

  (:method (fn (list argument-list))
    (->> list
         argument-list-arguments
         (map fn)
         argument-list))

  (:method ((fn t) expr)
    expr))
