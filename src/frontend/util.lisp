;;;; compiler.util.lisp
;;;;
;;;; Tridash Programming Language.
;;;; Copyright (C) 2021  Alexander Gutev
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

;;;; Utilities used by the frontend compiler

(in-package :tridash.frontend)

;;; Macros

(defmacro at-source (&body body)
  "Sets the node position to source in the dynamic extent of the forms
   in BODY. All declarations, processed by the forms in body, are
   treated as appearing in source position."

  `(let (*source-node*)
     ,@body))

(defmacro with-source-node (source &body body)
  "Evaluates the forms in BODY, with *SOURCE-NODE* bound to the
   result of the evaluation of the form SOURCE."

  `(let ((*source-node* ,source))
     ,@body))

(defmacro ensure-top-level (operator &body body)
  "Signals an error condition of type
   `SPECIAL-OPERATOR-REFERENCE-ERROR' if the declaration currently
   being processed is not at top-level."

  `(progn
     (unless (top-level?)
       (error 'special-operator-reference-error :operator ,operator))

     ,@body))

(defun top-level? ()
  "Returns true if the declaration currently being processed is at the
   top level."

  (zerop *level*))

(defmacro! match-syntax (operator o!args &body patterns)
  "Checks whether the arguments to an operator match the given pattern clauses

   OPERATOR:

      The operator, used in error reporting (evaluated).

   ARGS:

     The list of arguments (evaluated)

   PATTERNS:

     Optima pattern matching clauses.

     If none of the clauses match, a condition of type
     `INVALID-ARGUMENTS-ERROR' is signalled."

  `(match ,g!args
     ,@patterns

     (_
      (error 'invalid-arguments-error
             :operator ,operator
             :arguments ,g!args
             :expected ',(map #'first patterns)))))


;;; Optima Patterns

(defpattern atom (pattern)
  `(atom-expression- (identifier ,pattern)))

(defpattern functor (operator operands)
  `(functor-expression-
    (operator ,operator)
    (operands ,operands)))

(defpattern node (pattern)
  `(and (type
         (or atom-expression
             functor-expression
             node
             module))
        ,pattern))

(defpattern integer-literal (pattern)
  `(literal-value- (value (and (type integer) ,pattern))))

(defpattern string-literal (pattern)
  `(literal-value- (value (and (type string) ,pattern))))

(defpattern id (name)
  (check-type name string)

  `(eql (id-symbol ,name)))

(defpattern optional (&rest patterns)
  (reduce
   (lambda (pattern expr)
     `(or (cons ,pattern ,expr) nil))
   patterns
   :initial-value nil
   :from-end t))

(defpattern rest (pattern things)
  (with-gensyms (thing)
    `(guard ,things
            (and
             (listp ,things)
             (every
              (lambda (,thing)
                (match ,thing
                  (,pattern t)))
              ,things)))))


;;; Types

(deftype operator-associativity ()
  "Represents an operator associativity identifier symbol."

  `(member ,(id-symbol "left")
           ,(id-symbol "right")))


;;; Wrapping and Unwrapping Node Expressions

(defun wrap-expression (expression &optional (location '(0 . 0)))
  "Convert a list representation of a node expression into
   `NODE-EXPRESSION' objects."

  (match expression
    ((cons op operands)
     (functor-expression
      (wrap-expression op)
      (map #'wrap-expression operands)
      :location location))

    ((or t nil) expression)

    ((type symbol)
     (atom-expression expression :location location))

    ((type (or node-expression node))
     expression)

    (_
     (literal-value expression :location location))))

(defun unwrap-declaration (decl)
  "Converts a declaration, wrapped in `ATOM-EXPRESSION', `FUNCTOR-EXPRESSION' or
   `LITERAL-VALUE' structures to a CL primitive representation."

  (match decl
    ((atom-expression- identifier)
     identifier)

    ((node-list-expression- operands)
     (list* +list-operator+
            (map #'unwrap-declaration operands)))

    ((functor-expression- operator operands)
     (list* (unwrap-declaration operator)
            (map #'unwrap-declaration operands)))

    ((literal-value- value)
     value)

    ((type list)
     (map #'unwrap-declaration decl))

    (_ decl)))


;;; Node Name Canonicalization

(defun canonicalize-functor (operator operands)
  "Returns a canonicalized declaration with which the
   functor (OPERATOR OPERANDS) can be referenced, from
   *FUNCTOR-MODULE*."

  (flet ((canonicalize-operand (operand)
           (match operand
             ((list (eql +optional-argument+) name value)
              (list +optional-argument+
                    name
                    (canonicalize-node value)))

             (_ (canonicalize-node operand)))))

   (map #'canonicalize-operand (cons operator operands))))

(defun canonicalize-node (node)
  "Returns a canonicalized declaration with which NODE can be
   referenced, from *FUNCTOR-MODULE*."

  (match node
    ((type node)
     (let* ((home (home-module node)))
       (if (or (outer-module home) (= home *functor-module*))
           (name node)
           (list +in-module-operator+ (name home) (name node)))))

    ((context-node node)
     (canonicalize-node node))

    (_ node)))
