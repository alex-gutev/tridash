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

(defun top-level? ()
  "Returns true if the declaration currently being processed is at the
   top level."

  (zerop *level*))

(defun unwrap-declaration (decl)
  "Converts a declaration, wrapped in `ATOM-NODE', `FUNCTOR-NODE' or
   `LITERAL-NODE' structures to a CL primitive representation."

  (match decl
    ((atom-node- identifier)
     identifier)

    ((functor-node- operator operands)
     (list* (unwrap-declaration operator)
            (map #'unwrap-declaration operands)))

    ((literal-node- value)
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
