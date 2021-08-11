;;;; operators.lisp
;;;;
;;;; Tridash Programming Language.
;;;; Copyright (C) 2018  Alexander Gutev
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

(in-package :tridash.frontend)

;;;; Special Operator Constants

;;; Basic Operators

(defconstant +bind-operator+ :bind
  "Operator for establishing bindings between nodes.")

(defconstant +op-operator+ (id-symbol "/operator")
  "Operator for marking nodes as infix operators.")

(defconstant +quote-operator+ (id-symbol "/quote")
  "Operator for suppressing processing of declarations.")

(defconstant +ref-operator+ (id-symbol :ref)
  "Operator for referencing another node without creating a binding.")


;;; Contexts

(defconstant +context-operator+ (id-symbol "/context")
  "Operator for explicitly specifying the context to which bindings
   are established.")

(defconstant +state-operator+ (id-symbol "/state")
  "Operator for establishing a binding between different states of a
   node.")


;;; Subnodes and Outer Nodes.

(defconstant +outer-operator+ :outer
  "Special operator for referencing nodes defined in an outer scope.")

(defconstant +subnode-operator+ :subnode
  "Special operator for accessing sub-nodes of a node.")


;;; Meta-nodes

(defconstant +def-operator+ :define
  "Operator for defining meta-nodes.")

(defconstant +extern-operator+ (id-symbol "/external")
  "Special operator for adding stubs for meta-nodes which are defined
   externally.")

(defconstant +self-node+ (id-symbol "self")
  "Special node which references the current meta-node.")


;;; Attributes

(defconstant +attribute-operator+ (id-symbol "/attribute")
  "Special operator for setting node attributes.")

(defconstant +attribute-processor-operator+ (id-symbol "/attribute-processor")
  "Special operator for setting a meta-node to process a particular
   attribute.")


;;; Modules

(defconstant +module-operator+ (id-symbol "/module")
  "Operator which sets the current module.")

(defconstant +import-operator+ (id-symbol "/import")
  "Operator for importing nodes, from another module, into the current
   module.")

(defconstant +alias-operator+ (id-symbol "/use-as")
  "Operator for adding an alias for a module to the current module.")

(defconstant +use-operator+ (id-symbol "/use")
  "Operator for using a module, using its name as an alias, from the
   current module.")

(defconstant +export-operator+ (id-symbol "/export")
  "Operator for adding a node to the public nodes table of the current
   module.")

(defconstant +in-module-operator+ (id-symbol "/in")
  "Operator for referencing a node in another module which does not
   have an alias in the current module.")


(defconstant +special-operators+
  (list +bind-operator+
        +context-operator+

        +def-operator+
        +extern-operator+
        +op-operator+
        +outer-operator+
        +subnode-operator+
        +attribute-operator+
        +module-operator+
        +import-operator+
        +use-operator+
        +export-operator+
        +in-module-operator+
        +list-operator+

        +quote-operator+)

  "List of all special operators")


;;; Infix Operators

(defconstant +infix-operators+
  (alist-hash-map
   `((:open-paren 900)))

  "Initial infix operator table.")


;;; Utilities for Processing Special Operators

(defun make-syntax-patterns (patterns)
  (labels ((make-pattern (pattern place)
             (let ((place (or place '_)))
               (match pattern
                 ('atom
                  `(or (atom-expression- (identifier ,place))
                       (and (type symbol) ,place)))

                 ('node
                  `(and (or (type atom-expression)
                            (type functor-expression)
                            (type symbol)
                            (type list)
                            (type node)
                            (type module))
                        ,place))

                 ((list 'atom value)
                  (let ((sym (id-symbol value)))
                    `(or (atom-expression- (identifier (and ',sym ,place)))
                         (and ',sym ,place))))

                 ((and (or 'integer 'string) type)
                  `(or (literal-value- (value (and (type ,type) ,place)))
                       (and (type ,type) ,place)))

                 ((list* 'functor (list op-pattern op-place) args)
                  `(and
                    (or (functor-expression-
                         (operator ,(make-pattern op-pattern op-place))
                         (operands ,(make-patterns args)))

                        (cons ,(make-pattern op-pattern op-place)
                              ,(make-patterns args)))
                    ,place))

                 ((cons (and (or 'or 'and) op) patterns)
                  (cons op (map (rcurry #'make-pattern place) patterns)))

                 ('any place))))

           (make-patterns (patterns)
             (match patterns
               ((list (list 'rest (list* pattern (or (list place) nil))))
                (with-gensyms (x)
                  `(guard
                    ,place
                    (and (listp ,place)
                         (every
                          (lambda (,x)
                            (declare (ignorable ,x))
                            (match ,x
                              (,(make-pattern pattern '_) t)))
                          ,place)))))

               ((cons (list 'optional pattern) rest)
                `(or ,(make-patterns (cons pattern rest)) nil))

               ((cons (list* pattern (or (list place) nil)) rest)
                (list
                 'cons
                 (make-pattern pattern place)
                 (make-patterns rest))))))

    (make-patterns patterns)))

(defmacro match-syntax (operator (&rest patterns) args &body body)
  (labels ((extract-pattern (pattern)
             (match pattern
               ((or
                 (list (or 'rest 'optional) (list* pattern _))
                 (list* pattern _)
                 pattern)
                pattern))))

    `(match ,args
       (,(make-syntax-patterns patterns)
         ,@body)

       (_
        (error 'invalid-arguments-error
               :operator ,operator
               :arguments ,args
               :expected ',(map #'extract-pattern patterns))))))

(defmacro match-syntax* (operator args &body patterns)
  (labels ((extract-patterns (patterns)
             (map #'extract-pattern patterns))

           (extract-pattern (pattern)
             (match pattern
               ((or
                 (list (or 'rest 'optional) (list* pattern _))
                 (list* pattern _)
                 pattern)
                pattern))))

    `(match ,args
       ,@(loop for (pat . body) in patterns
            collect (list* (make-syntax-patterns pat) body))

       (_
        (error 'invalid-arguments-error
               :operator ,operator
               :arguments ,args
               :expected '(or ,@(map #'car patterns)))))))

(defmacro ensure-top-level (operator &body body)
  "Signals an error condition of type
   `SPECIAL-OPERATOR-REFERENCE-ERROR' if the declaration currently
   being processed is not at top-level."

  `(progn
     (unless (top-level?)
       (error 'special-operator-reference-error :operator ,operator))

     ,@body))
