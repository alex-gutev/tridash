;;;; tridash.asd
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

;;;; Frontend Error Conditions.

(in-package :tridash.frontend)


;;;; Base Error Condition Class

(define-condition semantic-error (error) ()
  (:documentation
   "Base condition class for semantic errors."))

(defmethod print-object ((err semantic-error) stream)
  (format stream "Semantic Error: ~a" (message err)))

(defgeneric message (condition)
  (:documentation
   "Returns an explanation of the error condition.")

  (:method ((condition t))
    "Other."))


;;;; Basic Node Lookup Errors

(define-condition node-type-error (semantic-error)
  ((node :initarg :node
         :reader node
         :documentation "The node.")

   (expected-type :initarg :expected
                  :reader expected-type
                  :documentation "The expected node type"))

  (:documentation
   "Incorrect node type condition. Signaled when the type of node
    found is different from the type of the node that was expected in
    the context."))

(defmethod message ((err node-type-error))
  (with-accessors ((node node)) err
    (format nil "~a is a ~a, expected a ~a."
            (name node) (node-type node) (expected-type err))))


(define-condition non-existent-node (semantic-error)
  ((node :initarg :node
         :reader node
         :documentation
         "The name of the node being searched for.")

   (node-table :initarg :node-table
               :reader node-table
               :documentation
               "The node-table of the module in which the node was
                searched for."))

  (:documentation
   "Error condition: the node NAME was not found in the module's node
    table."))

(defmethod message ((err non-existent-node))
  (format nil "No node: ~a in module: ~a." (node err) (node-table err)))


;;;; Module Errors

(define-condition non-existent-module (semantic-error)
  ((module-name :initarg :module-name
                :reader module-name
                :documentation "Name of the module."))

  (:documentation
   "Error condition: Referencing a non-existent module."))

(defmethod message ((err non-existent-module))
  (format nil "No module named ~a." (module-name err)))


(define-condition alias-clash-error (semantic-error)
  ((alias :initarg :alias
          :reader alias
          :documentation
          "The module alias.")

   (module-name :initarg :module
                :reader module-name
                :documentation
                "The name of the module."))

  (:documentation
   "Error condition: module alias already names a node in the node-table."))

(defmethod message ((err alias-clash-error))
  (format nil "Cannot add alias ~a for module ~a as it already names a node."
          (alias err) (module-name err)))


(define-condition alias-taken-error (alias-clash-error) ()
  (:documentation
   "Error condition: alias is already an alias for another module."))

(defmethod message ((err alias-taken-error))
  (format nil "Cannot add alias ~a for module ~a as it is already an alias for another module."
          (alias err) (module-name err)))


;;;; Node Context Errors

(define-condition target-node-error (semantic-error)
  ((node :initarg :node
         :reader node
         :documentation
         "The node which appeared as the target."))

  (:documentation
   "Condition for when a node is the target node of a binding, however
    it cannot appear as the target."))

(defmethod message ((err target-node-error))
  (format nil "~a cannot appear as the target of a binding." (node err)))


(define-condition ambiguous-context-error (semantic-error)
  ((node :initarg :node
         :reader node
         :documentation
         "The node with ambiguous context's"))

  (:documentation
   "Error condition for when a single path activates multiple contexts
    of a single node."))

(defmethod message ((err ambiguous-context-error))
  (format nil "Node ~a has two contexts activated by a single common ancestor."
          (name (node err))))
