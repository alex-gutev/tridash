;;;; conditions.lisp
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

;;;; Error Conditions for semantic errors related to building the node
;;;; definitions out of the parsed declarations.

(in-package :tridash.frontend)


;;;; Base Error Condition Class

(defvar *declaration-stack* nil
  "List of declarations currently being processed. The tail of the
   list contains the top-level declaration being processed. The head
   contains the inner most declaration, of the top-level, declaration
   currently being processed.")

(define-condition semantic-error (error)
  ((declaration-stack
    :initform *declaration-stack*
    :reader declaration
    :documentation
    "The declaration stack leading up to the declaration where the
     error occurred."))

  (:documentation
   "Base condition class for semantic errors."))

(defmethod print-object ((err semantic-error) stream)
  (format stream "Semantic Error: ~a" (error-description err)))


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

(defmethod error-description ((err node-type-error))
  (with-accessors ((node node)) err
    (format nil "~a is a ~a, expected a ~a."
            (name node) (node-type node) (expected-type err))))


(define-condition non-existent-node (semantic-error)
  ((node :initarg :node
         :reader node
         :documentation
         "The name of the node being searched for.")

   (module-name :initarg :module-name
                :reader module-name
                :documentation
                "The name of the module in which the node was looked
                 up."))

  (:documentation
   "Error condition: the node NAME was not found in the module's node
    table."))

(defmethod error-description ((err non-existent-node))
  (format nil "No node ~a in module ~a." (node err) (module-name err)))


;;;; Name Collision Errors

(define-condition node-exists-error (semantic-error)
  ((node :initarg :node
         :reader node
         :documentation
         "Name of the node being added.")

   (node-table :initarg :node-table
               :reader node-table
               :documentation
               "Node-table in which the node was being added."))
  (:documentation
   "Error condition: Adding a node to a table which already contains a
    node with that name."))

(defmethod error-description ((err node-exists-error))
  (format nil "Module ~a already contains a node ~a."
          (name (node-table err)) (node err)))


(define-condition meta-node-name-collision (node-exists-error) ()
  (:documentation
   "Error condition: Meta-node name already names a node which is not
    a meta-node."))

(defmethod error-description ((err meta-node-name-collision))
  (with-accessors ((node node) (node-table node-table)) err
    (format nil "Cannot create meta-node ~a in module ~a, as ~0@* ~a already names a node which is not a meta-node."
            node (name node-table))))


;;;; Module Errors

(define-condition non-existent-module (semantic-error)
  ((module-name :initarg :module-name
                :reader module-name
                :documentation "Name of the module."))

  (:documentation
   "Error condition: Referencing a non-existent module."))

(defmethod error-description ((err non-existent-module))
  (format nil "No module named ~a." (module-name err)))


(define-condition alias-clash-error (node-exists-error)
  ((module-name :initarg :module
                :reader module-name
                :documentation
                "The name of the module for which an alias is being
                 added."))

  (:documentation
   "Error condition: module alias already names a node in the node-table."))

(defmethod error-description ((err alias-clash-error))
  (format nil "Cannot add alias ~a for module ~a as it already names a node in module ~a."
          (node err) (module-name err) (name (node-table err))))


(define-condition alias-taken-error (alias-clash-error) ()
  (:documentation
   "Error condition: alias is already an alias for another module."))

(defmethod error-description ((err alias-taken-error))
  (format nil "Cannot add alias ~a for module ~a as it is already an alias for another module, in module ~a."
          (node err) (module-name err) (name (node-table err))))


(define-condition import-node-error (node-exists-error)
  ((module-name :initarg :module
                :reader module-name
                :documentation
                "Name of the module from which the node is being imported."))

  (:documentation
   "Error condition: A node is being imported in a module, which
    already contains a different node with the same name."))

(defmethod error-description ((e import-node-error))
  (format nil "Importing node ~a from module ~a into module ~a. There is already a node with the same name."
          (name e) (module-name e) (name (node-table e))))


;;;; Special Node/Operator Errors

(define-condition self-reference-error (semantic-error) ()
  (:documentation
   "Error condition: Referencing the special self node outside a
    meta-node definition."))

(defmethod error-description ((err self-reference-error))
  "'self' node referenced outside a meta-node definition.")


(define-condition invalid-arguments-error (semantic-error)
  ((operator :initarg :operator
             :reader operator
             :documentation
             "The operator.")
   (arguments :initarg :arguments
              :reader arguments
              :documentation
              "The arguments passed.")
   (expected :initarg :expected
             :reader expected
             :documentation
             "The expected arguments."))

  (:documentation
   "Error condition: Invalid arguments passed to a special operator."))

(defmethod error-description ((e invalid-arguments-error))
  (format nil "Invalid arguments ~a to ~a. Expected arguments of the form ~a."
          (arguments e) (operator e) (expected e)))


(define-condition invalid-value-error (semantic-error)
  ((thing :initarg :thing
          :reader thing
          :documentation
          "The 'thing' for which an invalid value was specified.")

   (allowed :initarg :allowed
            :reader allowed
            :initform nil
            :documentation
            "List of allowed values.")

   (value :initarg :value
          :reader value
          :documentation
          "The actual value.")))

(defmethod error-description ((e invalid-value-error))
  (format nil "Invalid value ~a for ~a. Must be one of: ~{~a~#[~; or ~:;, ~]~}."
          (value e) (thing e) (allowed e)))


(define-condition global-outer-reference-error (semantic-error) ()
  (:documentation
   "Error condition: Outer node reference operator used at
    top-level."))

(defmethod error-description ((e global-outer-reference-error))
  "Cannot reference outer nodes from global scope.")


(define-condition out-node-error (semantic-error) ()
  (:documentation
   "Error condition: out operator outside a meta-node definition."))

(defmethod error-description ((e out-node-error))
  "Cannot use OUT operator outside a meta-node definition.")


;;;; Node Binding Errors

(define-condition target-node-error (semantic-error)
  ((node :initarg :node
         :reader node
         :documentation
         "The node which appeared as the target."))

  (:documentation
   "Condition for when a node is the target node of a binding, however
    it cannot appear as the target."))

(defmethod error-description ((err target-node-error))
  (format nil "~a cannot appear as the target of a binding." (node err)))


(define-condition ambiguous-context-error (semantic-error)
  ((node :initarg :node
         :reader node
         :documentation
         "The node with ambiguous contexts"))

  (:documentation
   "Error condition for when a single path activates multiple contexts
    of a single node."))

(defmethod error-description ((err ambiguous-context-error))
  (format nil "Node ~a has two contexts activated by a single common ancestor."
          (name (node err))))
