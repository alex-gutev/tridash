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
    :reader declaration-stack
    :documentation
    "The declaration stack leading up to the declaration where the
     error occurred.")

   (module-table
    :initform *global-module-table*
    :reader module-table
    :documentation
    "The global module table."))

  (:documentation
   "Base condition class for semantic errors."))

(defmethod print-object :around ((err semantic-error) stream)
  (format stream "Semantic Error: ")
  (call-next-method))


;;;; Node Reference Errors

(define-condition not-node-error (semantic-error)
  ((node :initarg :node
         :reader node
         :documentation
         "The node declaration."))

  (:documentation
   "Error condition: A node was expected however the object provided
    is not a node."))

(defmethod print-object ((e not-node-error) stream)
  (format stream "`~a` is not a node." (node e)))


(define-condition module-node-reference-error (semantic-error)
  ((node :initarg :node
         :reader node
         :documentation "The referenced node declaration"))

  (:documentation
   "Error condition: The value of a module pseudo-node was referenced."))

(defmethod print-object ((e module-node-reference-error) stream)
  (format stream "Cannot reference the value of module pseudo-node `~a`." (node e)))


(define-condition non-node-operator-error (semantic-error)
  ((operator :initarg :operator
             :reader operator
             :documentation "The operator of the functor expression."))

  (:documentation
   "Error condition: The operator of a functor is not a node."))

(defmethod print-object ((e non-node-operator-error) stream)
  (format stream "`~a` cannot appear as an operator as it is not a node." (operator e)))


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

(defmethod print-object ((err non-existent-node) stream)
  (format stream "No node ~a in module ~a." (node err) (module-name err)))


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

(defmethod print-object ((err node-exists-error) stream)
  (format stream "Module ~a already contains a node ~a."
          (name (node-table err)) (node err)))


(define-condition meta-node-name-collision (node-exists-error) ()
  (:documentation
   "Error condition: Meta-node name already names a node which is not
    a meta-node."))

(defmethod print-object ((err meta-node-name-collision) stream)
  (with-accessors ((node node) (node-table node-table)) err
    (format stream "Cannot create meta-node ~a in module ~a, as ~0@* ~a already names a node which is not a meta-node."
            node (name node-table))))


(define-condition special-operator-name-error (meta-node-name-collision) ()
  (:documentation
   "Error condition: Meta-node names a special operator."))

(defmethod print-object ((e special-operator-name-error) stream)
  (format stream "Cannot redefine special operator ~a." (node e)))


;;;; Module Errors

(define-condition non-existent-module (semantic-error)
  ((module-name :initarg :module-name
                :reader module-name
                :documentation "Name of the module."))

  (:documentation
   "Error condition: Referencing a non-existent module."))

(defmethod print-object ((err non-existent-module) stream)
  (format stream "No module named ~a." (module-name err)))


(define-condition alias-clash-error (node-exists-error)
  ((module-name :initarg :module
                :reader module-name
                :documentation
                "The name of the module for which an alias is being
                 added."))

  (:documentation
   "Error condition: module alias already names a node in the node-table."))

(defmethod print-object ((err alias-clash-error) stream)
  (format stream "Cannot add alias ~a for module ~a as it already names a node in module ~a."
          (node err) (module-name err) (name (node-table err))))


(define-condition alias-taken-error (alias-clash-error) ()
  (:documentation
   "Error condition: alias is already an alias for another module."))

(defmethod print-object ((err alias-taken-error) stream)
  (format stream "Cannot add alias ~a for module ~a as it is already an alias for another module, in module ~a."
          (node err) (module-name err) (name (node-table err))))


(define-condition import-node-error (node-exists-error)
  ((module-name :initarg :module
                :reader module-name
                :documentation
                "Name of the module from which the node is being imported."))

  (:documentation
   "Error condition: A node is being imported in a module, which
    already contains a different node with the same name."))

(defmethod print-object ((e import-node-error) stream)
  (format stream "Importing node ~a from module ~a into module ~a. There is already a node with the same name."
          (name e) (module-name e) (name (node-table e))))


;;;; Special Node/Operator Errors

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

(defmethod print-object ((e invalid-arguments-error) stream)
  (format stream "Invalid arguments ~a to ~a. Expected arguments of the form ~a."
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

(defmethod print-object ((e invalid-value-error) stream)
  (format stream "Invalid value ~a for ~a. Must be one of: ~{~a~#[~; or ~:;, ~]~}."
          (value e) (thing e) (allowed e)))


(define-condition global-outer-reference-error (semantic-error) ()
  (:documentation
   "Error condition: Outer node reference operator used at
    top-level."))

(defmethod print-object ((e global-outer-reference-error) stream)
  (format stream "Cannot reference outer nodes from global scope."))


(define-condition special-operator-operand (semantic-error)
  ((operator :initarg :operator
             :reader operator
             :documentation
             "The special operator symbol."))

  (:documentation
   "Error condition: A special operator, which can only appear at
    top-level, appeared as an operand."))

(defmethod print-object ((e special-operator-operand) stream)
  (format stream "~a declarations may only appear at top-level." (operator e)))


;;;; Node Binding Errors

(define-condition meta-node-target-error (semantic-error)
  ((meta-node :initarg :meta-node
              :reader meta-node
              :documentation
              "The meta-node which appears as the target."))

  (:documentation
   "Error condition: A meta-node appears as the target of a binding."))

(defmethod print-object ((e meta-node-target-error) stream)
  (format stream "Meta-node `~a` may not appear as the target of a binding."
          (meta-node e)))

(define-condition module-node-target-error (semantic-error)
  ((node :initarg :node
         :reader node
         :documentation
         "The module node which appears as the target of a binding"))

  (:documentation
   "Error condition: Module pseudo-node appears as the target of a binding."))

(defmethod print-object ((e module-node-target-error) stream)
  (format stream "Module pseudo-node `~a` cannot appear as the target of a binding." (node e)))


(define-condition target-node-error (semantic-error)
  ((node :initarg :node
         :reader node
         :documentation
         "The node which appeared as the target."))

  (:documentation
   "Condition for when a node is the target node of a binding, however
    it cannot appear as the target."))

(defmethod print-object ((err target-node-error) stream)
  (format stream "~a cannot appear as the target of a binding." (node err)))


;;;; Graph Structure Errors

(define-condition ambiguous-context-error (semantic-error)
  ((node :initarg :node
         :reader node
         :documentation
         "The node with ambiguous contexts"))

  (:documentation
   "Error condition for when a single path activates multiple contexts
    of a single node."))

(defmethod print-object ((err ambiguous-context-error) stream)
  (format stream "Node ~a has multiple contexts activated by a single common ancestor."
          (name (node err))))


(define-condition ambiguous-meta-node-context (ambiguous-context-error) ()
  (:documentation
   "Error condition: Meta-node has more than a single context and thus
    the value function of the meta-node is ambiguous."))

(defmethod print-object ((e ambiguous-meta-node-context) stream)
  (format stream "The value function of meta-node ~a is ambiguous as it has multiple contexts."
          (name (node e))))


(define-condition node-cycle-error (semantic-error)
  ((node :initarg :node
         :reader node
         :documentation "The node at which the cycle was detected."))

  (:documentation
   "Error condition: A cycle in the graph was detected."))

(defmethod print-object ((e node-cycle-error) stream)
  (format stream "Cycle detected in node ~a" (name (node e))))


(define-condition dependency-not-reachable (semantic-error)
  ((node :initarg :node
         :reader node
         :documentation
         "The node with an unreachable dependency.")

   (dependency :initarg :dependency
               :reader dependency
               :documentation
               "The dependency of the node."))

  (:documentation
   "Error condition: A dependency of a node is not reachable from any
    input node."))

(defmethod print-object ((e dependency-not-reachable) stream)
  (format stream "Dependency ~a of ~a is not reachable from any input node."
          (name (dependency e)) (name (node e))))
