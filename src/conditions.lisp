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
  (format stream "~a is not a node." (node e)))


(define-condition module-node-reference-error (semantic-error)
  ((node :initarg :node
         :reader node
         :documentation "The referenced node declaration"))

  (:documentation
   "Error condition: The value of a module pseudo-node was referenced."))

(defmethod print-object ((e module-node-reference-error) stream)
  (format stream "Cannot reference the value of ~a." (node e)))


(define-condition non-node-operator-error (semantic-error)
  ((operator :initarg :operator
             :reader operator
             :documentation "The operator of the functor expression."))

  (:documentation
   "Error condition: The operator of a functor is not a node."))

(defmethod print-object ((e non-node-operator-error) stream)
  (format stream "~a cannot appear as an operator as it is not a node." (operator e)))


(define-condition non-existent-node-error (semantic-error)
  ((node :initarg :node
         :reader node
         :documentation
         "The name of the node being searched for.")

   (module :initarg :module
           :reader module
           :documentation
           "The module in which the node was looked up."))

  (:documentation
   "Error condition: the node NAME was not found in the module's node
    table."))

(defmethod print-object ((err non-existent-node-error) stream)
  (format stream "No node `~a` in module `~a`."
          (node err) (name (module err))))


;;;; Name Collision Errors

(define-condition node-exists-error (semantic-error)
  ((node-name :initarg :node-name
              :reader node-name
              :documentation
              "Name of the node being added.")

   (module :initarg :module
           :reader module
           :documentation
           "module in which the node was being added."))

  (:documentation
   "Error condition: Adding a node to a table which already contains a
    node with that name."))

(defmethod print-object ((err node-exists-error) stream)
  (format stream "Module `~a` already contains a node `~a`."
          (name (module err)) (node-name err)))


(define-condition redefine-special-operator-error (semantic-error)
  ((name :initarg :name
         :reader name
         :documentation
         "The special operator name."))

  (:documentation
   "Error condition: Attempt to define a node with the same name as a
    special operator."))

(defmethod print-object ((e redefine-special-operator-error) stream)
  (format stream "Identifier `~a` is reserved for special operators." (name e)))


;;;; Module Errors

(define-condition non-existent-module-error (semantic-error)
  ((module-name :initarg :module-name
                :reader module-name
                :documentation "Name of the module."))

  (:documentation
   "Error condition: Referencing a non-existent module."))

(defmethod print-object ((err non-existent-module-error) stream)
  (format stream "No module named `~a`." (module-name err)))


(define-condition create-alias-error (node-exists-error)
  ((module-name :initarg :module-name
                :reader module-name
                :documentation
                "The name of the module for which an alias is being
                 added."))

  (:documentation
   "Error condition: module alias already names a node in the module."))

(defmethod print-object ((err create-alias-error) stream)
  (format stream "Cannot create pseudo-node `~a` for module `~a` as it already names a node in module `~a`."
          (node-name err) (module-name err) (name (module err))))


(define-condition import-node-error (node-exists-error)
  ((source :initarg :source
           :reader source
           :documentation
           "The module from which the node is being imported."))

  (:documentation
   "Error condition: A node is being imported in a module, which
    already contains a different node with the same name."))

(defmethod print-object ((e import-node-error) stream)
  (format stream "Cannot import node `~a` from module `~a` into module `~a` as there is already a node with the same name."
          (node-name e) (name (source e)) (name (module e))))


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


(define-condition special-operator-reference-error (semantic-error)
  ((operator :initarg :operator
             :reader operator
             :documentation
             "The special operator symbol."))

  (:documentation
   "Error condition: A special operator, which can only appear at
    top-level, appeared as an operand."))

(defmethod print-object ((e special-operator-reference-error) stream)
  (format stream "`~a` declarations may only appear at top-level." (operator e)))


;;;; Node Binding Errors

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
  (format stream "~a has multiple contexts activated by a single common ancestor."
          (node err)))


(define-condition ambiguous-meta-node-context-error (ambiguous-context-error) ()
  (:documentation
   "Error condition: Meta-node has more than a single context and thus
    the value function of the meta-node is ambiguous."))

(defmethod print-object ((e ambiguous-meta-node-context-error) stream)
  (format stream "The value function of ~a is ambiguous as it has multiple contexts."
          (node e)))


(define-condition node-cycle-error (semantic-error)
  ((node :initarg :node
         :reader node
         :documentation "The node at which the cycle was detected."))

  (:documentation
   "Error condition: A cycle in the graph was detected."))

(defmethod print-object ((e node-cycle-error) stream)
  (format stream "Cycle detected in ~a." (node e)))


(define-condition dependency-not-reachable-error (semantic-error)
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

(defmethod print-object ((e dependency-not-reachable-error) stream)
  (format stream "Dependency ~a of ~a is not reachable from any input node."
          (dependency e) (node e)))
