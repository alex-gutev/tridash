;;;; node.lisp
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

;;;; Base `node' class definition.

(in-package :tridash.frontend)


(defclass node ()
  ((name
    :initarg :name
    :accessor name
    :documentation "The node's identifier name.")

   (dependencies
    :initform (make-hash-table :test #'eq)
    :initarg :dependencies
    :accessor dependencies
    :documentation
    "Hash table of the nodes on which the node's value depends, where
     each key is a `node' object and each value is the corresponding
     `node-link' object.")

   (observers
    :initform (make-hash-table :test #'eq)
    :initarg :observers
    :accessor observers
    :documentation
    "Hash table of the nodes which are dependent on the node's value,
     where each key is a `node' object and each value is the
     `node-link' object of this node in the observer node's
     DEPENDENCIES hash-table..")

   (contexts
    :accessor contexts
    :initform (make-hash-table :test #'equal)
    :documentation
    "Set of contexts of the node. Each key is a context
     identifier (either a dependency node or unique identifier) and
     the corresponding value is the `node-context' object.")

   (attributes
    :accessor attributes
    :initform (make-hash-table :test #'eq)
    :documentation
    "Set of miscellaneous attributes (key-value pairs) where each
     attribute is identified by a unique symbol."))

  (:documentation
   "Base node class. Stores the binding information about a node,
    i.e. the node's dependencies and observers"))

(defclass node-context ()
  ((operands
    :initarg :operands
    :initform (make-hash-table :test #'eq)
    :accessor operands
    :documentation
    "Set of dependency nodes which are operands to the context's value
     function.")

   (value-function
    :initarg :value-function
    :initform nil
    :accessor value-function
    :documentation
    "The context's value function."))

  (:documentation
   "Node context. A node context stores information about the node's
    value is computed at a particular moment (context)."))


;;;; Predicates

(defun node? (x)
  "Returns true if X is a `node'."

  (typep x 'node))

(defun value? (x)
  "Returns true if X is a literal value."

  (or (numberp x) (stringp x)))

(defun input-node? (node)
  "Returns true if NODE is an input node."

  (gethash :input (attributes node)))


;;;; Bindings

(defstruct (node-link (:constructor node-link (node &key context)))
  "NODE-LINK objects are used to refer to nodes indirectly. This
   allows nodes to be replaced with other nodes and have all
   references to the node be automatically updated.

   NODE is the dependency NODE.

   CONTEXT is the context identifier of which NODE is an operand."

  node
  context)


;;; Adding Bindings

(defmacro! ensure-binding ((source target &rest options) (link-var) &body body)
  "Establishes a binding from SOURCE to TARGET if it does not already
   exist. OPTIONS are additional optional arguments passed to
   ADD-BINDING. If the binding had not been previously established,
   the forms in BODY are evaluated, with the value of the last form
   being the value of the ENSURE-BINDING form."

  `(multiple-value-bind (,link-var ,g!existed?) (add-binding ,source ,target ,@options)
     (unless ,g!existed?
       ,@body)))

(defun add-binding (source target &key (context (and (node? source) source)) (add-function t))
  "Establishes a binding from the SOURCE node to the TARGET
   node. CONTEXT is the context identifier, of which SOURCE is an
   operand, defaults to SOURCE itself. If ADD-FUNCTION is true, the
   value function of the context is set to the SOURCE node
   link. Returns the `node-link' object of the established binding, as
   the first value, and true, as the second value, if the binding had
   previously been established."

  (if (node? source)

      (multiple-value-return (link in-hash?)
          (add-dependency source target
                          :context context
                          :add-function add-function)

        (unless in-hash?
          (add-observer target source link)))

      (prog1 source
        (when add-function
          (setf (value-function (context target context)) source)))))


(defun add-dependency (dependency node &key context add-function)
  "Adds DEPENDENCY as a dependency node of NODE. CONTEXT is the
   context identifier, of which, DEPENDENCY is an operand. If
   ADD-FUNCTION is true, the value function of the context is set to
   the node link of DEPENDENCY. Returns the `node-link' object of the
   dependency, as the first return value, and true, as the second
   value, if DEPENDENCY was already in the dependency set of NODE."

  (with-slots (dependencies) node
    (multiple-value-return (link in-hash?)
        (ensure-gethash dependency dependencies (node-link dependency :context context))

      (when (not in-hash?)
        (with-slots (operands value-function) (context node context)
          (setf (gethash dependency operands) link)
          (when add-function (setf value-function link)))))))

(defun add-observer (observer node link)
  "Adds OBSERVER as an observer node of NODE. LINK is the `node-link'
   object corresponding to the dependency NODE within the DEPENDENCIES
   hash-table of OBSERVER."

  (setf (gethash observer (observers node)) link))


;;; Removing Bindings

(defun remove-observer (node observer)
  "Removes OBSERVER from the observer set of NODE."

  (remove-dependency observer node))

(defun remove-dependency (node dependency)
  "Removes DEPENDENCY from the dependency set of NODE."

  (with-slots (dependencies) node
    (when (aand (gethash dependency dependencies)
                (remove-operand node it))

      (remhash dependency dependencies)
      (remhash node (observers dependency)))))


;;;; Contexts

(defun context (node context-id)
  "Returns the context of NODE with identifier CONTEXT-ID. If node
   does have such a context, a new context is created."

  (ensure-gethash context-id (contexts node) (make-instance 'node-context)))

(defun (setf context) (value node context-id)
  "Sets the context, of NODE, with identifier CONTEXT-ID to VALUE."

  (setf (gethash context-id (contexts node)) value))

(defmacro! create-context ((o!node o!context-id) &body forms)
  "Creates a context, of NODE, with identifier CONTEXT-ID. If NODE
   does not already contain a context with that identifier the forms
   in BODY are evaluated with the value of the last form being the
   value of the macro form.

   The environment, in which FORMS are evaluated, contains a BIND
   function which binds the dependency node (passed as the first
   argument) to NODE. The function takes a single keyword argument
   ADD-FUNCTION which corresponds to the ADD-FUNCTION argument of
   ADD-BINDING. The environment also contains a symbol macro
   VALUE-FUNCTION which is bound to the value function of the
   context."

  `(multiple-value-bind (,g!context ,g!in-hash?) (context ,g!node ,g!context-id)
     (unless ,g!in-hash?
       (flet ((bind (,g!dependency &key (add-function nil))
                (add-binding ,g!dependency ,g!node :context ,g!context-id :add-function add-function)))
         (with-slots (value-function) ,g!context
           ,@forms)))))


(defun remove-operand (node operand)
  "Removes the node OPERAND from the operands of CONTEXT. Returns true
   if the operand was removed, nil if the operand cannot be removed."

  (with-accessors ((operand node-link-node) (context-id node-link-context)) operand
    (with-slots (operands value-function) (context node context-id)
      (when (= (hash-table-count operands) 1)
        (remhash operand operands)
        (setf value-function nil)

        (remhash context-id (contexts node))

        t))))

(defun remove-context (node context-id)
  "Removes the context with identifier CONTEXT-ID if it is redundant,
   that is it has no operands and no value function."

  (with-slots (operands value-function) (context node context-id)
    (and (zerop (hash-table-count operands))
         (null value-function)
         (remhash context-id (contexts node)))))


;;;; Observers/Dependencies Utility Functions

(defun observer-list (node)
  "Returns a list of all observers of NODE."

  (hash-table-keys (observers node)))

(defun dependency-list (node)
  "Returns a list of all dependencies of NODE."

  (hash-table-keys (dependencies node)))


(defun observers-count (node)
  "Returns the number of observers of NODE."

  (hash-table-count (observers node)))

(defun dependencies-count (node)
  "Returns the number of dependencies of NODE."

  (hash-table-count (dependencies node)))


;;; Print Method

(defmethod print-object ((node node) stream)
  "Prints a representation of the NODE object which includes the
   node's name."

  (print-unreadable-object (node stream :type t)
    (format stream "~a" (name node))))
