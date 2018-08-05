;;;; node.lisp
;;;;
;;;; Metalink Programming Language.
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

(in-package :metalink.frontend)


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

   (wait-set
    :initform (make-hash-table :test #'eq)
    :initarg :wait-set
    :accessor wait-set
    :documentation
    "Set of nodes which the node is responsible for informing of a
     value change. The nodes are stored as values where the keys are
     their common observer.")

   (value-function
    :accessor value-function
    :initform nil
    :documentation
    "The function which computes the node's value. Dependency nodes
     are referenced using the `node-link' object created when the node
     was added as a dependency.")))


;;; Predicates

(defun node? (x)
  "Returns true if X is a `node'."
  
  (typep x 'node))

(defun value? (x)
  "Returns true if X is a literal value."

  (numberp x))

(defun n-ary-node? (node)
  "Returns true if NODE has more than one dependency node."
  
  (> (dependencies-count node) 1))


;;; Bindings

(defstruct (node-link (:constructor node-link (node)))
  "NODE-LINK objects are used to refer to nodes indirectly. This
   allows nodes to be replaced with other nodes and have all
   references to the node be automatically updated."

  node)

(defun add-binding (source target &optional (add-condition t))
  "Establishes a binding from the SOURCE node to the TARGET
   node. Returns the `node-link' object of the established binding."

  (if (node? source)
      (aprog1
          (add-dependency source target add-condition)
        (add-observer target source it))
      (prog1 source
        (when add-condition
          (add-default-condition target source)))))


(defun add-dependency (dependency node &optional add-condition)
  "Adds DEPENDENCY as a dependency node of NODE, returns the
   `node-link' object of the dependency. if ADD-CONDITION is true
   DEPENDENCY is added as the default conditional binding."

  (with-slots (dependencies) node
    (multiple-value-bind (link in-hash?)
        (ensure-gethash dependency dependencies (node-link dependency))

      (when (and add-condition (not in-hash?))
        (add-default-condition node link))
      
      link)))

(defun add-observer (observer node link)
  "Adds OBSERVER as an observer node of NODE. LINK is the `node-link'
   object corresponding to the dependency NODE within the DEPENDENCIES
   hash-table of OBSERVER."

  (setf (gethash observer (observers node)) link))


;;; Condition nodes

(defun add-default-condition (node link)
  "Adds a dependency node, with the `node-link' object LINK, as the
   default conditional binding of NODE. NODE takes on the value of the
   default conditional dependency if all other conditions evaluate to
   false."
  
  (appendf (value-function node) (list link)))

(defun add-condition (node cond-link value-link)
  "Adds a conditional binding. COND-LINK is the `node-link' object
   corresponding to the predicate dependency node and VALUE-LINK is
   the `node-link' object corresponding to the value node."
  
  (with-slots (value-function) node
    (aif (member value-link value-function)
         (setf (car it) (list 'if cond-link value-link))
         (appendf value-function (list (list 'if cond-link value-link))))))

(defun create-value-function (node)
  "Converts the list of conditions (stored in the VALUE-FUNCTION slot
   of NODE), which are of the form (IF COND VALUE), into a value
   function with a single IF block."

  (labels ((create-conditions (fn)
             (match (first fn)
               ((list 'if pred value)
                `(if ,pred ,value
                     ,(create-conditions (rest fn))))
               (nil 'self)
               (value value))))
    (with-slots (value-function) node
      (when value-function
        (setf value-function
              (create-conditions value-function))))))


;;; Observers/Dependencies Utility Functions

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
