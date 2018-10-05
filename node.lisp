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

   (value-functions
    :accessor value-functions
    :initform (make-hash-table :test #'eq)
    :documentation
    "Set of functions which compute the node's value. Stored as
     hash-table where each key is either a function identifier or is a
     dependency node and the corresponding value is the value
     function.")

   (attributes
    :accessor attributes
    :initform (make-hash-table :test #'eq)
    :documentation
    "Set of miscellaneous attributes (key-value pairs) where each
     attribute is identified by a unique symbol.")))


;;; Predicates

(defun node? (x)
  "Returns true if X is a `node'."

  (typep x 'node))

(defun value? (x)
  "Returns true if X is a literal value."

  (numberp x))

(defun n-ary-node? (node)
  "Returns true if NODE has more than one dependency node."

  (and
   (> (dependencies-count node) 1)
   (not (input-node? node))))


;;; Bindings

(defstruct (node-link (:constructor node-link (node &optional 2-way-p)))
  "NODE-LINK objects are used to refer to nodes indirectly. This
   allows nodes to be replaced with other nodes and have all
   references to the node be automatically updated."

  node
  2-way-p)

(defun add-binding (source target &optional (add-function t))
  "Establishes a binding from the SOURCE node to the TARGET
   node. Returns the `node-link' object of the established binding."

  (if (node? source)
      (aprog1
          (add-dependency source target add-function)
        (add-observer target source it)
        (mark-2-way source target it))
      (prog1 source
        (when add-function
          (setf (value-function target nil) source)))))

(defun mark-2-way (source target link)
  "If the node TARGET is a dependency of SOURCE, the `NODE-LINK' LINK
   and the link between SOURCE and TARGET are marked as 2-way
   links (the 2-WAY-P slots are set to true)."

  (awhen (gethash target (dependencies source))
    (setf (node-link-2-way-p it) t)
    (setf (node-link-2-way-p link) t)))


(defun add-dependency (dependency node &optional add-function)
  "Adds DEPENDENCY as a dependency node of NODE, returns the
   `node-link' object of the dependency. If ADD-FUNCTION is true, a
   value function, with key DEPENDENCY, which evaluates to the value
   of DEPENDENCY, is added."

  (with-slots (dependencies) node
    (multiple-value-bind (link in-hash?)
        (ensure-gethash dependency dependencies (node-link dependency))

      (when (and add-function (not in-hash?))
        (add-value-function node link))

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
               (nil (node-link 'self))
               (value value))))
    (with-slots (value-function) node
      (when value-function
        (setf value-function
              (create-conditions value-function))))))
;;; Value Functions

(defun value-function (node &optional fn)
  (gethash fn (value-functions node)))

(defun (setf value-function) (value node &optional fn)
  (setf (gethash fn (value-functions node)) value))

(defun add-value-function (node link)
  (setf (value-function node (node-link-node link)) link))


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
