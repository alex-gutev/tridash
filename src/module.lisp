;;;; module.lisp
;;;;
;;;; Tridash Programming Language.
;;;; Copyright (C) 2018-2019  Alexander Gutev
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

;;;; `MODULE' class and associated functions.

(in-package :tridash.frontend)

(defclass module ()
  ((name
    :accessor name
    :initarg :name
    :initform nil
    :documentation
    "The name of the module.")

   (outer-module
    :accessor outer-module
    :initarg :outer-module
    :initform nil
    :documentation
    "The module of the scope in which this module is contained.")

   (depth
    :accessor depth
    :initarg :depth
    :initform 0
    :documentation
    "The depth of the current module. Global modules are at depth 0.")

   (nodes
    :accessor nodes
    :initform (make-hash-map)
    :documentation
    "Map mapping node identifiers to node objects. This map includes
     all nodes, meta-nodes and pseudo nodes in the module.")

   (meta-nodes
    :accessor meta-nodes
    :initform (make-hash-set)
    :documentation
    "Set containing the `META-NODE's in the module.")

   (public-nodes
    :accessor public-nodes
    :initarg :public-nodes
    :initform (make-hash-map)
    :documentation
    "Map of nodes which are exported from the module. When this module
     is imported into another module by import(<module name>), the
     nodes in this table are imported into the module.")

   (operator-nodes
    :accessor operator-nodes
    :initarg :operator-nodes
    :initform (copy +infix-operators+)
    :documentation
    "The module's operator table. Each key is the identifier of a node
     that can appear in infix position, and the corresponding value is
     a list containing the operator's precedence and associativity.")

   (input-nodes
    :accessor input-nodes
    :initform (make-hash-set)
    :documentation
    "Set of the module's input nodes."))

  (:documentation
   "Module symbol table storing a mapping between node identifiers and
    node objects, and information related to the module."))


;;;; Constructor Functions

(defun make-inner-module (module)
  "Creates a new module that is contained in MODULE."

  (with-slots (name depth operator-nodes) module
    (make-instance 'module
                   :name name
                   :outer-module module
                   :depth (1+ depth)
                   :operator-nodes (copy operator-nodes))))


;;;; Predicates

(defun global-module? (module)
  (zerop (depth module)))


;;;; Node lookup functions

(defvar *create-nodes* t
  "Flag: If true ENSURE-NODE will create a new node rather than
   searching up the outer node tables.")

(defvar *search-module* nil
  "The name of the module in which the node is currently being looked up.")


(defun ensure-node (name module &optional (create *create-nodes*))
  "Searches for the node with identifier NAME in MODULE. If CREATE is
   true (defaults to the value of *CREATE-NODES*) and the node is not
   found in MODULE, it is created otherwise it is searched for in the
   OUTER-MODULE of MODULE."

  (flet ((create-node (c)
           (declare (ignore c))
           (when create
             (return-from ensure-node
               (add-node name (make-instance 'node :name name) module)))))

    (handler-bind
        ((non-existent-node #'create-node))
      (lookup-node name module (unless create (outer-module module))))))

(defun lookup-node (name module &optional (next-module (and module (outer-module module))))
  "Searches for the node with identifier NAME, in MODULE. If the node
   is not found in MODULE, MODULE's OUTER-MODULE is searched
   recursively. If the node is found it is returned in the first value
   with the module, in which it was found, in the second value. If no
   node with that identifier is found a `NON-EXISTENT-NODE' condition
   is signaled."

  (let ((*search-module* (or *search-module* (and module (name module)))))

   (unless module
     (error 'non-existent-node :node name :module-name *search-module*))

    (aif (get name (nodes module))
         (values it module)
         (lookup-node name next-module))))

(defun in-home-module? (node module)
  "Returns true if NODE was initially declared in MODULE, false if it
   was imported into it."

  (and (node? node) (eq (home-module node) module)))

(defun reference-operand (operand node context)
  "References the operand OPERAND from NODE in context CONTEXT. If
   OPERAND is a `META-NODE', adds NODE to the meta-node's instances
   and returns a `META-NODE-REF' expression."

  (typecase operand
    (meta-node
     (aprog1 (meta-node-ref operand)
       (add-to-instances node operand context it)))

    (module
     (error 'module-node-reference-error :node operand))

    (otherwise
     operand)))


;;;; Adding nodes

(defun add-node (name node module)
  "Adds the node NODE with name NAME to MODULE."

  (with-slots (nodes meta-nodes) module
    (when (memberp name nodes)
      (error 'node-exists-error :node name :module module))

    (unless (get :module (attributes node))
      (setf (get :module (attributes node)) module))

    (when (meta-node? node)
      (nadjoin node meta-nodes))

    (setf (get name nodes) node)))

(defun add-meta-node (name node module)
  "Adds the `META-NODE' NODE with name NAME to MODULE."

  (when (memberp name +special-operators+)
    (error 'special-operator-name-error :node name))

  (add-node name node module))

(defun add-external-meta-node (name module)
  "Adds a stub for an externally defined meta-node with name NAME to
   MODULE."

  (add-meta-node
   name
   (make-instance 'external-meta-node :name name)
   module))


;;; Removing nodes

(defun remove-node (name module)
  "Removes the node with name NAME from MODULE."

  (with-slots (nodes meta-nodes) module
    (awhen (get name nodes)
      (erase nodes name)
      (erase meta-nodes it))))


;;; Input Nodes

(defun add-input (node module)
  "Adds NODE to the input nodes of MODULE and sets its :INPUT attribute
   to T."

  (unless (input-node? node)
    (setf (attribute :input node) t)
    (context node :input)
    (nadjoin node (input-nodes module))))


;;; Importing and Exporting Nodes

(defun import-node (name src dest)
  "Import node NAME from SRC into DEST."

  (let* ((node (lookup-node name src)))
    (with-slots (nodes) dest
      (when (aand (get name nodes) (/= it node))
        (error 'import-node-error
               :node name
               :source src
               :module dest))

      (setf (get name nodes) node)

      (awhen (get name (operator-nodes src))
        (add-operator name (first it) (second it) (operator-nodes dest))))))

(defun export-node (name module)
  "Adds the node with name NAME to the PUBLIC-NODES of MODULE."

  (setf (get name (public-nodes module))
        (lookup-node name module)))
