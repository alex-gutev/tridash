;;;; module-table.lisp
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

;;;; Functions for creating and switching modules

(in-package :tridash.frontend)

(defclass module-table ()
  ((modules
    :accessor modules
    :initarg :modules
    :initform (make-hash-map)
    :documentation
    "Module Map. Each key is a module identifier symbol and the
     corresponding value is the `MODULE'.")

   (current-module
    :accessor current-module
    :initarg :current-module
    :initform (make-instance 'module :name :init)
    :documentation
    "The current `MODULE', into which nodes are added.")

   (attribute-processors
    :accessor attribute-processors
    :initarg :attribute-processors
    :initform (make-hash-map :test #'cl:equalp)
    :documentation
    "Map of attribute processors. Each key is the attribute's string
     identifier and the corresponding value is a meta-node which
     should be invoked to process the attribute."))

  (:documentation
   "Module table object mapping module identifiers to `MODULE'
    objects."))


(defmethod initialize-instance :after ((module-table module-table) &key &allow-other-keys)
  "Adds a mapping for the `MODULE' initially bound to the
   `CURRENT-MODULE' slot of MODULE-TABLE with identifier
   :INIT. Creates the builtin module."

  (with-slots (modules current-module) module-table
    (-> (create-builtin-module module-table)
        (import-all-nodes current-module))

    (setf (get :init modules) current-module)))

(defun change-module (module &optional (modules *global-module-table*))
  "Changes the CURRENT-MODULE of MODULES (defaults to
   *GLOBAL-MODULE-TABLE*) to MODULE."

  (setf (current-module modules)
        (ensure-module module modules)))

(defun ensure-module (module &optional (modules *global-module-table*))
  "Ensures that MODULE names a module and returns its `MODULE'
   object. If MODULE is not present in the MODULES a new empty
   `MODULE' is created."

  (ensure-get module (modules modules) (make-instance 'module :name module)))

(defun get-module (module &optional (modules *global-module-table*))
  "Returns the `MODULE' with identifier MODULE in MODULES. If there is
   no module with identifier MODULE in MODULES, signals a
   `NON-EXISTENT-MODULE-ERROR' condition."

  (with-retry-restart
    (or (get module (modules modules))
        (error 'non-existent-module-error :module-name module))))


;;;; Builtin Module Definition

;;; The 'builtin' module contains a few macro nodes such as the "case"
;;; meta-node. Eventually a macro-system will be developed and these
;;; will be specified in Tridash code.

(defparameter *core-meta-nodes*
  (external-meta-nodes
   `((if (cond then (,+optional-argument+ else)) ((:strictness (or cond (and then else)))))
     (:member (object key) ((:strictness (or object key))))
     (:fail ())
     (:catch (try catch) ((:strictness try)))))

  "Map of meta-nodes which comprise the language primitives.")

(defconstant +core-macro-nodes+
  `((-> ,+bind-operator+ 10 :right)
    (\: ,+def-operator+ 5 :right)
    (|.| ,+subnode-operator+ 1000 :left)
    (& ,+ref-operator+)
    (|..| ,+outer-operator+)
    (prog ,+list-operator+)

    (attribute-processor ,+attribute-processor-operator+))

  "List of core macro nodes where each a element is a list with the
   first item being the node's identifier and the second item being
   the operator to which the macro node expands to.")


(defun create-builtin-module (modules)
  "Creates the builtin module."

  (let ((builtin (ensure-module (id-symbol "builtin") modules)))
    (let ((case-node (add-external-meta-node (id-symbol "case") builtin))
          (?->-node (add-external-meta-node (id-symbol "?->") builtin)))

      (add-core-nodes builtin)
      (add-core-macros builtin)

      (setf (node-macro-function case-node) #'case-macro-function)
      (export-node (id-symbol "case") builtin)

      (setf (node-macro-function ?->-node) #'?->-macro-function)
      (export-node (id-symbol "?->") builtin)

      builtin)))

(defun add-core-nodes (builtin)
  "Adds the nodes in *CORE-META-NODES* to the module BUILTIN, and its
   export list."

  (doseq (node (map-values *core-meta-nodes*))
    (add-meta-node (name node) node builtin)
    (export-node (name node) builtin)))

(defun add-core-macros (builtin)
  "Adds the core macro nodes in +CORE-MACRO-NODES+ to the module
   BUILTIN."

  (doseq ((node operator &rest op-info) +core-macro-nodes+)
    (let ((meta-node  (macro-node node nil operator)))
      (with-slots (name) meta-node
        (add-meta-node name meta-node builtin)
        (export-node name builtin)

        (when op-info
          (add-core-operator name op-info builtin))))))

(defun add-core-operator (name op-info builtin)
  "Adds the identifier NAME to the operator table of module
   BUILTIN. OP-INFO is a list of the operator precedence and
   associativity."

  (destructuring-bind (precedence &optional (assoc :left)) op-info
    (add-operator name precedence assoc (operator-nodes builtin))))


(defun case-macro-function (operator operands module)
  "Case macro function. Transforms the case expression into a series
   of nested if expressions."

  (declare (ignore operator))

  (let ((if-node (list +in-module-operator+ (id-symbol "core") (id-symbol "if"))))
    (flet ((make-if (case expr)
             (match case
               ((list (eq (id-symbol ":")) cond node)
                (list if-node cond node expr))

               (_ case))))

      (process-declaration
       (reduce #'make-if operands :from-end t :initial-value nil)
       module))))

(defun ?->-macro-function (operator operands module)
  "Macro function of the ?-> operator."

  (match operands
    ((list src target)

     (process-declaration
      `(,+bind-operator+
        ((,+in-module-operator+ ,(id-symbol "core") ,(id-symbol "not"))
         (,+subnode-operator+ ,src ,(id-symbol "fail")))

        (,+bind-operator+
         (,+subnode-operator+ ,src ,(id-symbol "value"))
         ,target))
      module

      :top-level t))

    (_ (error 'invalid-arguments-error
              :expected '(node node)
              :operator operator
              :arguments operands))))


;;;; Creating Core Expressions

(defun if-expression (test then else)
  "Creates an if functor expression with test TEST, then expression
   THEN, and else expression ELSE."

  (functor-expression (get 'if *core-meta-nodes*)
                      (list test then else)))

(defun member-expression (object key)
  (functor-expression (get :member *core-meta-nodes*)
                      (list object key)))

(defun fail-expression ()
  (functor-expression (get :fail *core-meta-nodes*) nil))

(defun catch-expression (try catch)
  (functor-expression (get :catch *core-meta-nodes*)
                      (list try catch)))
