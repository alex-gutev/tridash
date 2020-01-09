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

(defparameter *core-meta-nodes*
  (external-meta-nodes
   `((if (cond then (,+optional-argument+ else)) ((:strictness (or cond (and then else)))))
     (:member (object key) ((:strictness (or object key))))

     ;; Reference Old Values
     (:previous-value (node) ((:strictness node)))

     ;; Equality
     ((:symbol-equal "symbol-equal") (a b) ((:strictness (or a b))))

     ;; Failures
     (:catch (try catch (,+optional-argument+ test)) ((:strictness try)))
     (:fail ((,+optional-argument+ type)))
     (:fail-type (thing) ((:strictness thing)))

     ;; Failure Types
     ((:empty-list "Empty-List") ())
     ((:no-value "No-Value%") ())
     ((:type-error "Type-Error%") ())
     ((:index-out-bounds "Index-Out-Bounds%") ())

     ((:invalid-integer "Invalid-Integer%") ())
     ((:invalid-real "Invalid-Real%") ())

     ((:arity-error "Arity-Error%") ())))

  "Map of meta-nodes which comprise the language primitives.")

(defconstant +core-macro-nodes+
  `((-> ,+bind-operator+ 10 :right)
    (\: ,+def-operator+ 5 :right)
    (|.| ,+subnode-operator+ 1000 :left)
    (& ,+ref-operator+)
    (|..| ,+outer-operator+)
    (prog ,+list-operator+))

  "List of core macro nodes where each a element is a list with the
   first item being the node's identifier and the second item being
   the operator to which the macro node expands to.")


(defun create-builtin-module (modules)
  "Creates the builtin module."

  (let ((builtin (ensure-module (id-symbol "builtin") modules)))
    (add-core-nodes builtin)
    (add-core-macros builtin)

    (let* ((name (id-symbol "c"))
           (chr-macro (external-meta-node name '(sym))))

      (setf (node-macro-function chr-macro) #'chr-macro)
      (add-meta-node name chr-macro builtin)
      (export-node name builtin))

    builtin))

(defun chr-macro (operator operands module)
  "Character macro-node function. Converts its argument to a literal
   character."

  (declare (ignore operator module))

  (destructuring-bind (thing) operands
    (atypecase (unwrap-declaration thing)
      (symbol
       (char (symbol-name it) 0))

      (string
       (char it 0))

      (integer
       (if (typep thing '(integer 0 9))
           (digit-char it)
           (fail-thunk)))

      (character
       it)

      (otherwise
       (fail-thunk)))))


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
    (let ((meta-node (macro-node node nil operator)))
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


;;;; Creating Core Expressions

(defun if-expression (test then else)
  "Creates an if functor expression with test TEST, then expression
   THEN, and else expression ELSE."

  (functor-expression (get 'if *core-meta-nodes*)
                      (list test then else)))

(defun member-expression (object key)
  (functor-expression (get :member *core-meta-nodes*)
                      (list object key)))

(defun fail-expression (&optional (type nil))
  (functor-expression (get :fail *core-meta-nodes*) (list type)))

(defun catch-expression (try catch &optional test)
  (functor-expression (get :catch *core-meta-nodes*)
                      (list try catch test)))
