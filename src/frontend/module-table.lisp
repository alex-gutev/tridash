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

  (ensure-get module (modules modules)
    (aprog1 (make-instance 'module :name module)
      (import-builtin-nodes modules it))))

(defun get-module (module &optional (modules *global-module-table*))
  "Returns the `MODULE' with identifier MODULE in MODULES. If there is
   no module with identifier MODULE in MODULES, signals a
   `NON-EXISTENT-MODULE-ERROR' condition."

  (with-retry-restart
    (or (get module (modules modules))
        (error 'non-existent-module-error :module-name module))))


;;;; Builtin Module Definition

(defconstant +core-meta-nodes+
  `((if ((cond then) ((else . :none)))
        ((:strictness (or cond (and then else)))))

    (:member ((object key)) ((:strictness (or object key))))

    ;; Reference Old Values
    (:previous-value ((node)) ((:strictness node)))

    ;; Equality
    ((:symbol-equal "symbol-equal") ((a b)) ((:strictness (or a b))))

    ;; Failures
    (:catch ((try catch) ((test . :none)))
            ((:strictness try)))

    (:fail (nil ((type . :none))))
    (:fail-type ((thing)) ((:strictness thing)))

    ;; Failure Types
    ((:empty-list "Empty-List") ())
    ((:no-value "No-Value%") ())
    ((:type-error "Type-Error%") ())
    ((:index-out-bounds "Index-Out-Bounds%") ())

    ((:invalid-integer "Invalid-Integer%") ())
    ((:invalid-real "Invalid-Real%") ())

    ((:arity-error "Arity-Error%") ()))

  "Builtin Meta-Node Specifications")

(defconstant +core-macro-nodes+
  `((,+bind-operator+ make-bind-expression 10 :right)
    (,+quote-operator+ make-quote-expression)
    (,+ref-operator+ make-ref-expression)
    (,+register-operator-operator+ make-register-operator-expression)
    (,+context-operator+ make-context-expression)
    (,+outer-node-operator+ make-outer-node-expression)
    (,+subnode-operator+ make-subnode-expression 1000 :left)

    (,+define-meta-node-operator+ make-define-meta-node-expression 5 :right)
    (,+external-meta-node-operator+ make-external-meta-node-expression)

    (,+attribute-operator+ make-attribute-expression)
    (,+attribute-processor-operator+ make-attribute-processor-expression)

    (,+module-operator+ make-module-expression)
    (,+import-operator+ make-import-expression)
    (,+alias-operator+ make-alias-expression)
    (,+export-operator+ make-export-expression)
    (,+in-module-operator+ make-in-module-expression)

    (,+list-operator+ make-node-list-expression))

  "List of core macro nodes where each a element is a list with the
   first item being the node's identifier and the second item being
   the name of the function implementing the macro.")

(defconstant +imported-core-nodes+
  (list +module-operator+
        +import-operator+
        +alias-operator+
        +export-operator+
        +in-module-operator+
        +list-operator+)

  "List of operators (from the builtin module) which should be
   imported automatically into every module.")

(defparameter *core-meta-nodes*
  (external-meta-nodes +core-meta-nodes+)

  "Map of meta-nodes which comprise the language primitives.")

(defparameter *node-true* (constant-type-node "True")
  "Node, of which the value represents boolean True.")

(defparameter *node-false* (constant-type-node "False")
  "Node, of which the value represents boolean False.")


(defun create-builtin-module (modules)
  "Creates the builtin module."

  (let ((builtin (ensure-module (id-symbol "builtin") modules)))
    (add-builtin-nodes builtin)

    builtin))

(defun add-builtin-nodes (builtin)
  (setf *core-meta-nodes* (external-meta-nodes +core-meta-nodes+))

  (add-core-nodes builtin)
  (add-core-macros builtin)

  ;; c - Character Macro Node

  (let* ((name (id-symbol "c"))
         (chr-macro (external-meta-node name '(sym))))

    (setf (node-macro-function chr-macro) #'chr-macro)
    (add-meta-node name chr-macro builtin)
    (export-node name builtin))

  ;; True and False Nodes

  (setf *node-true* (constant-type-node "True"))
  (setf *node-false* (constant-type-node "False"))

  (add-node (name *node-true*) *node-true* builtin)
  (add-node (name *node-false*) *node-false* builtin)

  (export-node (name *node-true*) builtin)
  (export-node (name *node-false*) builtin))


(defun add-core-nodes (builtin)
  "Adds the nodes in *CORE-META-NODES* to the module BUILTIN, and its
   export list."

  (doseq (node (map-values *core-meta-nodes*))
    (add-meta-node (name node) node builtin)
    (export-node (name node) builtin)))


(defun add-core-macros (builtin)
  "Adds the core macro nodes in +CORE-MACRO-NODES+ to the module
   BUILTIN."

  (doseq ((node fn &rest op-info) +core-macro-nodes+)
    (let ((meta-node (macro-node node nil fn)))
      (with-slots (name) meta-node
        (add-meta-node name meta-node builtin)
        (export-node name builtin)

        (when op-info
          (add-core-operator name op-info builtin))))))

(defun add-core-operator (name op-info builtin &key (position :infix))
  "Adds the identifier NAME to the operator table of module BUILTIN.

   NAME is the symbol naming the operator

   BUILTIN is the module to which to add the operator.

   OP-INFO is a list of the operator precedence and
   associativity.

   :POSITION is a symbol identifying the position in which the
   operator may occur. :INFIX indicates an infix operator and :PREFIX
   indicates a prefix operator."

  (destructuring-bind (precedence &optional (assoc :left)) op-info
    (add-operator position name precedence assoc (operator-nodes builtin))))

(defun make-node-list-expression (&key operator operands)
  (declare (ignore operator))
  (node-list-expression operands))

(defun chr-macro (operator operands module)
  "Character macro-node function. Converts its argument to a literal
   character."

  (declare (ignore operator module))

  (let ((*tridash-call-reason* :macro))
    (destructuring-bind (thing) operands
      (atypecase (unwrap-declaration thing)
        (symbol
         (char (symbol-name it) 0))

        (string
         (char it 0))

        (integer
         (if (<= 0 it 9)
             (digit-char it)
             (error 'tridash-fail :fail-type (get :type-error *core-meta-nodes*))))

        (character
         it)

        (otherwise
         (error 'tridash-fail :fail-type (get :type-error *core-meta-nodes*)))))))

(defun import-builtin-nodes (module-table module)
  "Import the builtin nodes, in the list +IMPORTED-CORE-NODES+ into
   MODULE."

  (with-slots (modules) module-table
    (when-let (builtin (get (id-symbol "builtin") modules))
      (loop
         for op in +imported-core-nodes+
         do
           (import-node op builtin module)))))

;;;; Creating Core Expressions

(defun if-expression (test then &optional (else :none))
  "Creates an if functor expression with test TEST, then expression
   THEN, and else expression ELSE."

  (ir:functor (get 'if *core-meta-nodes*)
              (list test then else)))

(defun member-expression (object key)
  (ir:functor (get :member *core-meta-nodes*)
              (list object key)))

(defun fail-expression (&optional (type :none))
  (ir:functor (get :fail *core-meta-nodes*) (list type)))

(defun catch-expression (try catch &optional (test :none))
  (ir:functor (get :catch *core-meta-nodes*)
              (list try catch test)))

;;;; Reference Previous Value of Node

(defun previous-value (node &optional (coalescablep nil))
  "Generate an expression which references the value of node prior to
   the latest node value update.

   If COALESCABLEP is T, NODE may be coalesced if it does not
   reference its own previous value, otherwise it may not be
   coalesced."

  (if coalescablep
      (setf (attribute :non-coalescable-self-reference node) t))

  ;; Set these two attributes as an actual runtime node object is
  ;; required to be able to reference its previous value.
  (setf (attribute :coalescable node) nil)
  (setf (attribute :removable node) nil)

  (->> node
       ir:node-ref
       list
       (ir:functor (get :previous-value *core-meta-nodes*))))
