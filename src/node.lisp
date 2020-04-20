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
    :initform (make-hash-map)
    :initarg :dependencies
    :accessor dependencies
    :documentation
    "Map of the node's dependency nodes, i.e. the nodes on which this
     node's value depends. Each key is a `NODE' object and the
     corresponding value is the `NODE-LINK' object.")

   (observers
    :initform (make-hash-map)
    :initarg :observers
    :accessor observers
    :documentation
    "Map of the node's observer nodes, i.e. the nodes whose values are
     dependent on the value of this node. Each key is a `NODE' object
     and the corresponding value is the `node-link' object of this
     node in the observer node's DEPENDENCIES map.")

   (contexts
    :accessor contexts
    :initform (make-hash-map)
    :documentation
    "Map of the node's contexts. Each key is a context identifier and
     the corresponding value is the `NODE-CONTEXT' object.")

   (attributes
    :accessor attributes
    :initform (make-hash-map :test #'cl:equalp)
    :documentation
    "Map of the node's attributes (miscellaneous key-value pairs)."))

  (:documentation
   "Base node class. Stores the binding information about a node,
    i.e. the node's dependencies and observers, and the node's
    contexts."))

(defclass node-context ()
  ((operands
    :initarg :operands
    :initform (make-hash-map)
    :accessor operands
    :documentation
    "Map containing the dependency nodes which are operands to this
     context's value function. Each key is the dependency `NODE'
     object and the corresponding value is the `NODE-LINK' object.")

   (value-function
    :initarg :value-function
    :initform nil
    :accessor value-function
    :documentation
    "The context's value function."))

  (:documentation
   "Node context. A node context stores information about the node's
    value is computed at a particular moment (context)."))

(defclass context-node ()
  ((node
    :initarg :node
    :accessor node
    :documentation "The node")

   (fail-test
    :initarg :fail-test
    :accessor fail-test
    :documentation
    "Failure type test node.")

   (context-id
    :initarg :context-id
    :accessor context-id
    :documentation "The identifier of the node's context."))

  (:documentation
   "Proxy object which 'acts' as the node in the NODE slot however all
    bindings established to NODE are established in the context with
    the identifier stored in the CONTEXT-ID slot."))


;;;; Constructors

(defun constant-node (id value)
  "Creates a constant node with identifier ID and value VALUE."

  (let* ((name (id-symbol id))
         (node (make-instance 'node :name name)))
    (add-constant-function value node)
    node))

(defun constant-type-node (id)
  "Creates a constant type node with identifier ID."

  (let* ((name (id-symbol id))
         (node (make-instance 'node :name name)))
    (add-constant-function (node-ref node) node)
    node))


;;;; Hash Function

(defmethod hash ((node node))
  (hash (name node)))


;;;; Predicates

(defun node? (x)
  "Returns true if X is a `node'."

  (typep x 'node))

(defun value? (x)
  "Returns true if X is a literal value."

  (or (numberp x) (stringp x)))

(defun input-node? (node)
  "Returns true if NODE is an input node."

  (attribute :input node))


;;;; Utility Functions

(defun attribute (attribute node &optional default)
  "Retrieves a node attribute. ATTRIBUTE is converted to a string if
   it is a symbol. If NODE does not have such an attribute, DEFAULT is
   returned."

  (get (string attribute) (attributes node) default))

(defun (setf attribute) (value attribute node)
  "Sets the value of a node attribute. ATTRIBUTE is converted to a
   string if it is a symbol."

  (setf (get (string attribute) (attributes node)) value))

(defun home-module (node)
  "Returns the module in which NODE was originally declared."

  (get :module (attributes node)))

(defun bool-value (value)
  "Converts value to a boolean. 0 and NIL are treated as boolean
   false, everything else is treated as boolean true."

  (not (memberp value '(0 nil))))

;;;; Bindings

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

  (let ((source (reference-operand source target context)))
    (when (typep target 'module)
      (error 'target-node-error :node target))

    (typecase source
      (node
       (multiple-value-return (link in-hash?)
           (add-dependency source target
                           :context context
                           :add-function add-function)

         (unless in-hash?
           (add-observer target source link))))

      (meta-node-ref
       (prog1 source
         (when add-function
           (add-function source (context target context)))))

      (otherwise
       (prog1 source
         (when add-function
           (add-constant-function source target)))))))


;;; Adding Dependencies

(defgeneric add-dependency (dependency node &key context add-function)
  (:documentation
   "Adds DEPENDENCY as a dependency node of NODE. CONTEXT is the
    context identifier, of which, DEPENDENCY is an operand. If
    ADD-FUNCTION is true, the value function of the context is set to
    the node link of DEPENDENCY. Returns the `node-link' object of the
    dependency, as the first return value, and true, as the second
    value, if DEPENDENCY was already in the dependency set of NODE."))

(defmethod add-dependency (dependency node &key context add-function)
  (with-slots (dependencies observers) node
    (flet ((observer? (observer)
             (aand (get observer observers)
                   (setf (node-link-two-way-p it) t))))

      (multiple-value-return (link in-hash?)
          (ensure-get dependency dependencies
            (node-link dependency
                       :context context
                       :two-way-p (observer? dependency)))

        (when (not in-hash?)
          (let ((context (context node context)))
            (with-slots (operands) context
              (setf (get dependency operands) link)
              (when add-function
                (add-function link context)))))))))

(defmethod add-dependency (dependency (proxy context-node) &key context add-function)
  "Adds the dependency to the node stored in the NODE slot of PROXY, in
   the context with the ID given by the CONTEXT-ID slot of PROXY."

  (declare (ignore context))

  (with-slots (node fail-test context-id) proxy
    (multiple-value-return (link existed?)
        (add-dependency dependency node
                        :context context-id
                        :add-function nil)

      (when (and (not existed?) add-function)
        (add-function
         link
         (context node context-id)
         fail-test)))))


(defun add-function (expression context &optional test)
  "Adds EXPRESSION to the value function of CONTEXT. If CONTEXT does
   not have a value function function, its VALUE-FUNCTION slot is set
   to EXPRESSION. If CONTEXT does have a value function, it is wrapped
   in a CATCH-EXPRESSION, with test function TEST, which evaluates to
   the value of the existing function or the value of EXPRESSION in
   case of failure."

  (with-slots (value-function) context
    (setf value-function
          (if value-function
              (catch-expression value-function expression test)
              expression))))

(defgeneric add-constant-function (constant node)
  (:documentation
   "Adds the constant CONSTANT to the init context of NODE."))

(defmethod add-constant-function (constant node)
  (add-function constant (context node :init)))

(defmethod add-constant-function (constant (proxy context-node))
  "Adds CONSTANT to the value function of the context given by the
   CONTEXT-ID slot of PROXY."

  (with-slots (node fail-test context-id) proxy
    (add-function constant
                  (context node context-id)
                  fail-test)))


;;; Adding Observers

(defgeneric add-observer (observer node link)
  (:documentation
   "Adds OBSERVER as an observer node of NODE. LINK is the `node-link'
    object corresponding to the dependency NODE within the
    DEPENDENCIES hash-table of OBSERVER."))

(defmethod add-observer (observer node link)
  (setf (get observer (observers node)) link))

(defmethod add-observer (observer (proxy context-node) link)
  (add-observer observer (node proxy) link))

(defmethod add-observer ((observer context-node) node link)
  (add-observer (node observer) node link))


;;; Replacing `NODE-LINK's with Expressions

(defgeneric replace-dependency-link (node link fn)
  (:documentation
   "Replaces the `NODE-LINK' object LINK, in the value function of
    NODE, with the expression returned by applying the function FN on
    the new `NODE-LINK' object created for the dependency node.

    The entries in the dependency set of NODE and the observer set of
    the dependency are updated to the new `NODE-LINK' objects."))

(defmethod replace-dependency-link (node link fn)
  (with-accessors ((link-node node-link-node)
                   (context node-link-context))
      link

    (let ((new-link (node-link link-node :context context)))
      (-<>> (dependencies node)
            (get link-node)
            (setf <> new-link))

      (-<>> (context node context)
            operands
            (get link-node)
            (setf <> new-link))

      (-<>> (observers link-node)
            (get node)
            (setf <> new-link))

      (setf link-node (funcall fn new-link)))))

(defmethod replace-dependency-link ((node context-node) link fn)
  (replace-dependency-link (node node) link fn))


;;; Removing Bindings

(defun remove-observer (node observer)
  "Removes OBSERVER from the observer set of NODE."

  (remove-dependency observer node))

(defun remove-dependency (node dependency)
  "Removes DEPENDENCY from the dependency set of NODE."

  (with-slots (dependencies) node
    (when (aand (get dependency dependencies)
                (remove-operand node it))

      (erase dependencies dependency)
      (erase (observers dependency) node)

      t)))


;;;; Contexts

(defgeneric context (node context-id)
  (:documentation
   "Returns the context of NODE with identifier CONTEXT-ID. If node
    does have such a context, a new context is created."))

(defmethod context ((node node) context-id)
  (ensure-get context-id (contexts node) (make-instance 'node-context)))

(defmethod context ((context-node context-node) (id t))
  "Returns the context referenced by the `CONTEXT-NODE'. ID is ignored
   completely."

  (with-slots (node context-id) context-node
    (context node context-id)))


(defgeneric (setf context) (value node context-id)
  (:documentation
   "Sets the context, of NODE, with identifier CONTEXT-ID to VALUE."))

(defmethod (setf context) (value (node node) context-id)
  (setf (get context-id (contexts node)) value))


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


;;; Removing Dependencies

(defun remove-operand (node operand)
  "Removes the node OPERAND from the operands of CONTEXT. Returns true
   if the operand was removed, nil if the operand cannot be removed."

  (with-accessors ((link-node node-link-node) (context-id node-link-context)) operand
    (let ((context (context node context-id)))
      (with-slots (operands value-function) context
        (cond
          ((= (length operands) 1)
           (erase operands link-node)
           (setf value-function nil)

           (erase (contexts node) context-id)
           t)

          ((remove-operand-from-fn node context value-function operand)
           (when (emptyp (operands context))
             (erase (contexts node) context-id))
           t))))))

(defgeneric remove-operand-from-fn (node context fn operand)
  (:documentation
   "Tries to remove the operand from the multi-operand context
    CONTEXT. Returns true if the operand was removed, NIL if it cannot
    be removed.")

  (:method ((node t) (context t) (fn t) (operand t))
    nil))

(defmethod remove-operand-from-fn (node context (fn object-expression) operand)
  (with-accessors ((operand-node node-link-node)) operand
    (with-accessors ((entries object-expression-entries)) fn
      (when (get operand-node (observers node))
        (awhen (first (find operand entries :key #'second))

          (erase (operands context) operand-node)
          (setf operand-node (previous-value node t))

          t)))))


(defun remove-context (node context-id)
  "Removes the context with identifier CONTEXT-ID if it is redundant,
   that is it has no operands and no value function."

  (with-slots (operands value-function) (context node context-id)
    (and (emptyp operands)
         (null value-function)
         (erase (contexts node) context-id))))


;;; Print Method

(defmethod print-object ((node node) stream)
  (format stream "<Node: ~a>" (name node)))
