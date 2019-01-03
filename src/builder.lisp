;;;; builder.lisp
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

;;;; Functions for building the node definition structures out of the
;;;; parsed source files.

(in-package :tridash.frontend)


;;;; Builder State

(defparameter *level* 0
  "The nesting level of the declaration currently being processed. 0
   indicates the top-level.")

(defparameter *meta-node* nil
  "The meta-node whose subgraph is currently being built. NIL when the
   global graph is being built.")

(defparameter *source-node* nil
  "If the node currently being processed appears as the target of a
   binding, this variable is bound to the source node of the binding,
   otherwise is NIL.")


;;;; Utility Functions and Macros

(defmacro at-source (&body body)
  "Sets the node position to source in the dynamic extent of the forms
   in BODY. All declarations, processed by the forms in body, are
   treated as appearing in source position."

  `(let (*source-node*)
     ,@body))

(defmacro with-source-node (source &body body)
  "Evaluates the forms in BODY, with *SOURCE-NODE* bound to the
   result of the evaluation of the form SOURCE."

  `(let ((*source-node* ,source))
     ,@body))

(defun top-level? ()
  "Returns true if the declaration currently being processed is at the
   top level."

  (zerop *level*))

;;;; Declaration Processing Interface

(defgeneric process-declaration (decl table &key &allow-other-keys)
  (:documentation
   "Processes the declaration, creates the node(s) specified by the
    declaration and adds them to TABLE. Returns the node created, if
    any, and the table in which it is contained.

    If the :TOP-LEVEL keyword argument is provided and is T, the
    declaration is processed as though it appears at top-level
    otherwise it is processed as though it appears at the level
     (1+ *LEVEL*)."))

(defgeneric process-functor (operator operands table)
  (:documentation
   "Processes the declaration functor, creates the node(s) specified
    by the declaration and adds them to TABLE. Returns the node
    created, if any, and the table in which it is contained."))


;;;; Build Graph

(define-file-builder trd (path module-table)
  (with-open-file (in path)
    (build-parsed-nodes (make-parser in) module-table)))

(defun build-parsed-nodes (parser &optional (*global-module-table* *global-module-table*))
  "Builds the `NODE' objects from the node declarations returned by
   successively calling PARSER and adds them to the `NODE-TABLE' of
   the current module in the `MODULE-TABLE' given in the second
   argument. This function does not build meta-node definitions."

  (with-slots (node-table) *global-module-table*
    (loop
       for decl = (funcall parser (operator-nodes node-table))
       while decl
       do
         (build-node decl))))

(defun build-node (node &optional (*global-module-table* *global-module-table*))
  "Builds the `NODE' object from the node declarations NODE and adds
   it to the `NODE-TABLE' of the current module in the `MODULE-TABLE'
   given in the second argument."

  (let ((*declaration-stack* nil)
        (*source-node* nil))
    (process-declaration node (node-table *global-module-table*) :top-level t)))

(defun finish-build-graph (&optional (*global-module-table* *global-module-table*))
  "Builds the definitions of all meta-nodes in all modules, and
   performs node coalescing. This function should be called
   separately, to finish building the graph, after building individual
   nodes using BUILD-PARTIAL-GRAPH and BUILD-NODE. This function
   should not be called after calling BUILD-GRAPH."

  (with-slots (modules) *global-module-table*
    ;; Build meta-node definitions
    (maphash-values #'build-meta-node-graphs modules)

    ;; Determine outer node references
    (maphash-values #'find-outer-node-references modules)
    (maphash-values #'add-outer-node-operands modules)

    ;; Fold constant nodes
    (maphash-values #'fold-constant-nodes modules)

    ;; Remove unreachable nodes
    (maphash-values #'remove-all-unreachable-nodes modules)

    ;; Check for cycles and ambiguous contexts
    (maphash-values #'check-structure modules)

    ;; Coalesce nodes
    (maphash-values #'coalesce-nodes modules)))


;;;; Build Meta-Nodes

(defun build-meta-node-graphs (table)
  "Builds the body of each meta-node, in the node table TABLE."

  (maphash-values (rcurry #'build-meta-node-graph table) (meta-nodes table)))

(defun build-meta-node-graph (meta-node outer-table)
  "Builds the graph corresponding to the body of the node
   meta-node. OUTER-TABLE is the node table in which the meta-node
   definition is located."

  (unless (external-meta-node? meta-node)
    (let* ((table (make-inner-node-table outer-table))
           (*meta-node* meta-node))

      ;; Add implicit self node
      (add-node +self-node+ meta-node table)

      (add-operand-nodes (operands meta-node) table)

      (let* ((last-node (process-node-list (definition meta-node) table :top-level t)))
        (make-meta-node-function meta-node last-node)
        (build-meta-node-graphs table)
        (setf (definition meta-node) table)))))

(defun add-operand-nodes (names table)
  "Creates a node for each element in NAMES, the element being the
   node name, and adds the nodes to TABLE. The nodes are marked as
   input nodes of TABLE"

  (with-slots (all-nodes) table

   (dolist (name names)
     (case (node-type (gethash name all-nodes))
       ((meta-node module)
        (remhash name all-nodes)))

     (add-input (ensure-node name table) table))))

(defun make-meta-node-function (meta-node last-node)
  "Creates the value function of the meta-node META-NODE. LAST-NODE is
   the last-node in the meta-node's definition."

  (with-slots (output-nodes contexts) meta-node
    (cond
      ((and last-node (zerop (hash-table-count contexts)))
       (add-binding last-node meta-node :context nil))

      ((> (hash-table-count contexts) 1)
       (error 'ambiguous-meta-node-context :node meta-node)))))


;;;; Methods: Processing Declaration

(defmethod process-declaration ((functor list) table &key top-level)
  "Processes the functor declaration by calling PROCESS-FUNCTOR with
   the OPERATOR argument being the CAR of FUNCTOR and OPERANDS being
   the CDR of FUNCTOR."

  (let ((*declaration-stack* (cons functor *declaration-stack*))
        (*level* (if top-level 0 (1+ *level*))))

    (destructuring-bind (operator . operands) functor
      (process-functor operator operands table))))

(defmethod process-declaration ((name symbol) table &key top-level)
  "Creates a node with identifier NAME and adds it to table, if table
   does not already contain a node with that identifier. Returns the
   newly created, or existing, node."

  (let* ((*declaration-stack* (cons name *declaration-stack*))
         (*level* (if top-level 0 (1+ *level*)))
         (node (ensure-node name table)))

    (unless (or *return-meta-node* (not (meta-node? node)) (eq node *meta-node*))
      (error 'node-type-error :node node :expected 'node))

    (values node table)))

(defmethod process-declaration (literal table &key)
  "Method for literal values (everything which is not a symbol or list
   is considered a literal). The literal value is simply returned."

  (declare (ignore table))
  literal)


;;;; Methods: Processing Functors

(defmacro match-syntax ((operator &rest expected) args &body clauses)
  `(match ,args
     ,@clauses
     (_
      (error 'invalid-arguments-error
             :operator ,operator
             :arguments ,args
             :expected ',expected))))

(defmacro ensure-top-level (operator &body body)
  "Signals an error condition of type SPECIAL-OPERATOR-OPERAND if the
   declaration currently being processed is not at top-level."

  `(progn
     (unless (top-level?)
       (error 'special-operator-operand :operator ,operator))

     ,@body))


;;; Special Operators

;;; Operator Declarations

(defmethod process-functor ((operator (eql +op-operator+)) args table)
  "Registers a node as an infix operator with a specific precedence
   and associativity."

  (ensure-top-level operator
    (match-syntax (+op-operator+ identifier number (or "left" "right"))
        args

      ((list* (guard op (symbolp op))
              (guard precedence (integerp precedence))
              (optional (list (guard associativity (symbolp associativity)))))

       (add-operator op precedence (operator-associativity associativity) (operator-nodes table))))))

(defun operator-associativity (assoc)
  "Returns the operator precedence (LEFT or RIGHT) for the precedence
   given as an argument to the op operator."

  (cond
    ((eq assoc (id-symbol "left"))
     :left)
    ((or (eq assoc (id-symbol "right"))
         (null assoc))
     :right)
    (t
     (error 'invalid-value-error
            :thing "operator associativity"
            :allowed '("left" "right")
            :value assoc))))


;;; Module Declarations

(defmethod process-functor ((operator (eql +module-operator+)) args table)
  "Changes the current module to the MODULE specified in ARGS."

  (declare (ignore table))

  (ensure-top-level operator
    (match-syntax (+module-operator+ identifier)
        args
      ((list (guard module (symbolp module)))
       (change-module module)))))

(defmethod process-functor ((operator (eql +use-operator+)) args table)
  "Adds a module as a node to the TABLE."

  (ensure-top-level operator
    (match-syntax (+use-operator+ (list identifier))
        args

      ((list* args)
       (iter (for module in args)
             (process-declaration (list +alias-operator+ module module) table :top-level t))))))

(defmethod process-functor ((operator (eql +alias-operator+)) args table)
  "Adds an alias for a module to TABLE."

  (ensure-top-level operator
    (match-syntax (+alias-operator+ identifier identifier)
        args
      ((list (guard module (symbolp module))
             (guard alias (symbolp alias)))

       (match (gethash alias (all-nodes table))
         ((type node)
          (error 'alias-clash-error
                 :node alias
                 :module module
                 :node-table table))

         ((and (type node-table) (not (eq table)))
          (error 'alias-taken-error
                 :node alias
                 :module module
                 :node-table table))

         (nil
          (setf (module-alias alias table) (get-module module))))))))

(defmethod process-functor ((operator (eql +import-operator+)) args table)
  "Imports nodes directly into TABLE, from another module."

  (ensure-top-level operator
    (with-slots (all-nodes) table
      (match-syntax (+import-operator+ identifier (list identifier))
          args

        ((list* (guard module (symbolp module)) nodes)
         (let ((module (get-module module)))
           (if nodes
               (mapcar (rcurry #'import-node module table) nodes)
               (maphash-keys (rcurry #'import-node module table) (public-nodes module)))))))))

(defmethod process-functor ((operator (eql +export-operator+)) args table)
  "Adds nodes to the public-nodes list of TABLE."

  (ensure-top-level operator
    (match-syntax (+export-operator+ (list identifier))
        args

      ((list* nodes)
       (mapcar (rcurry #'export-node table) nodes)))))

(defmethod process-functor ((operator (eql +in-module-operator+)) args table)
  "Looks up a node in another module, which does not have an alias in
   the current module."

  (match-syntax (+in-module-operator+ identifier node)
      args

    ((list (guard module (symbolp module)) node)
     (process-subnode (get-module module *global-module-table*) module node table))))


;;; Definitions

(defmethod process-functor ((operator (eql +def-operator+)) operands table)
  "Processes a meta-node definition. Creates a new meta-node and adds
   it to TABLE. If TABLE already contains a node with the same
   identifier but it is not a meta-node an error condition is
   signaled."

  (ensure-top-level operator
    (match-syntax (+def-operator+ ((identifier . identifier) . nodes))
        operands

      ((list* (list* (guard name (symbolp name)) args) body)
       (add-meta-node
        name
        (make-instance 'meta-node :name name :operands args :definition body)
        table)))))

(defmethod process-functor ((operator (eql +extern-operator+)) args table)
  "Adds a stub for an externally defined meta-node to TABLE."

  (ensure-top-level operator
    (match-syntax (+extern-operator+ (list identifier))
        args

      ((list* nodes)
       (mapcar (rcurry #'add-external-meta-node table) nodes)))))


;;; Attributes

(defmethod process-functor ((operator (eql +attribute-operator+)) args table)
  "Sets an attribute of a node to a particular value."

  (ensure-top-level operator
    (match-syntax (+attribute-operator+ node string literal)
        args
      ((list node
             (guard attribute (or (symbolp attribute) (stringp attribute)))
             value)


       (let* ((*return-meta-node* t)
              (attribute (string attribute)))

         (multiple-value-bind (node table)
             (process-declaration node table)

           (unless (node? node)
             (error 'node-type-error :expected 'node :node node))

           (when (equalp attribute "input")
             (add-input node table))

           (setf (attribute attribute node) value)))))))


;;; Node lists

(defmethod process-functor ((operator (eql +list-operator+)) nodes table)
  "Processes a list of nodes, returns the last node in the list."

  (process-node-list nodes table))

(defun process-node-list (nodes table &key (top-level (top-level?)))
  "Process a list of nodes, returns the last node in the list."

  (loop
     for (decl . rest) on nodes
     ;; At top-level if not last node or list node is at top-level
     for (node node-table) = (multiple-value-list (process-declaration decl table :top-level (or rest top-level)))
     finally
       (return (values node node-table))))


;;; Outer nodes

(defmethod process-functor ((operator (eql +outer-operator+)) args table)
  (with-slots (outer-table) table
    (unless outer-table
      (error 'global-outer-reference-error))

    (match-syntax (+outer-operator+ identifier)
        args

      ((list name)
       (multiple-value-bind (outer-node outer-table)
           (lookup-node name outer-table)

         (add-outer-node outer-node outer-table table))))))

(defun add-outer-node (outer-node outer-table table)
  "Adds a reference to NODE, which is located in an outer node-table (OUTER-TABLE),
   to the current meta-node being built (the value of *META-NODE*)."

  (-> (outer-node outer-node outer-table *meta-node*)
      (ensure-node table)
      (values table)))


;;; Bindings

(defmethod process-functor ((operator (eql +bind-operator+)) operands table)
  "Establishes a binding from the first node to the second node in
   OPERANDS."

  (match-syntax (+bind-operator+ node node)
      operands

    ((list source target)
     (with-source-node (at-source (process-declaration source table))
       (let* ((target (process-declaration target table))
              (value-link (add-binding *source-node* target)))

         (unless (top-level?)
           (let* ((name (cons operator operands))
                  (cond-node (ensure-node name table))
                  (cond-link (add-binding cond-node target :context *source-node* :add-function nil)))

             (setf (value-function (context target *source-node*))
                   `(if ,cond-link ,value-link ,(node-link :self)))

             (values cond-node table))))))))


;;; Subnodes

(defmethod process-functor ((operator (eql +subnode-operator+)) operands table)
  "Creates a node with a value function that accesses a particular
   field of an object. The binding is created in both directions, from
   the object node (the node containing the object value) to the
   subnode and from the subnode to the object node. In the latter
   direction the value of the field of the object value, stored in the
   object node, is updated."

  (match-syntax (+subnode-operator+ node identifier)
      operands

    ((list node key)
     (process-subnode (process-declaration node table) node key table))))


(defgeneric process-subnode (object-node object-decl key table)
  (:documentation
   "Generic function for processing subnode expressions."))

(defmethod process-subnode ((object-node node) object-decl key table)
  "Creates a node which references a field of an object stored in
   another node."

  (let* ((name (list +subnode-operator+ object-decl key))
         (subnode (ensure-node name table)))
    (make-source-subnode object-decl key subnode table)

    (handler-case
        (make-target-subnode object-decl key subnode table)
      (target-node-error ()))

    (values subnode table)))


(defun make-source-subnode (node key subnode table)
  "Makes the value function of the subnode for when it appears as the
   source of a binding. NODE is the object node referenced, KEY is the
   subnode key and SUBNODE is the subnode `NODE' object."

  (let ((node (at-source (process-declaration node table))))
    (create-context (subnode node)
      (setf value-function (list :member (bind node) key)))))

(defun make-target-subnode (node key subnode table)
  "Binds the subnode to the object node and updates the value function
   of the object node. NODE is the object node declaration, KEY is the
   field and SUBNODE is the subnode `NODE' object."

  (with-source-node subnode
    (let ((object-node (process-declaration node table)))
      (create-context (object-node :object)
        (setf value-function (list :object)))

      (ensure-binding (subnode object-node :context :object :add-function nil)
          (link)
        (push (list key link) (cdr (value-function (context object-node :object))))))))

(defmethod process-subnode ((module node-table) object-decl key table)
  "Returns the node with identifier KEY in the module MODULE."

  (declare (ignore object-decl table))
  (values (lookup-node key module) module))


;;; Meta-Node instances

(defmethod process-functor (operator operands table)
  "Creates a node with a VALUE-FUNCTION that invokes the meta-node
   with identifier OPERATOR and with operands OPERANDS. The operand
   nodes are added as dependencies of the node."

  (-> (lookup-meta-node operator table)
      (process-meta-node-decl operator operands table)))

(defun process-meta-node-decl (meta-node operator operands table)
  "Processes a node functor node declaration where the operator is the
   meta-node META-NODE. If the meta-node has a macro-function it is
   evaluated and the resulting node is returned, otherwise an instance
   of the meta-node is created."

  (aif (attribute :macro-function meta-node)
       (funcall it operator operands table)
       (make-meta-node-instance meta-node operator operands table)))

(defun make-meta-node-instance (meta-node operator operands table)
  "Creates a node which invokes the meta-node META-NODE with operands
   OPERANDS, and adds it to TABLE."

  (when (and *source-node* (null (target-meta-node meta-node)))
    (error 'target-node-error :node (cons operator operands)))

  (add-meta-node-instance meta-node operator operands table))


(defun add-meta-node-instance (meta-node operator operands table)
  "Creates a node with a VALUE-FUNCTION function that invokes the
   meta-node META-NODE with operands OPERANDS, and adds it to
   TABLE. If TABLE already contains such a node it is returned."

  (with-accessors ((target-meta-node target-meta-node)) meta-node
   (multiple-value-bind (instance operand-nodes table)
       (create-instance-node meta-node operator operands table)
     (add-meta-node-value-function instance meta-node operand-nodes)

     (when target-meta-node
       (handler-case
           (iter
             (with meta-node = (lookup-meta-node target-meta-node table))
             (for operand in (process-operands operands table instance))
             (add-meta-node-value-function operand meta-node (list instance) :context instance))
         (target-node-error ())))

     (values instance table))))

(defun create-instance-node (meta-node operator operands table)
  "Creates a meta-node instance node. The node is created with
   name (CONS OPERATOR OPERANDS) and added to TABLE. Additionally
   processes the operand node declarations in OPERANDS. Does not
   create the value function of the instance node. Returns three
   values: the instance node, the operand nodes and the table to which
   the instance was added."

  (let ((name (cons operator operands)))
    (multiple-value-bind (operands table) (process-operands operands table)

      ;; Add META-NODE to the meta node references of *META-NODE*
      (when (and *meta-node* (not (eq meta-node *meta-node*)))
        (ensure-gethash meta-node (meta-node-references *meta-node*)))

      (values (ensure-node name table t) operands table))))

(defun add-meta-node-value-function (instance meta-node operands &key (context meta-node))
  "Creates the value function of the meta-node instance INSTANCE,
   which invokes META-NODE with operands OPERANDS. OPERANDS are bound
   to INSTANCE and added as operands to the context CONTEXT."

  (create-context (instance context)
    (add-to-instances instance meta-node context)
    (setf value-function (cons meta-node (bind-operands instance operands :context context)))))

(defun add-to-instances (instance meta-node context)
  "Adds the meta-node instance INSTANCE, at context CONTEXT, to the
   list of instances of META-NODE."

  (push (list instance context *meta-node*) (instances meta-node)))


;;; Binding Operands

(defun process-operands (operands table &optional *source-node*)
  "Creates the operand nodes and adds them to table if they are not
   already in table. Returns the list of `node' objects as the first
   value and the table, in which a node object was found or created,
   with the greatest depth."

  (let ((*create-nodes* t))
    (iter
      (with op-table = (node-table *global-module-table*))
      (for operand in operands)

      (multiple-value-bind (node node-table)
          (process-declaration operand table)

        (collect node into nodes)

        (when (and node-table (> (depth node-table) (depth op-table)))
          (setf op-table node-table)))

      (finally (return (values nodes op-table))))))

(defun bind-operands (node operands &key context)
  "Establishes bindings between the operands (OPERANDS) and the
   meta-node instance (NODE)."

  (flet ((bind-operand (operand)
           (if (value? operand)
               operand
               (add-binding operand node :context context :add-function nil))))
  (mapcar #'bind-operand operands)))