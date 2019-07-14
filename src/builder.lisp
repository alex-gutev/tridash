;;;; builder.lisp
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

;;;; Functions for building the node definition structures out of the
;;;; parsed source files.

(in-package :tridash.frontend)

(in-readtable cut-syntax)


;;;; Builder State

(defparameter *level* 0
  "The nesting level of the declaration currently being processed. 0
   indicates the top-level.")

(defvar *meta-node* nil
  "The meta-node which is currently being built. NIL when the global
   module is being built.")

(defvar *source-node* nil
  "If the node currently being processed appears as the target of a
   binding, this variable is bound to the source node of the binding,
   otherwise it is NIL.")

(defvar *functor-module* nil
  "The module in which functor nodes are created.")


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


;;;; Build Entire Graph

(define-file-builder trd (path module-table)
  (with-open-file (in path)
    (build-parsed-nodes (make-parser in) module-table)))

(defun build-parsed-nodes (parser &optional (*global-module-table* *global-module-table*))
  "Builds the `NODE' objects from the node declarations returned by
   successively calling PARSER and adds them to the CURRENT-MODULE of
   the `MODULE-TABLE' given in the second argument. This function does
   not build meta-node definitions."

  (with-slots (current-module) *global-module-table*
    (loop
       for decl = (funcall parser (operator-nodes current-module))
       while decl
       do
         (build-node decl))))

(defun build-node (node &optional (*global-module-table* *global-module-table*))
  "Builds the `NODE' object from the node declaration NODE and adds it
   to the CURRENT-MODULE of the `MODULE-TABLE' given in the second
   argument."

  (let ((*functor-module* (ensure-module :init *global-module-table*))
        (*declaration-stack* nil)
        (*source-node* nil))
    (process-declaration node (current-module *global-module-table*) :top-level t)))

(defun finish-build-graph (&optional (*global-module-table* *global-module-table*))
  "Performs the final build steps which include building the
   meta-nodes, node coalescing, removal of unreachable nodes, constant
   folding and structure checking. This function should be called
   separately, to finish building the graph, after adding the
   individual nodes using BUILD-PARTIAL-GRAPH and BUILD-NODE.

   Returns a `FLAT-NODE-TABLE' containing all nodes and meta-nodes."

  (with-slots (modules) *global-module-table*
    ;; Build meta-node definitions
    (foreach (compose #'build-meta-nodes #'meta-nodes) (map-values modules))

    (let ((node-table (flatten-modules (map-values modules))))
      ;; Determine outer node references
      (foreach #'outer-node-references (meta-nodes node-table))
      (foreach #'add-outer-node-operands (meta-nodes node-table))

      (finish-build node-table)

      node-table)))

(defun flatten-modules (modules)
  "Returns a `FLAT-NODE-TABLE' containing all nodes in all modules in
   MODULES."

  (let ((nodes (make-hash-set))
        (meta-nodes (make-hash-set))
        (input-nodes (make-hash-set)))

    (labels ((merge-module (module)
               (partition-nodes (map-values (nodes module)) nodes meta-nodes)
               (nunion input-nodes (input-nodes module))))

      (foreach #'merge-module modules)

      (make-instance 'flat-node-table
                     :nodes nodes
                     :meta-nodes meta-nodes
                     :input-nodes input-nodes))))

(defun flatten-meta-node (definition)
  "Returns a `FLAT-NODE-TABLE' containing all and only the nodes which
   were initially declared in the module DEFINITION."

  (let ((nodes (make-hash-set))
        (meta-nodes (make-hash-set)))

    (-> (remove-if-not (rcurry #'in-home-module? definition) (map-values (nodes definition)))
        (partition-nodes nodes meta-nodes))

    (make-instance 'flat-node-table
                   :nodes nodes
                   :meta-nodes meta-nodes
                   :input-nodes (input-nodes definition))))

(defun partition-nodes (nodes node-set meta-node-set)
  "Partitions NODES into `NODE's and `META-NODE's. `NODE's are added
   to NODE-SET and `META-NODE's are added to META-NODE-SET."

  (flet ((merge-node (node)
           (typecase node
             (meta-node
              (nadjoin node meta-node-set))

             (node
              (nadjoin node node-set)))))

    (foreach #'merge-node nodes)))

(defun finish-build (node-table)
  "Performs node coalescing, removal of unreachable nodes, constant
   folding and structure checking. NODE-TABLE is the `FLAT-NODE-TABLE'
   containing all nodes in all modules."

  (with-slots (nodes input-nodes) node-table
    ;; Fold constant nodes
    (fold-constant-nodes nodes)

    ;; Coalesce Nodes
    (coalesce-all node-table)

    ;; Finish Building Meta-Node Subgraphs
    (foreach #'finish-build-meta-node (meta-nodes node-table))))

(defun finish-build-meta-node (meta-node)
  "Performs the final build steps (node coalescing, etc.) in the
   definition of META-NODE. The DEFINITION of META-NODE is converted
   to a `FLAT-NODE-TABLE'."

  (with-slots (name definition) meta-node
    (when (typep definition 'module)
      ;; Add referenced outer-nodes as operands of each instance of each
      ;; meta-node.
      (foreach #'add-outer-node-operands (meta-nodes definition))

      (setf definition (flatten-meta-node definition))

      (nadjoin meta-node (nodes definition))
      (finish-build definition))))


;;;; Build Meta-Nodes

(defun build-meta-nodes (meta-nodes)
  "Builds the definitions of the meta-nodes in META-NODES."

  (foreach #'build-meta-node meta-nodes))

(defun build-meta-node (meta-node)
  "Builds the definition of META-NODE, from the declarations contained
   in its DEFINITION slot. The value of DEFINITION is replaced with
   the `MODULE' object containing the nodes in the definition."

  (with-slots (name definition) meta-node
    ;; Check that META-NODE is not an `EXTERNAL-META-NODE' and that it
    ;; has not been built already
    (unless (or (external-meta-node? meta-node)
                (typep definition 'module)
                (typep definition 'flat-node-table))

      (let* ((module (make-inner-module (home-module meta-node)))
             (value-node (make-self-node meta-node module))
             (*meta-node* meta-node)
             (*functor-module* module))

        (add-operand-nodes (operand-node-names meta-node) module)

        (let* ((*create-nodes* nil)
               (last-node (at-source (process-node-list definition module :top-level t))))
          (make-meta-node-function meta-node value-node last-node)
          (setf definition module)

          (build-meta-nodes (meta-nodes definition)))))))

(defun add-operand-nodes (names module)
  "Creates a node for each element in NAMES, the element being the
   node name, and adds the nodes to MODULE The nodes are added to the
   input nodes of MODULE."

  (dolist (name names)
    (add-input (ensure-node name module t) module)))

(defun make-self-node (meta-node module)
  "Create the self which represents the value of META-NODE. MODULE is
   the `MODULE' containing the meta-node's definition."

  (aprog1 (ensure-node +self-node+ module t)
    (add-binding it meta-node :add-function t)))

(defun make-meta-node-function (meta-node value-node last-node)
  "Creates the value function of the meta-node META-NODE. VALUE-NODE
   is the automatically added self node. LAST-NODE is the last-node in
   the meta-node's definition."

  (with-slots (output-nodes contexts) value-node
    (cond
      ((and last-node (emptyp contexts))
       (add-binding last-node value-node :context nil))

      ((> (length contexts) 1)
       (error 'ambiguous-meta-node-context-error :node meta-node)))))


;;;; Methods: Processing Declarations

(defmethod process-declaration ((functor list) module &key)
  "Processes the functor declaration by calling PROCESS-FUNCTOR with
   the OPERATOR argument being the CAR of FUNCTOR and OPERANDS being
   the CDR of FUNCTOR."

  (destructuring-bind (operator . operands) functor
    (process-functor operator operands module)))

(defmethod process-declaration ((name symbol) module &key)
  "Creates a node with identifier NAME and adds it to MODULE, if
   MODULE does not already contain a node with that
   identifier. Returns the newly created, or existing, node."

  (ensure-node name module))

(defmethod process-declaration :around (decl module &key top-level (add-outer t) ((:level *level*) (if top-level 0 (1+ *level*))))
  "Processes the declaration with DECL added to the front of
   *DECLARATION-STACK*, and *LEVEL* incremented by one."

  (let ((*declaration-stack* (cons decl *declaration-stack*))
        (node (call-next-method)))

    (post-process-node node module :add-outer add-outer)))

(defun post-process-node (node module &key (add-outer t))
  "Perform the necessary post-processing on NODE, such as adding to
   the current META-NODE's list of referenced outer nodes."

  (when (meta-node? node)
    ;; Signal error if meta-node appears in target position.
    (when *source-node*
      (error 'target-node-error :node node))

    ;; Add NODE to the meta-node references of *META-NODE*
    (when (and *meta-node* (/= node *meta-node*))
      (nadjoin node (meta-node-references *meta-node*))))

  ;; If inside a meta-node and node referenced is from an outer
  ;; module, add it to the outer-nodes set of the meta-node.
  (if (and add-outer
           *meta-node*
           (node? node)
           (not (meta-node? node))
           (not (in-home-module? node module)))
      (add-outer-node node (home-module node) module)
      node))

(defmethod process-declaration ((n null) (module t) &key)
  "Processes the NIL declaration. NIL declarations only originate from
   processing done internally by the frontend and not from
   user-written code."

  nil)

(defmethod process-declaration (literal module &key)
  "Method for literal values (everything which is not a symbol or list
   is considered a literal). The literal value is simply returned."

  (declare (ignore module))
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
  "Signals an error condition of type
   `SPECIAL-OPERATOR-REFERENCE-ERROR' if the declaration currently
   being processed is not at top-level."

  `(progn
     (unless (top-level?)
       (error 'special-operator-reference-error :operator ,operator))

     ,@body))


;;; Special Operators

;;; Operator Declarations

(defmethod process-functor ((operator (eql +op-operator+)) args module)
  "Registers a node as an infix operator with a specific precedence
   and associativity, in MODULE."

  (ensure-top-level operator
    (match-syntax (+op-operator+ identifier number (or "left" "right"))
        args

      ((list* (guard op (symbolp op))
              (guard precedence (integerp precedence))
              (optional (list (guard associativity (symbolp associativity)))))

       (add-operator op precedence (operator-associativity associativity) (operator-nodes module))))))

(defun operator-associativity (assoc)
  "Returns the operator precedence (LEFT or RIGHT) for the precedence
   given as an argument to the op operator."

  (cond
    ((= assoc (id-symbol "left"))
     :left)
    ((or (= assoc (id-symbol "right"))
         (null assoc))
     :right)
    (t
     (error 'invalid-value-error
            :thing "operator associativity"
            :allowed '("left" "right")
            :value assoc))))


;;; Module Declarations

(defmethod process-functor ((operator (eql +module-operator+)) args module)
  "Changes the current module to the MODULE specified in ARGS."

  (declare (ignore module))

  (ensure-top-level operator
    (match-syntax (+module-operator+ identifier)
        args
      ((list (guard module (symbolp module)))
       (change-module module)))))

(defmethod process-functor ((operator (eql +use-operator+)) args module)
  "Adds a module as a node to MODULE."

  (ensure-top-level operator
    (match-syntax (+use-operator+ (list identifier))
        args

      ((list* args)
       (foreach #L(process-declaration (list +alias-operator+ %1 %1) module :top-level t) args)))))

(defmethod process-functor ((operator (eql +alias-operator+)) args module)
  "Adds an alias for a module to MODULE."

  (ensure-top-level operator
    (match-syntax (+alias-operator+ identifier identifier)
        args
      ((list (guard module-id (symbolp module-id))
             (guard alias (symbolp alias)))

       (match (get alias (nodes module))
         ((or (type node)
              (guard it (and (typep it 'module)
                             (not (eq it (get-module module-id))))))

          (error 'create-alias-error
                 :node-name alias
                 :module-name module-id
                 :module module))

         (_
          (setf (get alias (nodes module)) (get-module module-id))))))))

(defmethod process-functor ((operator (eql +import-operator+)) args module)
  "Imports nodes directly into MODULE, from another module."

  (ensure-top-level operator
    (with-slots (all-nodes) module
      (match-syntax (+import-operator+ identifier (list identifier))
          args

        ((list* (guard from-module (symbolp from-module)) nodes)
         (let ((from-module (get-module from-module)))
           (if nodes
               (foreach (rcurry #'import-node from-module module) nodes)
               (foreach (rcurry #'import-node from-module module) (map-keys (public-nodes from-module))))))))))

(defmethod process-functor ((operator (eql +export-operator+)) args module)
  "Adds nodes to the PUBLIC-NODES list of MODULE."

  (ensure-top-level operator
    (match-syntax (+export-operator+ (list identifier))
        args

      ((list* nodes)
       (foreach (rcurry #'export-node module) nodes)))))

(defmethod process-functor ((operator (eql +in-module-operator+)) args module)
  "Looks up a node in another module, by identifier.."

  (declare (ignore module))

  (match-syntax (+in-module-operator+ identifier node)
      args

    ((list (guard module (symbolp module)) node)
     (process-subnode (get-module module) node))))


;;; Definitions

(defmethod process-functor ((operator (eql +def-operator+)) operands module)
  "Processes a meta-node definition. Creates a new meta-node and adds
   it to MODULE."

  (ensure-top-level operator
    (match-syntax (+def-operator+ ((identifier . identifier) . nodes))
        operands

      ((list* (list* (guard name (symbolp name)) args) body)
       (add-meta-node
        name
        (make-instance 'meta-node
                       :name name
                       :operands (parse-operands args module)
                       :definition body)
        module)))))

(defmethod process-functor ((operator (eql +extern-operator+)) args module)
  "Adds a stub for an externally defined meta-node to MODULE."

  (ensure-top-level operator
    (match-syntax (+extern-operator+ (list identifier))
        args

      ((list* (and (type symbol) node) operands)
       (add-external-meta-node node module
                               :operands (parse-operands operands module))))))

(defun parse-operands (operands module)
  "Parses the operand list OPERANDS. Checks that the structure of the
   list is correct and processes the default values of optional
   arguments."

  (match-state operands
    :start 'required
    (required
     (list* (and (type symbol) operand)
            rest)
     :from required

     (cons operand (next rest)))

    (optional
     (list*
      (list* (eql +optional-argument+)
             (and (type symbol) operand)
             (or (list default) nil))
      rest)

     :from (required optional)

     (cons (list +optional-argument+ operand (process-declaration default module))
           (next rest)))

    (rest
     (and
      (list (list (eql +rest-argument+) (type symbol)))
      whole)

     whole)

    (end nil)

    (other
     operands

     (error 'invalid-operand-list-error :operands operands))))


;;; Attributes

(defmethod process-functor ((operator (eql +attribute-operator+)) args module)
  "Sets an attribute of a node to a particular value."

  (ensure-top-level operator
    (match-syntax (+attribute-operator+ node string literal)
        args
      ((list node
             (guard attribute (or (symbolp attribute) (stringp attribute)))
             value)

       (let* ((*create-nodes* t)
              (attribute (string attribute)))

         (let ((node (process-declaration node module)))

           (unless (node? node)
             (error 'not-node-error :node node))

           (setf (attribute attribute node)
                 (or
                  (-<> attribute
                       string-upcase
                       id-symbol
                       (process-attribute node <> value module))
                  value))))))))

(defmethod process-attribute (node (attribute (eql (id-symbol "INPUT"))) value (module t))
  "Adds NODE to the input-nodes list of its home module."

  (when (bool-value value)
    (add-input node (home-module node)))
  value)


;;; Node lists

(defmethod process-functor ((operator (eql +list-operator+)) nodes module)
  "Processes a list of nodes, returns the last node in the list."

  (process-node-list nodes module))

(defun process-node-list (nodes module &key (top-level (top-level?)))
  "Process a list of nodes, returns the last node in the list."

  (loop
     for (decl . rest) on nodes
     ;; At top-level if not last node or list node is at top-level
     for node =
       (let ((*create-nodes* (if rest *create-nodes* (null *meta-node*))))
         (process-declaration decl module :top-level (or rest top-level)))
     finally
       (return node)))


;;; Outer nodes

(defmethod process-functor ((operator (eql +outer-operator+)) args module)
  (with-slots (outer-module) module
    (unless outer-module
      (error 'global-outer-reference-error))

    (match-syntax (+outer-operator+ identifier)
        args

      ((list name)
       (lookup-node name outer-module)))))

(defun add-outer-node (outer-node outer-module module)
  "Adds a reference to NODE, which is located in an outer module (OUTER-MODULE),
   to the current meta-node being built (the value of *META-NODE*)."

  (-> (outer-node outer-node outer-module *meta-node*)
      (ensure-node module t)))


;;; Bindings

(defmethod process-functor ((operator (eql +bind-operator+)) operands module)
  "Establishes a binding from the first node to the second node in
   OPERANDS."

  (match-syntax (+bind-operator+ node node)
      operands

    ((list source target)
     (with-source-node
         (at-source
           (let ((*create-nodes* (null *meta-node*)))
             (process-declaration source module)))

       (let* ((*create-nodes* t)
              (target (process-declaration target module))
              (value-link (add-binding *source-node* target)))

         (unless (top-level?)
           (let* ((name (canonicalize-functor operator (list *source-node* target)))
                  (cond-node (ensure-node name *functor-module* t)))

             (ensure-binding (cond-node target :context *source-node* :add-function nil)
                 (cond-link)

               (replace-dependency-link
                target value-link
                (lambda (value-link)
                  (if-expression cond-link value-link (fail-expression)))))

             cond-node)))))))

(defmethod process-functor ((operator (eql +context-operator+)) operands module)
  "Creates a `CONTEXT-NODE' proxy for the NODE given in the first
   argument and the context with identifier given in the second
   argument."

  (match-syntax (+context-operator+ node id)
      operands

    ((list node context-id)
     (let ((node (process-declaration node module :level *level*)))
       (if *source-node*
           (make-instance 'context-node :context-id context-id :node node)
           node)))))


;;; Subnodes

(defmethod process-functor ((operator (eql +subnode-operator+)) operands module)
  "Creates a node with a value function that accesses a particular
   field of an object. The binding is created in both directions, from
   the object node (the node containing the object value) to the
   subnode and from the subnode to the object node. In the latter
   direction the value of the field of the object value, stored in the
   object node, is updated."

  (match-syntax (+subnode-operator+ node identifier)
      operands

    ((list node key)
     (let ((object-node (at-source (process-declaration node module :add-outer nil))))
       (process-subnode object-node key)))))


(defgeneric process-subnode (object-node key)
  (:documentation
   "Generic function for processing subnode expressions."))

(defmethod process-subnode ((object-node node) key)
  "Creates a node which references a field of an object stored in
   another node."

  ;; Use the actual name of the object node
  (let* ((object-decl (name object-node))
         (module (home-module object-node))
         (name (list +subnode-operator+ object-decl key))
         (subnode (ensure-node name module t)))
    (make-source-subnode object-decl key subnode module)

    (handler-case
        (make-target-subnode object-decl key subnode module)
      (target-node-error ()))

    subnode))


(defun make-source-subnode (node key subnode module)
  "Makes the value function of the subnode for when it appears as the
   source of a binding. NODE is the object node referenced, KEY is the
   subnode key and SUBNODE is the subnode `NODE' object."

  (let ((node (at-source (process-declaration node module))))
    (create-context (subnode node)
      (->> (member-expression (bind node) key)
           (setf value-function)))))

(defun make-target-subnode (node key subnode module)
  "Binds the subnode to the object node and updates the value function
   of the object node. NODE is the object node declaration, KEY is the
   field and SUBNODE is the subnode `NODE' object."

  (with-source-node subnode
    (let ((object-node (process-declaration node module)))
      (create-context (object-node :object)
        (setf value-function (object-expression)))

      (ensure-binding (subnode object-node :context :object :add-function nil)
          (link)

        (->> (context object-node :object)
             value-function
             object-expression-entries
             (push (list key link)))))))

(defmethod process-subnode ((module module) node)
  "Returns the node with identifier NODE in the module MODULE."

  (lookup-node node module))


;;; Meta-Node instances

(defmethod process-functor (operator operands module)
  "Creates a node with a VALUE-FUNCTION that invokes the meta-node
   with identifier OPERATOR and with operands OPERANDS. The operand
   nodes are added as dependencies of the node."

  (-> (process-operator-node operator module)
      (process-meta-node-decl operator operands module)))

(defun process-operator-node (operator module)
  "Processes the operator node OPERATOR in MODULE. Signals an error if
   OPERATOR is not a node."

  (let ((*create-nodes* nil))
    (at-source
      (aprog1 (process-declaration operator module)
        (unless (node? it)
          (error 'non-node-operator-error :operator it))))))


(defun process-meta-node-decl (meta-node operator operands module)
  "Processes a functor node declaration where the operator is the
   meta-node META-NODE. If the meta-node has a macro-function it is
   evaluated and the resulting node is returned, otherwise an instance
   of the meta-node is created."

  (aif (node-macro-function meta-node)
       (funcall it operator operands module)
       (make-meta-node-instance meta-node operator operands module)))

(defun make-meta-node-instance (meta-node operator operands module)
  "Creates a functor node which invokes the meta-node META-NODE with
   operands OPERANDS, and adds it to *FUNCTOR-MODULE*"

  (acond
    ((aand *source-node* (target-transform-node meta-node))
     (let ((source (process-declaration (gensym) module)))
       (transform-target-meta-node
        it
        source
        (cons (canonicalize-node meta-node) operands)
        module)
       source))

    ((and *source-node* (null (target-meta-node meta-node)))
     (error 'target-node-error :node (cons operator operands)))

    (t
     (add-meta-node-instance meta-node operands module))))


(defun add-meta-node-instance (meta-node operands module)
  "Creates a node with a VALUE-FUNCTION function that invokes the
   meta-node META-NODE with operands OPERANDS, and adds it to
   *FUNCTOR-MODULE*. If such a node already exists it is returned."

  (with-accessors ((target-meta-node target-meta-node)) meta-node
    (multiple-value-bind (instance operand-nodes)
        (create-instance-node meta-node operands module)
      (add-meta-node-value-function instance meta-node operand-nodes)

      (when target-meta-node
        (if *source-node*
            (make-reverse-bindings target-meta-node instance operands module)
            (handler-case
                (make-reverse-bindings target-meta-node instance operands module)
              (target-node-error ()))))

      instance)))

(defun make-reverse-bindings (target-meta-node instance operands module)
  "Establishes the bindings between a meta-node instance and the
   operand nodes. TARGET-META-NODE is the meta-node which becomes the
   binding's function. INSTANCE is the meta-node instance `NODE' and
   OPERANDS is the list of the operand node declarations, which are
   processed in MODULE."

  (iter
    (for operand in (process-operands operands module :source instance :create t))
    (add-meta-node-value-function operand target-meta-node (list instance) :context instance)))

(defun create-instance-node (meta-node operands module)
  "Creates a meta-node instance node and adds it to
   *FUNCTOR-MODULE*. Additionally processes the operand node
   declarations in OPERANDS, in MODULE.  Returns two values: the
   instance node and the operand nodes.

   NOTE: Does not create the value function of the instance node."

  (let* ((operands (process-operands operands module :create (or (null *meta-node*) *source-node*)))
         (name (canonicalize-functor meta-node operands)))

    (values (ensure-node name *functor-module* t) operands)))

(defun add-meta-node-value-function (instance meta-node operands &key (context meta-node))
  "Creates the value function of the meta-node instance INSTANCE,
   which invokes META-NODE with operands OPERANDS. OPERANDS are bound
   to INSTANCE and added as operands to the context CONTEXT."

  (create-context (instance context)
    (-<>> (make-arguments meta-node (home-module instance) operands)
          (bind-operands instance <> :context context)
          (functor-expression
           (if (meta-node? meta-node)
               meta-node
               (add-binding meta-node instance :context context :add-function nil)))
          (setf value-function))

    (add-to-instances instance meta-node context value-function)))

(defun add-to-instances (instance meta-node context expression)
  "Adds the meta-node instance INSTANCE, at context CONTEXT, to the
   list of instances of META-NODE. EXPRESSION is the expression in
   which META-NODE is referenced."

  (when (meta-node? meta-node)
    (nadjoin (instance instance context *meta-node* expression)
             (instances meta-node))))

(defun make-arguments (meta-node module arguments)
  "Creates the argument list for a functor expression."

  (cond
    ((meta-node? meta-node)
     (check-arity meta-node arguments)

     (labels ((match-arguments (operands arguments)
                (multiple-value-match (values operands arguments)
                  (((list (list (eql +rest-argument+) _))
                    _)
                   (list (argument-list arguments)))

                  (((list*
                     (list*
                      (eql +optional-argument+)
                      _
                      (or nil (list default)))
                     operands)
                    nil)

                   (cons (post-process-node default module) (match-arguments operands nil)))

                  (((list* _ operands)
                    (list* argument arguments))

                   (cons argument (match-arguments operands arguments))))))

       (match-arguments (operands meta-node) arguments)))

    (t arguments)))


(defmethod process-attribute ((node t) (attribute (eql (id-symbol "TARGET-NODE"))) value module)
  (process-operator-node value module))


;;; Meta-Nodes appearing as targets of a binding

(defun transform-target-meta-node (transform source decl module)
  (process-declaration
   (resolve (call-tridash-meta-node transform (list source decl)))
   module
   :top-level t))


;;; Binding Operands

(defun process-operands (operands module &key ((:source *source-node*)) ((:create *create-nodes*) (null *meta-node*)))
  "Creates the operand nodes and adds them to MODULE if they are not
   already in MODULE. Returns the list of `node' objects in the first
   value and the module in the second."

  (values
   (map (rcurry #'process-declaration module) operands)
   module))

(defun bind-operands (node operands &key context)
  "Establishes bindings between the operands (OPERANDS) and the
   meta-node instance (NODE)."

  (labels ((bind-operand (operand)
             (atypecase (reference-operand operand node context)
               (node
                (add-binding operand node :context context :add-function nil))

               (argument-list
                (->> operand
                     argument-list-arguments
                     (map #'bind-operand)
                     argument-list))

               (otherwise it))))
    (map #'bind-operand operands)))


;;; Node Name Canonicalization

(defun canonicalize-functor (operator operands)
  "Returns a canonicalized declaration with which the
   functor (OPERATOR OPERANDS) can be referenced, from
   *FUNCTOR-MODULE*."

  (map #'canonicalize-node (cons operator operands)))

(defun canonicalize-node (node)
  "Returns a canonicalized declaration with which NODE can be
   referenced, from *FUNCTOR-MODULE*."

  (match node
    ((type node)
     (let* ((home (home-module node)))
       (if (or (outer-module home) (= home *functor-module*))
           (name node)
           (list +in-module-operator+ (name home) (name node)))))

    ((context-node node)
     (canonicalize-node node))

    (_ node)))
