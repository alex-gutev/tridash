;;;; compiler.lisp
;;;;
;;;; Tridash Programming Language.
;;;; Copyright (C) 2018-2021  Alexander Gutev
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


;;;; Build Entire Graph

(defvar *current-module* nil
  "The module to which the nodes currently being compiled are
   added. This differs from the module in the CURRENT-MODULE slot of
   *GLOBAL-NODE-TABLE* when building meta-node definitions.")

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

  (let ((*current-module* (current-module *global-module-table*))
        (*functor-module* (ensure-module :init *global-module-table*))
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
      ;; Update all meta-node instances to pass outer-nodes
      (foreach (rcurry #'update-meta-node-instances nil)
               (nodes node-table))

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

(defun finish-build (node-table &key in-meta-node)
  "Performs node coalescing, removal of unreachable nodes, constant
   folding and structure checking. NODE-TABLE is the `FLAT-NODE-TABLE'
   containing all nodes in all modules.

   IN-META-NODE is a flag for whether NODE-TABLE is the node table of
   a meta-node."

  (with-slots (nodes meta-nodes input-nodes) node-table
    ;; Node Coalescing
    (coalesce-all node-table :in-meta-node in-meta-node)

    ;; Finish Building Meta-Node Subgraphs
    (foreach #'finish-build-meta-node (meta-nodes node-table))

    ;; Remove unused meta-nodes
    (setf meta-nodes (remove-unused-meta-nodes nodes meta-nodes))))

(defgeneric finish-build-meta-node (meta-node)
  (:documentation
   "Performs the final build steps (node coalescing, etc.) in the
    definition of META-NODE. The DEFINITION of META-NODE is converted
    to a `FLAT-NODE-TABLE'.")

  (:method ((meta-node final-meta-node))
    nil)

  (:method ((meta-node external-meta-node))
    nil))

(defmethod finish-build-meta-node ((meta-node built-meta-node))
  (with-slots (name definition attributes) meta-node
    (let ((*meta-node* meta-node))
      ;; Determine the meta-node's outer node references
      (outer-node-references meta-node)

      ;; Update all meta-node instances to pass outer nodes
      (foreach (rcurry #'update-meta-node-instances meta-node)
               (remove-if-not #'node? (map-values (nodes definition))))

      (change-class meta-node 'final-meta-node
                    :definition (flatten-meta-node definition))

      (nadjoin meta-node (nodes definition))
      (finish-build definition :in-meta-node t)

      (remove-constant-outer-nodes meta-node))))


;;;; Build Meta-Nodes

(defvar *restricted-meta-nodes* (make-hash-set)
  "Set of meta-nodes which may not be used in a macro function or
   target transform.")

(defvar *create-top-level-nodes* nil
  "If true, a node is created for each top-level node declaration
   consisting of the node's identifier.")

(defmacro! restrict-meta-node (o!meta-node &body body)
  "Evaluates the forms in BODY with META-NODE added to
   *RESTRICTED-META-NODES*. This prevents the meta-node from being used
   inside a macro or target-transform function, during the evaluation
   of BODY."

  `(progn
     (when (memberp ,g!meta-node *restricted-meta-nodes*)
       (error 'compile-meta-node-loop-error :meta-node ,g!meta-node))

     (let ((*restricted-meta-nodes* (adjoin ,g!meta-node *restricted-meta-nodes*)))
       ,@body)))

(defun build-meta-nodes (meta-nodes)
  "Builds the definitions of the meta-nodes in META-NODES."

  (foreach #'build-meta-node meta-nodes))

(defgeneric build-meta-node (meta-node)
  (:documentation
   "Builds the definition of META-NODE, and converts it to a
    `BUILT-META-NODE' object.")

  (:method ((meta-node built-meta-node))
    meta-node)

  (:method ((meta-node external-meta-node))
    meta-node))

(defmethod build-meta-node :around ((meta-node meta-node))
  "Adds META-NODE to the restricted meta-nodes set, while invoking the
   next method."

  (restrict-meta-node meta-node
    (call-next-method)))

(defmethod build-meta-node ((meta-node meta-node-spec))
  "Builds the definition of META-NODE, from the declarations contained
   in its DEFINITION slot. The value of DEFINITION is replaced with
   the `MODULE' object containing the nodes in the definition."

  (with-slots (name definition operands attributes) meta-node
    (let* ((body (definition meta-node))
           (module (make-inner-module (home-module meta-node)))
           (value-node (make-self-node module))
           (*meta-node* meta-node)
           (*current-module* module)
           (*functor-module* module))

      (change-class meta-node 'built-meta-node
                    :operands (add-operand-nodes operands module)
                    :definition module)

      (let* ((*create-nodes* nil)
             (*create-top-level-nodes* t)
             (last-node (at-source (process-node-list body module :top-level t))))

        (make-meta-node-function meta-node value-node last-node)
        (add-binding value-node meta-node :add-function t)

        (let ((used (used-meta-nodes (map-values (nodes definition)))))
          (erase used meta-node)
          (setf (meta-node-references meta-node) used))

        (build-meta-nodes (meta-nodes definition))))))

(defun add-operand-nodes (operands module)
  "Creates nodes, in MODULE, for each operand in OPERANDS. Returns the
   operands list with the operand symbols replaced by the created
   local nodes."

  (flet ((add-operand (operand)
           (match operand
             ((list* type name value)
              (-<> (aprog1
                       (ensure-node name module t)
                     (add-input it module))
                   (list* type <> value)))

             (name
              (aprog1
                  (ensure-node name module t)
                (add-input it module))))))
    (map #'add-operand operands)))

(defun make-self-node (module)
  "Create the self node. MODULE is the `MODULE' containing the
   meta-node's definition."

  (ensure-node +self-node+ module t))

(defun make-meta-node-function (meta-node value-node last-node)
  "Creates the value function of the meta-node META-NODE. VALUE-NODE
   is the automatically added self node. LAST-NODE is the last-node in
   the meta-node's definition."

  (with-slots (output-nodes contexts) value-node
    (cond
      ((and last-node (emptyp contexts))
       (add-binding last-node value-node :context nil))

      ((> (length contexts) 1)
       (error 'ambiguous-meta-node-context-error :node meta-node))

      ((emptyp (contexts value-node))
       (error 'meta-node-no-function-error :meta-node meta-node)))))


;;;; Methods: Processing Declarations

(defmethod process-declaration ((atom atom-expression) module &key)
  "Compile an atom node expression.

   Creates a node with the given identifier and add it to MODULE, if
   it does not already contain a node with that identifier.

   Returns the corresponding node."

  (->> (and *create-top-level-nodes* (top-level?))
       (or *create-nodes*)
       (ensure-node (atom-expression-identifier atom) module)))

(defmethod process-declaration ((literal literal-value) module &key)
  "Compile a literal value.

   Simply returns the literal value."

  (literal-value-value literal))

(defmethod process-declaration :around (decl module &key top-level (add-outer t) ((:level *level*) (if top-level 0 (1+ *level*))) (add-stack t))
  "Processes the declaration with DECL added to the front of
   *DECLARATION-STACK*, and *LEVEL* incremented by one."

  (let* ((*declaration-stack* (if add-stack (cons decl *declaration-stack*) *declaration-stack*))
         (node (call-next-method)))

    (post-process-node node module :add-outer add-outer)))

(defun post-process-node (node module &key (add-outer t))
  "Perform the necessary post-processing on NODE, such as adding to
   the current META-NODE's list of referenced outer nodes."

  (when (meta-node? node)
    ;; Signal error if meta-node appears in target position.
    (when *source-node*
      (error 'target-node-error :node node)))

  ;; If inside a meta-node and node referenced is from an outer
  ;; module, add it to the outer-nodes set of the meta-node.
  (if (and add-outer
           *meta-node*
           (node? node)
           (not (meta-node? node))
           (not (in-home-module? node module)))
      (add-outer-node node module)
      node))

(defun add-outer-node (outer-node module &optional (meta-node *meta-node*))
  "Adds OUTER-NODE to the OUTER-NODES set of META-NODE and creates a
   local node in MODULE."

  (ensure-get outer-node (outer-nodes meta-node)
    (aprog1 (-> (outer-node-name meta-node)
                (ensure-node module t))

      (add-input it module))))


(defmethod process-declaration ((n null) (module t) &key)
  "Processes the NIL declaration. NIL declarations only originate from
   processing done internally by the frontend and not from
   user-written code."

  nil)

(defmethod process-declaration ((true (eql t)) (module t) &key)
  "Literal true constant."
  t)

(defmethod process-declaration ((node node) (module t) &key)
  "Returns the node."

  (labels ((in-module (module)
             (or (in-home-module? node module)
                 (aand (outer-module module)
                       (in-module it)))))

    ;; Ensure that NODE is either defined in the global scope or an
    ;; enclosing scope.
    (unless (or (global-module? (home-module node))
                (in-module module))
      (error 'node-reference-out-scope-error :node node)))

  node)


;;; Meta-Node instances

(defmethod process-declaration ((functor functor-expression) module &key)
  "Process a functor node expression."

  (with-struct-slots functor-expression- (operator operands) functor
    (-> (process-operator-node operator module)
        (process-meta-node-decl operator operands module))))

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

  (acond
    ((and *source-node* (target-transform-node meta-node))
     (let ((source (process-declaration (wrap-expression (gensym)) module)))
       (transform-target-meta-node
        it
        source
        (cons (canonicalize-node meta-node) operands)
        module)
       source))

    ((node-macro-function meta-node)
     (funcall it operator operands module))

    (t
     (make-meta-node-instance meta-node operator operands module))))

(defun make-meta-node-instance (meta-node operator operands module)
  "Creates a functor node which invokes the meta-node META-NODE with
   operands OPERANDS, and adds it to *FUNCTOR-MODULE*"

  (when (and *source-node* (null (target-meta-node meta-node)))
    (error 'target-node-error :node (cons operator operands)))

  (add-meta-node-instance meta-node operands module))


(defun add-meta-node-instance (meta-node operands module)
  "Creates a node with a VALUE-FUNCTION function that invokes the
   meta-node META-NODE with operands OPERANDS, and adds it to
   *FUNCTOR-MODULE*. If such a node already exists it is returned."

  (with-accessors ((target-meta-node target-meta-node)) meta-node
    (multiple-value-bind (instance operand-nodes)
        (create-instance-node meta-node operands module)
      (add-meta-node-value-function instance meta-node operand-nodes)

      (when (and *source-node* target-meta-node)
        (make-reverse-bindings target-meta-node instance operands module))

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
          (ir:functor
           (if (meta-node? meta-node)
               meta-node
               (add-binding meta-node instance :context context :add-function nil)))
          (setf value-function))))

(defun make-arguments (meta-node module arguments)
  "Creates the argument list for a functor expression."

  (cond
    ((meta-node? meta-node)
     (let ((map (make-argument-map meta-node arguments)))
       (order-arguments meta-node map module)))

    (t arguments)))

(defun order-arguments (meta-node map module)
  "Order meta-node instance argument values.

   META-NODE is the meta-node.

   MAP is a map mapping the meta-node operand names to the
   corresponding argument value nodes.

   MODULE is the module in which the default meta-node value nodes
   should be added."

  (with-slots (operands) meta-node
    (labels ((get-arguments (operands)
               (match operands
                 ((list (list (eql +rest-argument+) operand))
                  (list (ir:argument-list (ensure-list (get-operand operand)))))

                 ((list*
                   (list (eql +optional-argument+) operand default)
                   operands)

                  (multiple-value-bind (value supplied-p)
                      (get-operand operand)

                    (cons
                     (if supplied-p
                         value
                         (post-process-node default module))

                     (get-arguments operands))))

                 ((list* operand operands)
                  (multiple-value-bind (value supplied-p)
                      (get-operand operand)

                    (if supplied-p
                        (cons value (get-arguments operands))
                        (error 'missing-argument-error
                               :meta-node meta-node
                               :operand (operand-name operand)))))))

             (get-operand (operand)
               (multiple-value-prog1
                   (get (operand-name operand) map)

                 (erase map (operand-name operand))))

             (operand-name (operand)
               (etypecase operand
                 (node (name operand))
                 (symbol operand))))

      (prog1 (get-arguments operands)
        (unless (emptyp map)
          (error 'unknown-arguments-error
                 :meta-node meta-node
                 :arguments (map-keys map)))))))

(defun make-argument-map (meta-node values)
  "Create a map mapping meta-node operand names to their supplied values.

   META-NODE is a meta-node.

   VALUES is the list of values supplied to the meta-node instance.

   Returns a HASH-MAP."

  (with-slots (operands) meta-node
    (let ((map (make-hash-map))
          (num-values (length values)))

      (labels ((add-positional (operands values)
                 (multiple-value-match (values operands values)
                   ((_
                     (list* (list (eql +optional-argument+) (type symbol) _) _))

                    (add-keyword values))

                   (((list (list (eql +rest-argument+) operand))
                     _)

                    (add-arg operand values))

                   (((or (list* (list (eql +optional-argument+) operand _) _)
                         (list* operand operands))
                     (list* value values))

                    (add-arg operand value)
                    (add-positional operands values))

                   ((nil (list* _ _))
                    (error 'too-many-arguments-error
                           :meta-node meta-node
                           :max-arguments (cdr (meta-node-arity meta-node))
                           :num-arguments num-values))))

               (add-keyword (values)
                 (match values
                   ((list* (list (eql +optional-argument+) (and name (type symbol)) value)
                           values)
                    (add-arg name value)
                    (add-keyword values))

                   (nil)

                   (_
                    (error 'malformed-keyword-arguments-error))))

               (add-arg (operand value)
                 (setf (get (operand-name operand) map) value))

               (operand-name (operand)
                 (etypecase operand
                   (node (name operand))
                   (symbol operand))))

        (add-positional operands values)
        map))))

(defmethod process-attribute ((node t) (attribute (eql (id-symbol "TARGET-NODE"))) value module)
  (process-operator-node (wrap-expression value) module))


;;; Meta-Nodes appearing as targets of a binding

(defun transform-target-meta-node (transform source decl module)
  (-<> (list (unwrap-declaration source) (unwrap-declaration decl))
       (call-meta-node transform <>)
       (let ((*tridash-call-reason* :target-transform)) <>)
       wrap-expression
       (process-declaration module :top-level t)))


;;; Binding Operands

(defun process-operands (operands module &key ((:source *source-node*)) ((:create *create-nodes*) (null *meta-node*)))
  "Creates the operand nodes and adds them to MODULE if they are not
   already in MODULE. Returns the list of `node' objects in the first
   value and the module in the second."

  (flet ((process-operand (operand)
           (match operand
             ((functor-expression-
               (operator
                (atom-expression- (identifier (eql +optional-argument+))))
               (operands
                (list (atom-expression- (identifier name)) value)))

              (list +optional-argument+
                    name
                    (process-declaration value module)))

             (_
              (process-declaration operand module)))))

    (values
     (map #'process-operand operands)
     module)))

(defun bind-operands (node operands &key context)
  "Establishes bindings between the operands (OPERANDS) and the
   meta-node instance (NODE)."

  (labels ((bind-operand (operand)
             (atypecase (reference-operand operand node context)
               (node
                (add-binding operand node :context context :add-function nil))

               (ir:argument-list
                (->> operand
                     ir:arguments
                     (map #'bind-operand)
                     ir:argument-list))

               (otherwise it))))
    (map #'bind-operand operands)))
