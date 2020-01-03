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

(defvar *current-module* nil
  "The module to which the nodes currently being compiled are
   added. This differs from the module in the CURRENT-MODULE slot of
   *GLOBAL-NODE-TABLE* when building meta-node definitions.")

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

(defmethod process-declaration ((atom atom-node) module &key)
  (process-declaration (atom-node-identifier atom)
                       module
                       :level *level*
                       :add-stack nil))

(defmethod process-declaration ((functor functor-node) module &key)
  (process-declaration (cons (functor-node-operator functor) (functor-node-operands functor))
                       module
                       :level *level*
                       :add-stack nil))

(defmethod process-declaration ((literal literal-node) module &key)
  (process-declaration (literal-node-value literal)
                       module
                       :level *level*
                       :add-stack nil))


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

  (ensure-node name module (or *create-nodes*
                               (and *create-top-level-nodes* (top-level?)))))

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

(defmethod process-declaration (literal module &key)
  "Method for literal values (everything which is not a symbol or list
   is considered a literal). The literal value is simply returned."

  (declare (ignore module))
  literal)

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


;;;; Methods: Processing Functors

(defmacro match-syntax (operator (&rest patterns) args &body body)
  (labels ((make-pattern (pattern place)
             (let ((place (or place '_)))
               (match pattern
                 ('atom
                  `(or (atom-node- (identifier ,place))
                       (and (type symbol) ,place)))

                 ('node
                  `(and (or (type atom-node)
                            (type functor-node)
                            (type symbol)
                            (type list)
                            (type node)
                            (type module))
                        ,place))

                 ((list 'atom value)
                  (let ((sym (id-symbol value)))
                    `(or (atom-node- (identifier (and ',sym ,place)))
                         (and ',sym ,place))))

                 ((and (or 'integer 'string) type)
                  `(or (literal-node- (value (and (type ,type) ,place)))
                       (and (type ,type) ,place)))

                 ((list* 'functor (list op-pattern op-place) args)
                  `(and
                    (or (functor-node-
                         (operator ,(make-pattern op-pattern op-place))
                         (operands ,(make-patterns args)))

                        (cons ,(make-pattern op-pattern op-place)
                              ,(make-patterns args)))
                    ,place))

                 ((cons (and (or 'or 'and) op) patterns)
                  (cons op (map (rcurry #'make-pattern place) patterns)))

                 ('any place))))

           (make-patterns (patterns)
             (match patterns
               ((list (list 'rest (list* pattern (or (list place) nil))))
                (with-gensyms (x)
                  `(guard
                    ,place
                    (and (listp ,place)
                         (every
                          (lambda (,x)
                            (declare (ignorable ,x))
                            (match ,x
                              (,(make-pattern pattern '_) t)))
                          ,place)))))

               ((cons (list 'optional pattern) rest)
                `(or ,(make-patterns (cons pattern rest)) nil))

               ((cons (list* pattern (or (list place) nil)) rest)
                (list
                 'cons
                 (make-pattern pattern place)
                 (make-patterns rest)))))

           (extract-pattern (pattern)
             (match pattern
               ((or
                 (list (or 'rest 'optional) (list* pattern _))
                 (list* pattern _)
                 pattern)
                pattern))))

    `(match ,args
       (,(make-patterns patterns)
         ,@body)

       (_
        (error 'invalid-arguments-error
               :operator ,operator
               :arguments ,args
               :expected ',(map #'extract-pattern patterns))))))

(defmacro ensure-top-level (operator &body body)
  "Signals an error condition of type
   `SPECIAL-OPERATOR-REFERENCE-ERROR' if the declaration currently
   being processed is not at top-level."

  `(progn
     (unless (top-level?)
       (error 'special-operator-reference-error :operator ,operator))

     ,@body))

(defun unwrap-declaration (decl)
  "Converts a declaration, wrapped in `ATOM-NODE', `FUNCTOR-NODE' or
   `LITERAL-NODE' structures to a CL primitive representation."

  (match decl
    ((atom-node- identifier)
     identifier)

    ((functor-node- operator operands)
     (list* (unwrap-declaration operator)
            (map #'unwrap-declaration operands)))

    ((literal-node- value)
     value)

    ((type list)
     (map #'unwrap-declaration decl))

    (_ decl)))


;;; Special Operators

(defmethod process-functor ((operator atom-node) args module)
  (process-functor (atom-node-identifier operator) args module))


;;; Operator Declarations

(defmethod process-functor ((operator (eql +op-operator+)) args module)
  "Registers a node as an infix operator with a specific precedence
   and associativity, in MODULE."

  (ensure-top-level operator
    (match-syntax +op-operator+
        ((atom op) (integer precedence) (optional ((or (atom "left") (atom "right")) associativity)))
        args

      (add-operator op precedence (operator-associativity associativity) (operator-nodes module)))))

(defun operator-associativity (assoc)
  "Returns the operator precedence (LEFT or RIGHT) for the precedence
   given as an argument to the op operator."

  (cond
    ((= assoc (id-symbol "left"))
     :left)
    ((or (= assoc (id-symbol "right"))
         (null assoc))
     :right)))


;;; Module Declarations

(defmethod process-functor ((operator (eql +module-operator+)) args module)
  "Changes the current module to the MODULE specified in ARGS."

  (declare (ignore module))

  (ensure-top-level operator
    (match-syntax +module-operator+
        ((atom module))
        args

      (change-module module))))

(defmethod process-functor ((operator (eql +use-operator+)) args module)
  "Adds a module as a node to MODULE."

  (ensure-top-level operator
    (match-syntax +use-operator+
        ((rest (atom modules)))
        args

      (foreach #L(process-declaration (list +alias-operator+ %1 %1) module :top-level t)
               modules))))

(defmethod process-functor ((operator (eql +alias-operator+)) args module)
  "Adds an alias for a module to MODULE."

  (ensure-top-level operator
    (match-syntax +alias-operator+
        ((atom module-id) (atom alias))
        args

      (match (get alias (nodes module))
        ((or (type node)
             (guard it (and (typep it 'module)
                            (not (eq it (get-module module-id))))))

         (error 'create-alias-error
                :node-name alias
                :module-name module-id
                :module module))

        (_
         (setf (get alias (nodes module)) (get-module module-id)))))))

(defmethod process-functor ((operator (eql +import-operator+)) args module)
  "Imports nodes directly into MODULE, from another module."

  (ensure-top-level operator
    (with-slots (all-nodes) module
      (match-syntax +import-operator+
          ((atom from-module) (rest (node nodes)))
          args

        (let ((from-module (get-module from-module)))

          (if nodes
              (foreach (rcurry #'import-node from-module module)
                       (map #'unwrap-declaration nodes))
              (import-all-nodes from-module module)))))))

(defmethod process-functor ((operator (eql +export-operator+)) args module)
  "Adds nodes to the PUBLIC-NODES list of MODULE."

  (ensure-top-level operator
    (match-syntax +export-operator+
        ((rest (node nodes)))
        args

      (foreach (rcurry #'export-node module)
               (map #'unwrap-declaration nodes)))))

(defmethod process-functor ((operator (eql +in-module-operator+)) args module)
  "Looks up a node in another module, by identifier.."

  (declare (ignore module))

  (match-syntax +in-module-operator+
      ((atom module) (node node))
      args

    (process-subnode (get-module module)
                     (unwrap-declaration node))))


;;; Definitions

(defmethod process-functor ((operator (eql +def-operator+)) operands module)
  "Processes a meta-node definition. Creates a new meta-node and adds
   it to MODULE."

  (ensure-top-level operator
    (match-syntax +def-operator+
        (((functor (atom name) (rest (any args))))
         (rest (any body)))

        operands

      (add-meta-node
       name
       (make-instance 'meta-node-spec
                      :name name
                      :operands (parse-operands (map #'unwrap-declaration args) module)
                      :definition body)
       module))))

(defmethod process-functor ((operator (eql +extern-operator+)) args module)
  "Adds a stub for an externally defined meta-node to MODULE."

  (ensure-top-level operator
    (match-syntax +extern-operator+
        ((atom name) (rest (any operands)))
        args

      (-<> (map #'unwrap-declaration operands)
           (parse-operands module)
           (add-external-meta-node name module :operands <>)))))

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
    (match-syntax +attribute-operator+
        ((node node) ((or string atom) attribute) (any value))
        args

      (let* ((*create-nodes* t)
             (attribute (string attribute)))

        (let ((node (process-declaration (unwrap-declaration node) module))
              (value (unwrap-declaration value)))

          (unless (node? node)
            (error 'not-node-error :node node))

          (setf (attribute attribute node)
                (or
                 (-<> attribute
                      string-upcase
                      id-symbol
                      (process-attribute node <> value module))
                 value)))))))

(defmethod process-attribute (node (attribute (eql (id-symbol "INPUT"))) value (module t))
  "Adds NODE to the input-nodes list of its home module."

  (when (bool-value value)
    (add-input node (home-module node)))
  value)

(defmethod process-functor ((operator (eql +attribute-processor-operator+)) args module)
  "Sets the attribute processor for an attribute."

  (ensure-top-level operator
    (match-syntax +attribute-processor-operator+
        (((or string atom) attribute) (node node))
        args

      (let ((node (process-operator-node (unwrap-declaration node) module))
            (attribute (string attribute)))

        (unless (meta-node? node)
          (error 'not-meta-node-error :node node))

        (setf (get attribute (attribute-processors *global-module-table*))
              node)))))


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

    (match-syntax +outer-operator+
        ((node node))
        args

      (lookup-node (unwrap-declaration node) outer-module))))


;;; Bindings

(defmethod process-functor ((operator (eql +bind-operator+)) operands module)
  "Establishes a binding from the first node to the second node in
   OPERANDS."

  (match-syntax +bind-operator+
      ((any source) (node target))
      operands

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

            cond-node))))))

(defmethod process-functor ((operator (eql +context-operator+)) operands module)
  "Creates a `CONTEXT-NODE' proxy for the NODE given in the first
   argument and the context with identifier given in the second
   argument."

  (match-syntax +context-operator+
      ((node node) (node context-id) (optional (node fail-test)))
      operands

    (let ((node (process-declaration (unwrap-declaration node) module :level *level*)))
      (unless (node? node)
        (error 'not-node-error :node node))

      (if *source-node*
          (make-instance 'context-node
                         :context-id (unwrap-declaration context-id)
                         :node node
                         :fail-test
                         (let ((*create-nodes* nil))
                           (-<> (process-declaration (unwrap-declaration fail-test) module)
                                list
                                (bind-operands node <> :context context-id)
                                first
                                (at-source)
                                (and fail-test <>))))
          node))))

(defmethod process-functor ((operator (eql +ref-operator+)) operands module)
  "Returns a `NODE-REF' for the node passed as an argument."

  (match-syntax +ref-operator+
      ((node node))
      operands

    (node-ref
     (process-declaration (unwrap-declaration node) module
                          :add-outer nil :level *level*))))


;;; States

(defmethod process-functor ((operator (eql +state-operator+)) operands module)
  "Creates a state node, for a given state of a node, and binds it to
   the node in the :STATE context. Returns the state node."

  (match-syntax +state-operator+
      ((node node) (optional (atom from-state)) (optional (atom to-state)))
      operands

    (let* ((node (process-declaration node module :level *level*))
           (state-node
            (-> (canonicalize-functor +state-operator+ (list node))
                (ensure-node *functor-module* t))))

      (if from-state
          (let* ((from-state (unwrap-declaration from-state))
                 (to-state (unwrap-declaration to-state))
                 (state-link
                  (add-binding state-node node :context :state :add-function nil)))

            (make-explicit-state-node node state-link from-state to-state))

          state-node))))

(defun make-explicit-state-node (node state-link from-state to-state)
  "Creates a node which represents to the value of NODE when
   transitioning from FROM-STATE to TO-STATE. If TO-STATE is NIL it is
   assumed that the node represents the value of NODE when
   transitioning to the state FROM-STATE.

   STATE-LINK is the `node-link' object corresponding to the
   dependency node which stores the state of NODE."

  (let ((state-node
         (-<> (list* node from-state (ensure-list to-state))
              (canonicalize-functor +state-operator+ <>)
              (ensure-node *functor-module* t))))

    (ensure-binding (state-node node :context :state :add-function nil)
        (link)

      (setf (node-link-weak-p link) t)

      (let ((context (context node :state)))
        (with-slots (value-function) context
          (setf
           value-function

           (if-expression
            (make-state-check state-link from-state to-state)
            (previous-value state-node)

            (or value-function (previous-value node)))))))

    state-node))

(defun make-state-check (state-link from-state to-state)
  "Generate a Tridash expression which checks whether the state, given
   by the `node-link' object STATE-LINK, corresponds to the transition
   FROM-STATE to TO-STATE or whether the current state is FROM-STATE,
   if TO-STATE is NIL."

  (flet ((state-check (expression state)
           (functor-expression
            (get :symbol-equal *core-meta-nodes*)
            (list expression state))))

    (if to-state

        (with-struct-slots node-link- ((state-node node)) state-link
          (if-expression
           (state-check (previous-value state-node) from-state)
           (state-check state-link to-state)
           0))

        (state-check state-link from-state))))

(defun previous-value (node &optional (coalescablep nil))
  "Generate an expression which references the value of node prior to
   the latest node value update.

   If COALESCABLEP is T, NODE may be coalesced if it does not
   reference its own previous value, otherwise it may not be
   coalesced."

  (if coalescablep
      (setf (attribute :non-coalescable-self-reference node) t))

  (setf (attribute :coalescable node) nil)

  (->> node
       node-ref
       list
       (functor-expression (get :previous-value *core-meta-nodes*))))


;;; Subnodes

(defmethod process-functor ((operator (eql +subnode-operator+)) operands module)
  "Creates a node with a value function that accesses a particular
   field of an object. The binding is created in both directions, from
   the object node (the node containing the object value) to the
   subnode and from the subnode to the object node. In the latter
   direction the value of the field of the object value, stored in the
   object node, is updated."

  (match-syntax +subnode-operator+
      ((node node) (atom key))
      operands

    (let* ((node (unwrap-declaration node))
           (object-node (at-source (process-declaration node module :add-outer nil))))
      (process-subnode object-node key))))


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

  (acond
    ((and *source-node* (target-transform-node meta-node))
     (let ((source (process-declaration (gensym) module)))
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
          (setf value-function))))

(defun make-arguments (meta-node module arguments)
  "Creates the argument list for a functor expression."

  (cond
    ((meta-node? meta-node)
     (check-arity meta-node arguments)

     (labels ((match-arguments (operands arguments)
                (multiple-value-match (values operands arguments)
                  (((list* (list (eql +rest-argument+) _) _)
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
  (-<> (list (unwrap-declaration source) (unwrap-declaration decl))
       (call-meta-node transform <>)
       (let ((*tridash-call-reason* :target-transform)) <>)
       (process-declaration module :top-level t)))


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
