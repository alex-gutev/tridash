;;;; special-operators.lisp
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

;;;; Methods for processing the special operators

(in-package :tridash.frontend)


;;; Registering Infix/Prefix Operators

(defmethod process-declaration ((decl register-operator-expression) (module t) &key)
  "Process register infix/prefix operator declaration."

  (ensure-top-level +register-operator-operator+
    (match-syntax +register-operator-operator+
        (register-operator-expression-operands decl)

      ((or (list* (atom op)
                  (integer-literal precedence)
                  (optional
                   (atom
                    (and (type operator-associativity)
                         associativity))))

           (list* (atom (id "infix"))
                  (atom op)
                  (integer-literal precedence)
                  (optional
                   (atom
                    (and (type operator-associativity)
                         associativity)))))

       (add-operator :infix op precedence (operator-associativity associativity) (operator-nodes module)))

      ((list (atom (id "prefix"))
             (atom op)
             (integer-literal precedence))

       (add-operator :prefix op precedence nil (operator-nodes module))))))

(defun operator-associativity (assoc)
  "Returns the operator precedence (LEFT or RIGHT) for the precedence
   given as an argument to the op operator."

  (cond
    ((= assoc (id-symbol "left"))
     :left)
    ((or (= assoc (id-symbol "right"))
         (null assoc))
     :right)))


;;; Modules

(defmethod process-declaration ((decl module-expression) module &key)
  "Process set module declaration."

  (declare (ignore module))

  (ensure-top-level +module-operator+
    (match-syntax +module-operator+
        (module-expression-operands decl)

      ((list (atom module))
       (change-module module)))))

(defmethod process-declaration ((decl alias-expression) module &key)
  "Process add module alias node declaration."

  (ensure-top-level +alias-operator+
    (match-syntax +alias-operator+
        (alias-expression-operands decl)

      ((list (atom module-id) (atom alias))

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

(defmethod process-declaration ((decl import-expression) module &key)
  "Process import node declaration."

  (ensure-top-level +import-operator+
    (with-slots (all-nodes) module
      (match-syntax +import-operator+
          (import-expression-operands decl)

        ((list* (atom from-module) (rest (node _) nodes))
         (let ((from-module (get-module from-module)))
           (if nodes
               (foreach (rcurry #'import-node from-module module)
                        (map #'unwrap-declaration nodes))
               (import-all-nodes from-module module))))))))

(defmethod process-declaration ((decl export-expression) module &key)
  "Process export node declaration."

  (ensure-top-level +export-operator+
    (match-syntax +export-operator+
        (export-expression-operands decl)

      ((rest (node _) nodes)
       (foreach (rcurry #'export-node module)
                (map #'unwrap-declaration nodes))))))

(defmethod process-declaration ((decl in-module-expression) module &key)
  "Process reference to node in another module expression."

  (declare (ignore module))

  (match-syntax +in-module-operator+
      (in-module-expression-operands decl)

    ((list (atom module) (node node))
     (process-subnode (get-module module)
                      (unwrap-declaration node)))))


;;; Definitions

(defmethod process-declaration ((decl define-meta-node-expression) module &key)
  "Process meta-node definition."

  (ensure-top-level +define-meta-node-operator+
    (match-syntax +define-meta-node-operator+
        (define-meta-node-expression-operands decl)

      ((list* (functor (atom name) args) body)
       (add-meta-node
        name
        (make-instance 'meta-node-spec
                       :name name
                       :operands (parse-operands args module)
                       :definition body)
        module)))))

(defmethod process-declaration ((decl external-meta-node-expression) module &key)
  "Process external meta-node declaration."

  (ensure-top-level +external-meta-node-operator+
    (match-syntax +external-meta-node-operator+
        (external-meta-node-expression-operands decl)

      ((list* (atom name) operands)

       (-<> (parse-operands operands module)
            (add-external-meta-node name module :operands <>))))))

(defun parse-operands (operands module)
  "Parses the operand list, of a meta-node definition.

   OPERANDS: Operands of the meta-node definition.

   MODULE: Module in which the definition is processed

   Returns a `META-NODE-OPERANDS' structure."

  (labels ((parse-required (operands &optional args)
             (match operands
               ((list* (atom-expression- (identifier operand)) rest)

                (parse-required rest (cons operand args)))

               (_
                (values
                 (nreverse args)
                 operands))))

           (parse-optional (operands &optional args)
             (match operands
               ((list*
                 (functor-expression-
                  (operator
                   (atom-expression- (identifier (eql +optional-argument+))))
                  (operands
                   (list* (atom-expression- (identifier operand))
                          (optional default))))
                 rest)

                (-<>> (if default (process-declaration default module) :none)
                      (cons operand)
                      (cons <> args)
                      (parse-optional rest)))

               (_
                (values
                 (nreverse args)
                 operands))))

           (parse-rest (operands)
             (match operands
               ((list
                 (functor-expression-
                  (operator
                   (atom-expression- (identifier (eql +rest-argument+))))
                  (operands
                   (list (atom-expression- (identifier operand))))))

                operand)

               (nil)

               (_
                (error 'invalid-operand-list-error :operands operands)))))

    (multiple-value-bind (required rest)
        (parse-required operands)

      (multiple-value-bind (optional rest)
          (parse-optional rest)

        (multiple-value-bind (rest-arg)
            (parse-rest rest)

          (make-instance 'meta-node-operands
                         :required required
                         :optional optional
                         :rest-argument rest-arg))))))


;;; Node Attributes

(defmethod process-declaration ((decl attribute-expression) module &key)
  "Process set node attribute declaration."

  (ensure-top-level +attribute-operator+
    (match-syntax +attribute-operator+
        (attribute-expression-operands decl)

      ((list (node node)
             (or (string-literal attribute)
                 (atom attribute))
             value)

       (let* ((*create-nodes* t)
              (attribute (string attribute)))

         (let ((node (process-declaration node module))
               (value (unwrap-declaration value)))

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

(defmethod process-declaration ((decl attribute-processor-expression) module &key)
  "Process attribute processor declaration."

  (ensure-top-level +attribute-processor-operator+
    (match-syntax +attribute-processor-operator+
        (attribute-processor-expression-operands decl)

      ((list (or (string attribute)
                 (atom attribute))
             (node node))

       (let ((node (process-operator-node node module))
             (attribute (string attribute)))

         (unless (meta-node? node)
           (error 'not-meta-node-error :node node))

         (setf (get attribute (attribute-processors *global-module-table*))
               node))))))


;;; Node Lists

(defmethod process-declaration ((decl node-list-expression) module &key)
  "Process a list of nodes grouped in a node list expression."

  (process-node-list (node-list-expression-operands decl) module))

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



;;; Outer Nodes

(defmethod process-declaration ((decl outer-node-expression) module &key)
  "Process explicit outer node reference."

  (with-slots (outer-module) module
    (unless outer-module
      (error 'global-outer-reference-error))

    (match-syntax +outer-node-operator+
        (outer-node-expression-operands decl)

      ((list (node node))
       (lookup-node (unwrap-declaration node) outer-module)))))


;;; Bindings

(defmethod process-declaration ((decl bind-expression) module &key)
  "Process node binding declaration."

  (match-syntax +bind-operator+
      (bind-expression-operands decl)

    ((list source (node target))

     (with-source-node
         (at-source
           (let ((*create-nodes* (null *meta-node*)))
             (process-declaration source module)))

       (let* ((*create-nodes* t)
              (target (process-declaration target module))
              (value-link (add-binding *source-node* target)))

         (unless (top-level?)
           (let* ((name (canonicalize-functor :bind (list *source-node* target)))
                  (cond-node (ensure-node name *functor-module* t)))

             (ensure-binding (cond-node target :context *source-node* :add-function nil)
                 (cond-link)

               (replace-dependency-link
                target value-link
                (lambda (value-link)
                  (if-expression cond-link value-link (fail-expression)))))

             cond-node)))))))

(defmethod process-declaration ((decl context-expression) module &key)
  "Process explicit node context expression."

  (match-syntax +context-operator+
      (context-expression-operands decl)

    ((list* (node node)
            (node context-id)
            (optional (node fail-test)))

     (let ((node (process-declaration node module))
           (context-id (unwrap-declaration context-id)))
       (unless (node? node)
         (error 'not-node-error :node node))

       (if *source-node*
           (make-instance 'context-node
                          :context-id context-id
                          :node node
                          :fail-test
                          (if fail-test
                              (let ((*create-nodes* nil))
                                (-<> (process-declaration fail-test module)
                                     list
                                     (bind-operands node <> :context context-id)
                                     first
                                     (at-source)))
                              :none))
           node)))))


;;; Node References and Quoting

(defmethod process-declaration ((decl ref-expression) module &key)
  "Process raw node object reference."

  (match-syntax +ref-operator+
      (ref-expression-operands decl)

    ((list (node node))
     (ir:node-ref
      (process-declaration node module
                           :add-outer nil :level *level*)))))

(defmethod process-declaration ((decl quote-expression) module &key)
  "Process quote expression."

  (declare (ignore module))

  (match-syntax +quote-operator+
      (quote-expression-operands decl)

    ((list thing)
     (unwrap-declaration thing))))


;;; Subnodes

(defmethod process-declaration ((decl subnode-expression) module &key)
  "Process subnode expression."

  (match-syntax +subnode-operator+
      (subnode-expression-operands decl)

    ((list (node node) (atom key))
     (let* ((object-node (at-source (process-declaration node module :add-outer nil))))
       (process-subnode object-node (unwrap-declaration key))))))

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
         (subnode (ensure-node name module t))
         (object-decl (wrap-expression object-decl)))
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
        (setf value-function (ir:object)))

      (ensure-binding (subnode object-node :context :object :add-function nil)
          (link)

        (->> (context object-node :object)
             value-function
             ir:entries
             (push (list key link)))))))

(defmethod process-subnode ((module module) node)
  "Returns the node with identifier NODE in the module MODULE."

  (lookup-node node module))
