;;;; backend.lisp
;;;;
;;;; Metalink Programming Language.
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

;;;; JavaScript Backend

(in-package :metalink.backend.js)


(defconstant +node-class+ "MetaLinkNode"
  "Runtime node class name.")

(defvar *node-table-var* "node_table"
  "Global node table variable.")


(defvar *node-link-indices* nil
  "Hash-table storing the dependency indices of each dependency of
   each node. Each key is a `NODE' object and the corresponding value
   is a hash-table mapping `NODE-LINK' objects (of the dependency
   nodes) to their dependency index.")

(defvar *meta-node-ids*
  "Hash-table mapping `META-NODE' objects to global meta-node function
   identifiers.")


;;;; Utility Functions

;;; Access Node Expressions

(defun access-node (node)
  "Returns an expression which references NODE."

  (js-element *node-table-var* (js-string (name node))))

(defparameter *node-path* #'access-node
  "Function which takes a node as an argument and returns an
   expression which references that node.")

(defun node-path (node)
  "Returns an expression which references NODE by calling the function
   bound to *NODE-PATH*."

  (funcall *node-path* node))


;;; Call Meta-Node Expressions

(defvar *in-tail-position* t
  "Boolean flag for whether the expression, currently being compiled,
   occurs in tail position of the value function, currently being
   compiled.")

(defun meta-node-id (meta-node)
  "Returns the global meta-node function/operator identifier of
   META-NODE. If META-NODE is a `META-NODE' object either a new
   identifier is created or the existing identifier is returned. If
   META-NODE is a symbol naming a primitive operator, it is returned
   as is."

  (etypecase meta-node
    (meta-node
     (ensure-gethash meta-node *meta-node-ids*
                     (mkstr "metanode" (hash-table-count *meta-node-ids*))))

    (symbol meta-node)))

(defun meta-node-call (meta-node operands)
  "Returns a `JS-CALL' expression for the meta-node META-NODE with
   operands OPERANDS."

  (make-js-call (meta-node-id meta-node) operands))


(defparameter *meta-node-call* #'meta-node-call
  "Function which should return a JS expression or block which invokes
   the meta-node, passed as the first argument, with the operands,
   passed as the second argument.")

(defun make-meta-node-call (meta-node operands)
  "Returns an expression or block which calls META-NODE with
   OPERANDS."

  (funcall *meta-node-call* meta-node operands))


;;;; Compilation

(defmethod compile-nodes ((backend (eql :javascript)) table)
  "Compile the node and meta-node definitions, in the `NODE-TABLE'
   TABLE, to JavaScript."

  (let ((*node-link-indices* (make-hash-table :test #'eq))
        (*meta-node-ids* (make-hash-table :test #'eq)))

    (output-code (generate-code table))))

(defun generate-code (table)
  "Generates the JavaScript code for the node and meta-node
   definitions in the `NODE-TABLE' TABLE."

  (with-slots (nodes meta-nodes) table
    (list
     (make-preamble)

     (create-nodes nodes)
     (create-meta-nodes meta-nodes)

     (init-nodes nodes))))

(defun make-preamble ()
  "Creates the code which should appear before any node
   definitions. Currently this contains only the declaration of the
   node table variable."

  (js-var *node-table-var* (js-object)))


;;;; Creating nodes

(defun create-nodes (nodes)
  "Generate the node creation code of each `NODE' in NODES."

  (loop
     for node being the hash-value of nodes
     append (create-node node)))

(defun create-node (node)
  "Generate the node creation code of NODE. This includes the creation
   of the node, its dependency queues and its value computation
   function, however it does not include the binding of the node to
   its observers."

  (let ((path (node-path node)))
    (establish-dependency-indices node)

    (list
     (js-call '= path (js-new +node-class+))

     (aand (dependencies-count node)
           (plusp it)
           (js-call (js-member path "make_dependencies") it))

     (awhen (create-compute-function node)
       (js-call '= (js-member path "compute") it)))))

(defun establish-dependency-indices (node)
  "Establishes the dependency indices of the dependencies of NODE, and
   adds them to *NODE-LINK-INDICES*."

  (dohash (dependency link (dependencies node))
    (dependency-index node link)))

(defun dependency-index (node link)
  "Returns the dependency index of the dependency (of NODE)
   corresponding to the `NODE-LINK' object LINK. If LINK does not have
   a dependency index, a new index is assigned to it."

  (let ((links (ensure-gethash node *node-link-indices* (make-hash-table :test #'eq))))
    (ensure-gethash link links (hash-table-count links))))


;;;; Creating meta-nodes

(defun create-meta-nodes (meta-nodes)
  "Generates the meta-node functions of each `META-NODE' in META-NODES."

  (loop
     for meta-node being the hash-value of meta-nodes
     collect (create-meta-node meta-node)))

(defun create-meta-node (meta-node)
  "Generates the meta-node function of META-NODE."

  (with-slots (definition operands value-function) meta-node

    (cond
      ;; If meta-node has no subnodes other than the operand nodes, it
      ;; can be coalesced to a single function
      ((and value-function
            (= (hash-table-count (nodes definition))
               (length operands)))

       (create-function-meta-node meta-node))

      (t (create-async-meta-node meta-node)))))


(defun create-function-meta-node (meta-node)
  "Generates the value-function of the `meta-node' META-NODE. This can
   only be used if META-NODE has no subnodes, other than its operand
   nodes."

  (with-slots (value-function operands) meta-node
    (let ((op-vars (make-operand-ids operands))
          (tail-recursive-p nil))

      (labels ((get-input (link)
                 (cdr (assoc (name (node-link-node link)) op-vars)))

               (make-meta-node-call (node operands)
                 (if (and *in-tail-position* (eq node meta-node))
                     (prog1
                         (js-block
                          (js-call '= (js-array (mapcar #'cdr op-vars)) (js-array operands))
                          (js-continue))
                       (setf tail-recursive-p t))
                     (meta-node-call node operands))))

        (let* ((*meta-node-call* #'make-meta-node-call)
               (body (make-function-body value-function #'get-input)))

          (js-function (meta-node-id meta-node)
                       (mapcar #'cdr op-vars)
                       (list
                        (if tail-recursive-p
                            (js-while "true" body)
                            body))))))))

(defun create-async-meta-node (meta-node)
  "Generates the value-function of the `meta-node'. Unlike
   CREATE-FUNCTION-META-NODE, the function generated does not directly
   return the computed value, instead it creates the meta-node's
   subgraph, dispatches the operand values to the operand nodes and
   returns a promise which is resolved when the value of the meta-node
   is computed. This function can be used to compile any meta-node."

  (let ((*node-link-indices* (make-hash-table :test #'eq)))

    (with-slots (definition operands) meta-node
      (symbol-macrolet ((promise-var "promise"))

        (let ((*node-path* (lambda (node)
                             (if (eq node meta-node) "self"
                                 (access-node node)))))

          (labels
              ((get-node (operand)
                 (js-element *node-table-var* (js-string operand)))

               (send-operand-wait (operand)
                 (js-call (js-member (get-node (car operand)) "send_wait")))

               (dispatch-operand-value (operand)
                 (js-call (js-member (get-node (car operand)) "dispatch")
                          (cdr operand)))

               (create-compute-fn ()
                 (let ((path (node-path meta-node)))
                   (js-call
                    '=
                    (js-member path "set_value")
                    (js-lambda
                     (list "value")
                     (list
                      (js-call (js-member promise-var "resolve") "value")))))))


            (let ((op-vars (make-operand-ids operands)))
              (js-function
               (meta-node-id meta-node)
               (mapcar #'cdr op-vars)
               (list (js-var "self")
                     (create-node meta-node)
                     (init-node meta-node)

                     (js-var "promise" (js-new "ValuePromise"))
                     (create-compute-fn)

                     (generate-code definition)
                     (mapcar #'send-operand-wait op-vars)
                     (mapcar #'dispatch-operand-value op-vars)
                     (js-return (js-member "promise" "promise")))))))))))

(defun make-operand-ids (operands)
  "Generates variable names for each operand. Returns an association
   list where each key is the operand and the associated value is the
   variable name."

  (loop
     for operand in operands
     for i = 0 then (1+ i)
     collect (cons operand (mkstr "a" i))))


;;;; Generate node compute functions

(defun create-compute-function (node)
  "Generates the value computation function of NODE. The anonymous
   function expression is returned."

  (symbol-macrolet ((values-var "values"))
    (flet ((get-input (link)
             (js-element values-var (dependency-index node link))))

      (with-slots (value-function) node
        (when value-function
          (js-lambda (list values-var)
                     (make-function-body value-function #'get-input)))))))


(defvar *get-input* nil
  "Function which should return an expression that references the
   value of the dependency corresponding to the `NODE-LINK' object
   passed as an argument.")

(defvar *var-counter* 0
  "Variable identifier counter. Appended as a suffix to variables
   introduced in a function.")


(defun make-function-body (function *get-input*)
  "Generates the body of the value computation function
   FUNCTION. *GET-INPUT* is a function which should return an
   expression that references the value of the dependency
   corresponding to the `NODE-LINK' object passed as an argument."

  (let ((*var-counter* 0))
    (make-statements function)))

(defun make-statements (fn &optional ret-var)
  "Generates the statements making up the body of the value
   function/expression FN, a list of the statements is returned. An
   assignment statement is generated, which stores the value computed
   by the expression in RET-VAR. If RET-VAR is NIL a return statement
   is generated instead."

  (multiple-value-bind (block expression)
      (make-expression fn ret-var)
    (list
     block
     (if expression (make-return expression ret-var)))))

(defun make-expression (fn var &optional (*in-tail-position* *in-tail-position*))
  "Generates the code for a single expression FN. VAR is the name of
   the variable in which the result of the expression is
   stored. Returns two values: a block, which computes the result and
   an expression for referencing that value. If FN is compiled to a
   single expression, the first return value is NIL and the second
   value is the expression. If VAR is NIL and FN is not compiled to a
   single expression, the second return value is NIL as the block
   returned in the first value contains a return statement."

  (match fn
    ((list 'if cond then else)
     (make-if cond then else var))

    ((list* meta-node operands)
     (make-call meta-node operands))

    ((type node-link)
     (values nil (funcall *get-input* fn)))

    (_ (values nil fn))))

(defun make-if (cond then else ret-var)
  "Generates an IF block for a conditional expression with condition
   COND, if-true expression THEN and else expression ELSE."

  (let ((cond-var (var-name)))
    (multiple-value-bind (cond-block cond-expr)
        (make-expression cond cond-var nil)
      (values
       (js-block
        (and cond-block cond-var)
        cond-block

        (js-if cond-expr
               (make-js-block (make-statements then ret-var))
               (make-js-block (make-statements else ret-var))))
       ret-var))))

(defun make-call (meta-node operands)
  "Generates code which invokes META-NODE with OPERANDS."

  (multiple-value-bind (op-block op-expressions)
      (make-operands operands)

    (let ((call (make-meta-node-call meta-node op-expressions)))
      (if (expressionp call)
          (values op-block call)
          (values (js-block op-block call) nil)))))

(defun make-operands (operands)
  "Generates code which computes the value of each operand. The first
   return value is a list of the statements of a block which computes
   the value of each operand. The second return value is a list of
   expressions for referencing the operands' values."

  (iter
    (for operand in operands)
    (for op-var = (var-name))

    (multiple-value-bind (op-block op-expr)
        (make-expression operand op-var nil)
      (when op-block
        (collect (js-var op-var) into block-statements)
        (collect op-block into block-statements))
      (collect op-expr into expressions))

    (finally (return
               (values
                (and block-statements (make-js-block block-statements))
                expressions)))))

(defun make-return (expression ret-var)
  "Generates a statement which returns the value computed by the JS
   expression EXPRESSION. If RET-VAR is NIL a return statement is
   generated otherwise a statement which stores the value of
   EXPRESSION in RET-VAR is generated."

  (if ret-var
      (js-call '= ret-var expression)
      (js-return expression)))

(defun var-name ()
  "Returns a new unused variable name."
  (mkstr "var" (incf *var-counter*)))


;;;; Generate dispatch methods

(defun init-nodes (nodes)
  "Generates the initialization code of each `NODE' in NODES."

  (loop
     for node being the hash-value of nodes
     append (init-node node)))

(defun init-node (node)
  "Generates the initialization code of NODE. This includes the
   binding of the node to its observers and the initialization of the
   wait set."

  (list
   (bind-observers node)
   (init-wait-set node)))

(defun bind-observers (node)
  "Generates code which binds the `NODE' NODE to its observers."

  (with-slots (observers) node
    (js-call
     (js-member (node-path node) "add_observers")
     (js-array
      (iter (for (observer link) in-hashtable (observers node))
            (for index = (dependency-index observer link))
            (collect (list (js-array (list (node-path observer) index)))))))))

(defun init-wait-set (node)
  "Generates the wait-set initialization code of the `NODE' NODE."

  (let ((wait-nodes (remove-duplicates (flatten (hash-table-values (wait-set node))))))
    (when wait-nodes
      (js-call '=
               (js-member (node-path node) "wait_nodes")
               (js-array (mapcar #'node-path wait-nodes))))))
