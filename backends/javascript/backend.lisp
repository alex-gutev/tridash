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

(defvar *meta-node-types* (make-hash-table :test #'eq))


;;;; Utility Functions

;;; Access Node Expressions

(defun access-node (node)
  "Returns an expression which references NODE."

  (js-element *node-table-var* (js-string (name node))))

(defparameter *node-path* #'access-node
  "Function which takes a node as an argument and returns an
   expression which references that node.")

(defun node-path (node)
  "Returns an expression which references NODE, by calling the
   functiOn bound to *NODE-PATH*."

  (funcall *node-path* node))


;;; Call Meta-Node Expressions

(defvar *in-tail-position* t
  "Boolean flag for whether the expression, currently being compiled,
   occurs in tail position of the value function, currently being
   compiled.")

(defvar *in-async* nil
  "Boolean flag for whether the expression, currently being compiled,
   is executed asynchronously from the execution of the value
   function.")

(defvar *return-variable* nil
  "The return variable in which the result computed by the expression,
   currently being compiled, should be stored. If NIL the result
   should be returned directly using a return statement.")


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
   operands OPERANDS. If the meta-node is asynchronous a CONS is
   returned, with the CAR being the symbol ASYNC and the CDR being the
   meta-node call expression."

  (let ((call (make-js-call (meta-node-id meta-node) operands)))
    (if (async-mete-node? meta-node)
        (cons 'async call)
        call)))


(defparameter *meta-node-call* #'meta-node-call
  "Function which should return a JS expression or block which invokes
   the meta-node, passed as the first argument, with the operands,
   passed as the second argument.")

(defun make-meta-node-call (meta-node operands)
  "Returns an expression or block which calls META-NODE with OPERANDS,
   by calling the function bound to *META-NODE-CALL*."

  (funcall *meta-node-call* meta-node operands))


;;;; Compilation

(defmethod compile-nodes ((backend (eql :javascript)) table)
  "Compile the node and meta-node definitions, in the `NODE-TABLE'
   TABLE, to JavaScript."

  (let ((*node-link-indices* (make-hash-table :test #'eq))
        (*meta-node-ids* (make-hash-table :test #'eq)))

    (output-code (make-preamble))
    (output-code (generate-code table))))

(defun generate-code (table)
  "Generates the JavaScript code for the node and meta-node
   definitions in the `NODE-TABLE' TABLE."

  (with-slots (nodes meta-nodes) table
    (list

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
    (case (meta-node-type meta-node)
      (sync
       (create-function-meta-node meta-node))
      (async
       (create-async-meta-node meta-node)))))


(defun async-mete-node? (meta-node)
  (eq (meta-node-type meta-node) 'async))

(defgeneric meta-node-type (meta-node)
  (:documentation
   "Returns SYNC if the meta-node computes its value synchronously,
    ASYNC if the meta-node computes its value asynchronously.")

  (:method ((meta-node symbol)) 'sync))

(defmethod meta-node-type ((meta-node meta-node))
  "Returns SYNC if `META-NODE' has no subnodes other than its operand
   nodes, that is the entire subgraph has been coalesced into the
   value function. Returns ASYNC if it has other subnodes besides the
   operand nodes."

  (with-slots (value-function operands definition) meta-node
    (ensure-gethash
     meta-node
     *meta-node-types*

     (cond
       ;; If meta-node has no subnodes other than the operand nodes, it
       ;; can be coalesced to a single function
       ((and value-function
             (= (hash-table-count (nodes definition))
                (length operands)))

        'sync)

       (t 'async)))))


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
                 (if (and *in-tail-position* (not *in-async*) (eq node meta-node))
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
              ((get-operand (operand)
                 "Returns an expression accessing the operand node
                  named by OPERAND."
                 (js-element *node-table-var* (js-string operand)))

               (send-operand-wait (operand)
                 "Generates an expression which calls the send_wait
                  method of the operand node OPERAND."
                 (js-call (js-member (get-operand (car operand)) "send_wait")))

               (dispatch-operand-value (operand)
                 "Generates an expression which calls the dispatch
                  method of the operand node passing in the operand's
                  value. The CAR of OPERAND contains the operand
                  node's name, the CDR the name of the operand
                  variable."

                 (js-call (js-member (get-operand (car operand)) "dispatch")
                          (cdr operand)))

               (make-meta-node-call (node operands)
                 "Generates an expression which invokes the meta-node
                  NODE with operands OPERANDS. If in tail position a
                  tail call is generated."

                 (if (and *in-tail-position* (async-mete-node? node))
                     (make-tail-call node operands)
                     (meta-node-call node operands)))

               (make-tail-call (node operands)
                 "Generates a tail call for the meta-node NODE with
                  operands OPERANDS. The tail call reuses the existing
                  promise and if it is a self-call the existing node
                  table is reused."

                 (js-block
                  (cdr (meta-node-call node (append operands
                                                    (list promise-var)
                                                    (when (eq node meta-node)
                                                      (list *node-table-var*)))))
                  (js-throw (js-new "EndUpdate"))))

               (create-value-node ()
                 "Generates the definition of the value node."

                 (list
                  (js-var "self")
                  (let ((*meta-node-call* #'make-meta-node-call))
                    (create-node meta-node))

                  (init-node meta-node)
                  (create-compute-fn)))

               (create-compute-fn ()
                 "Generates the compute function of the value node."

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
               (append (mapcar #'cdr op-vars)
                       (list (js-call '= promise-var (js-new "ValuePromise"))
                             *node-table-var*))

               (list
                ;; Wrap node table creation in an if which checks
                ;; whether a node table was provided as an argument.
                (js-if
                 (js-call '=== *node-table-var* "undefined")
                 (js-block
                  (js-call '= *node-table-var* (js-object))
                  (create-value-node)
                  (generate-code definition)))

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

(defun make-statements (fn)
  "Generates the statements making up the body of the value
   function/expression FN, the list of statements is returned. The
   list includes a statement which stores the result computed by the
   expression in *RETURN-VARIABLE*, or returns the result directly if
   *RETURN_VARIABLE* is NIL."

  (multiple-value-call #'add-to-block (make-expression fn)))

(defun add-to-block (block expression)
  "Creates a new block which contains BLOCK and a statement which
   returns the result of EXPRESSION, generated using MAKE-RETURN. The
   list of the block's statements is returned."

  (flet ((get-expression (expression)
           "If EXPRESSION is a CONS with the CAR equal to the symbol
            ASYNC, the expression stored in the CDR is
            returned. Otherwise EXPRESSION is returned as is."

           (match expression
             ((cons 'async expression) expression)
             (_ expression))))

    (list block
          (when expression
            (make-return (get-expression expression))))))

(defun make-expression (fn &key ((:return-variable *return-variable*) *return-variable*) ((:tailp *in-tail-position*) *in-tail-position*))
  "Generates the code for a single expression
   FN. :RETURN-VARIABLE (bound to *RETURN-VARIABLE*) is the name of
   the variable in which the result should be stored (NIL if it should
   be returned directly). :TAILP (bound to *IN-TAIL-POSITION*) if a
   flag for whether FN occurs in tail position.

   Returns two values: a block, which computes the result and an
   expression for referencing that value. If FN is compiled to a
   single expression, the first return value is NIL and the second
   value is the expression. If VAR is NIL and FN is not compiled to a
   single expression, the second return value is NIL as the block
   returned in the first value contains a return statement."

  (match fn
    ((list 'if cond then else)
     (make-if cond then else))

    ((list* meta-node operands)
     (make-call meta-node operands))

    ((type node-link)
     (values nil (funcall *get-input* fn)))

    (_ (values nil fn))))

(defun make-if (cond then else)
  "Generates an IF block for a conditional expression with condition
   COND, if-true expression THEN and else expression ELSE."

  (let ((cond-var (var-name)))
    (multiple-value-bind (cond-block cond-expr)
        (make-expression cond :return-variable cond-var :tailp nil)

      (multiple-value-call
          #'combine-blocks
        cond-block

        (use-expression
         cond-expr

         (lambda (cond-expr)
           (values
            (js-block
             (js-if cond-expr
                    (make-js-block (make-statements then))
                    (and else (make-js-block (make-statements else)))))
            *return-variable*)))))))

(defun make-call (meta-node operands)
  "Generates code which invokes META-NODE with OPERANDS."

  (make-operands
   operands
   (lambda (expressions)
     (let ((call (make-meta-node-call meta-node expressions)))
       (if (value-expression? call)
           (values nil call)
           (values call nil))))))

(defun make-operands (operands body-fn)
  "Generates code which computes the value of each operand. BODY-FN is
   a function, which is called to generate the code which makes use of
   the operands' values. It is called with one argument: a list of JS
   expressions which reference the values of the operands, and should
   return two values: a block and an expression.

   Two values are returned, a block which computes the values of the
   operands, and an expression.

   If all operands are synchronous expression: the block returned
   contains the statements of the block which computes the operands'
   values and the block returned by BODY-FN. The expression returned
   by BODY-FN is returned.

   If there is at least one of the operands is an asynchronous
   expression: the block returned is the block which computes the
   operand values, and the expression is a Promise with a then handler
   function that contains the block returned by BODY-FN followed by a
   return statement, which returns the expression returned by BODY-FN."

  (iter
    (for operand in operands)
    (for op-var = (var-name))

    (multiple-value-bind (op-block op-expr)
        (make-expression operand :return-variable op-var :tailp nil)

      (when op-block
        (collect (js-var op-var) into block-statements)
        (collect op-block into block-statements))

      (collect op-expr into expressions))

    (finally
     (return
      (multiple-value-call
          #'combine-blocks
        (and block-statements (make-js-block block-statements))
        (use-expressions expressions body-fn))))))


(defun make-return (expression)
  "Generates a statement which returns the value computed by the JS
   expression EXPRESSION. If *RETURN-VARIABLE* is non-NIL an
   assignment statement, which stores the value computed by EXPRESSION
   into the variable named by *RETURN-VARIABLE*, is generated
   otherwise a return statement is generated."

  (if *return-variable*
      (js-call '= *return-variable* expression)
      (js-return expression)))

(defun var-name (&optional (prefix "var"))
  "Returns a new unused variable name."
  (mkstr prefix (incf *var-counter*)))


(defun make-promise (expressions vars body)
  "Generates a promise expression, which waits for the promise
   returned by EXPRESSIONS to resolve. The promise has a then handler
   function attached to it, in which each element in VARS is the name
   of the variable which stores the resolved value of the
   corresponding expression in EXPRESSIONS, and BODY is the list of
   statements comprising the body of the handler function. EXPRESSIONS
   may either be a list or a single EXPRESSION, likewise VAR may
   either be a list or a single variable name."

  (let*-if ((args (js-array expressions) expressions)
           (promise (js-call (js-member "Promise" "all") args) expressions)
           (vars (js-array vars) vars))

      (listp expressions)

    (js-call
     (js-member promise "then")
     (js-lambda (list vars) body))))

(defun combine-blocks (block1 block2 expression)
  "Combines BLOCK1 BLOCK2 into a single block. Returns as the first
   value: a block which contains the statements of BLOCK1 and BLOCK2,
   and EXPRESSION as the second return value."

  (let ((statements1 (and block1 (js-block-statements block1)))
        (statements2 (and block2 (js-block-statements block2))))

    (values
     (when (or statements1 statements2)
       (js-block statements1 statements2))
     expression)))


(defun make-body-block (body-fn arg)
  "Generates the body of a promise handler function. The body contains
   the block returned by BODY-FN (called with argument ARG) followed
   by a return statement, which returns the expression returned by
   BODY-FN."

  (let ((*return-variable* nil)
        (*in-async* t))
    (multiple-value-call #'add-to-block (funcall body-fn arg))))

(defun use-expression (expression body-fn)
  "Calls the BODY-FN with an expression which references the value
   computed by EXPRESSION. BODY-FN should return two values a block
   and an expression.

   If EXPRESSION is synchronous it is passed directly to BODY-FN and
   the values returned by BODY-FN are returned directly.

   IF EXPRESSION is asynchronous the expression returned (second
   return value) is EXPRESSION with a then handler function attached
   to it. The handler function contains the block returned by BODY-FN
   followed by a statement which returns the value computed by the
   expression returned by BODY-FN. The block returned (first return
   value) is NIL."

  (match expression
    ((cons 'async expression)
     (let ((var (var-name)))
       (values
        nil
        (cons 'async (make-promise expression var (make-body-block body-fn var))))))

    (_ (funcall body-fn expression))))

(defun use-expressions (expressions body-fn)
  "Same as USE-EXPRESSION except that EXPRESSIONS is a list of
   expressions and BODY-FN is passed a list of expressions which
   reference each expression in EXPRESSIONS."

  (flet ((get-expression (expression)
             (match expression
               ((cons 'async expression)
                (values expression (var-name)))

               (_ expression))))

    (iter
      (for expression in expressions)

      (multiple-value-bind (expression var)
          (get-expression expression)
        (when var
          (collect expression into promise-args)
          (collect var into fn-args))

        (collect (or var expression) into body-args)

        (finally
         (return
           (if promise-args
               (values
                nil
                (cons 'async
                      (make-promise promise-args fn-args (make-body-block body-fn body-args))))
               (funcall body-fn body-args)
               )))))))

(defun value-expression? (thing)
  "Returns true if THING is a JS expression. Unlike EXPRESSIONP, this
   returns true if THING is a CONS where the CAR is the symbol ASYNC
   and EXPRESSIONP returns true for the CDR."

  (match thing
    ((cons 'async thing)
     (expressionp thing))

    (_ (expressionp thing))))


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
