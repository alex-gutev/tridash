;;;; functions.lisp
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

;;;; Compile value function expressions to JS code

(in-package :tridash.backend.js)


;;;; Symbol Names

(define-constant +thunk-class+ (mkstr +tridash-prefix+ "Thunk")
  :test #'equal
  :documentation "Thunk constructor function name.")

(define-constant +end-update-class+ (mkstr +tridash-prefix+ "EndUpdate")
  :test #'equal
  :documentation "EndUpdate class name.")


;;;; Utility Functions

;;; Access Node Expressions

(defun access-node (node)
  "Returns an expression which references NODE."

  (mkstr "node" (node-index node)))

(defun node-index (node)
  "Returns the index of NODE within the node table variable."

  (ensure-get node *node-ids* (length *node-ids*)))

(defparameter *node-path* #'access-node
  "Function which takes a node as an argument and returns an
   expression which references that node.")

(defun node-path (node)
  "Returns an expression which references NODE, by calling the
   function bound to *NODE-PATH*."

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

(defconstant +js-primitive-operators+
  (alist-hash-map
   (list
    (cons (id-symbol "and") "&&")
    (cons (id-symbol "or") "||")
    (cons (id-symbol "not") "!")

    (cons (id-symbol "int") "parseInt")
    (cons (id-symbol "real") "parseFloat")
    (cons (id-symbol "string") "String")

    (cons (id-symbol "int?") "Tridash.isInteger")
    (cons (id-symbol "number?") "Tridash.isReal")
    (cons (id-symbol "string?") "Tridash.isString")

    (cons (id-symbol "inf?") "Tridash.isInf")
    (cons (id-symbol "NaN?") "Tridash.isNaN")

    (cons (id-symbol "+") "+")
    (cons (id-symbol "-") "-")
    (cons (id-symbol "*") "*")
    (cons (id-symbol "/") "/")
    (cons (id-symbol "<") "<")
    (cons (id-symbol ">") ">")
    (cons (id-symbol "<=") "<=")
    (cons (id-symbol ">") ">")
    (cons (id-symbol "!=") "!==")
    (cons (id-symbol "=") "===")))

  "Map mapping tridash primitive operators to their corresponding
   JavaScript primitive operators.")


(defun meta-node-id (meta-node)
  "Returns the global meta-node function/operator identifier of
   META-NODE. If META-NODE is a `META-NODE' object either a new
   identifier is created or the existing identifier is returned. If
   META-NODE is an `EXTERNAL-META-NODE' the JavaScript
   function/operator name corresponding to the name of the meta-node,
   in +JS-PRIMITIVE-OPERATORS+, the node's :PUBLIC-NAME attribute, or
   the name of the meta-node itself is returned."

  (etypecase meta-node
    (external-meta-node
     (or (get (name meta-node) +js-primitive-operators+)
         (attribute :public-name meta-node)
         (name meta-node)))

    (meta-node
     (ensure-get meta-node *meta-node-ids*
                 (mkstr "metanode" (length *meta-node-ids*))))))

(defun meta-node-call (meta-node operands)
  "Returns a `JS-CALL' expression for the meta-node META-NODE with
   operands OPERANDS. If the meta-node is asynchronous a CONS is
   returned, with the CAR being the symbol ASYNC and the CDR being the
   meta-node call expression."

  (let ((call (make-js-call (meta-node-id meta-node) operands)))
    (if (async-meta-node? meta-node)
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


;;;; Meta-Node Functions

(defun create-function-meta-node (meta-node)
  "Generates the value-function of the `meta-node' META-NODE. This can
   only be used if META-NODE has no subnodes, other than its operand
   nodes."

  (with-slots (contexts operands) meta-node
    (let ((context (cdr (first contexts)))
          (op-vars (make-operand-ids operands))
          (tail-recursive-p nil))

      (with-slots (value-function) context
        (labels ((get-input (link)
                   (cdr (assoc (name (node-link-node link)) op-vars :test #'equal)))

                 (make-meta-node-call (node operands)
                   (if (and *in-tail-position* (not *in-async*) (eq node meta-node))
                       (prog1
                           (js-block
                            (js-call "=" (js-array (map #'cdr op-vars)) (js-array operands))
                            (js-continue))
                         (setf tail-recursive-p t))
                       (meta-node-call node operands))))

          (let* ((*meta-node-call* #'make-meta-node-call)
                 (body (make-function-body value-function #'get-input)))

            (list
             (js-function
              (meta-node-id meta-node)
              (map #'cdr op-vars)

              (list
               (let ((*output-code* (make-code-array)))
                 (create-meta-nodes (meta-nodes (definition meta-node)))
                 *output-code*)

               (if tail-recursive-p
                   (js-while "true" (js-block body))
                   body)))

             (store-in-public-nodes meta-node (meta-node-id meta-node)))))))))

(defun create-async-meta-node (meta-node)
  "Generates the value-function of the `meta-node'. Unlike
   CREATE-FUNCTION-META-NODE, the function generated does not directly
   return the computed value, instead it creates the meta-node's
   subgraph, dispatches the operand values to the operand nodes and
   returns a promise which is resolved when the value of the meta-node
   is computed. This function can be used to compile any meta-node."

  (with-slots (definition operands) meta-node
    (symbol-macrolet ((promise-var "promise")
                      (node-table-var "node_table"))

      (let ((*node-path* (lambda (node)
                           (js-element node-table-var (node-index node))))
            (*lazy-nodes* (find-lazy-nodes definition))
            (*node-ids* (make-hash-map))
            (*initial-values* nil))

        (labels
            ((get-operand (operand)
               "Returns an expression accessing the operand node named
                by OPERAND."

               (node-path (find operand (nodes definition) :key #'name)))

             (operand-node-var (operand)
               "Returns a JS array with two elements: the operand node
                and the variable storing the operand's value."

               (js-array (list (get-operand (car operand))
                               (cdr operand))))

             (make-meta-node-call (node operands)
               "Generates an expression which invokes the meta-node
                NODE with operands OPERANDS. If in tail position a
                tail call is generated."

               (if (and (eq *current-node* meta-node)
                        *in-tail-position*
                        (async-meta-node? node))
                   (make-tail-call node operands)
                   (meta-node-call node operands)))

             (make-tail-call (node operands)
               "Generates a tail call for the meta-node NODE with
                operands OPERANDS. The tail call reuses the existing
                promise and if it is a self-call the existing node
                table is reused."

               (js-block
                (->> (when (= node meta-node)
                       (list node-table-var))

                     (append operands (list promise-var))
                     (meta-node-call node)
                     (cdr))
                (js-throw (js-new +end-update-class+))))

             (create-update-fn ()
               "Generates the update value function of the value
                node."

               (let ((path (node-path meta-node)))
                 (append-code
                  (js-call
                   "="
                   (js-member path "update_value")
                   (js-lambda
                    (list "value")
                    (list
                     (js-call (js-member promise-var "resolve") "value"))))))))


          (let ((op-vars (make-operand-ids operands))
                (node-creation-code (make-code-array)))

            (let ((*output-code* node-creation-code)
                  (*meta-node-call* #'make-meta-node-call))

              (append-code (js-call "=" node-table-var (js-array)))

              (generate-code definition)
              (create-update-fn))

            (list
             (js-function
              (meta-node-id meta-node)
              (append (map #'cdr op-vars)
                      (list (js-call "=" promise-var (js-new (js-member +tridash-namespace+ "ValuePromise")))
                            node-table-var))

              (list
               ;; Wrap node table creation in an if which checks
               ;; whether a node table was provided as an argument.
               (js-if
                (js-call "===" node-table-var "undefined")
                (make-js-block node-creation-code))

               (js-call
                (js-member +tridash-namespace+ "set_values")
                (js-array (append (map #'operand-node-var op-vars) *initial-values*)))

               (js-return (js-member "promise" "promise"))))

             (ensure-list (store-in-public-nodes meta-node (meta-node-id meta-node))))))))))

(defun make-operand-ids (operands)
  "Generates variable names for each operand. Returns an association
   list where each key is the operand and the associated value is the
   variable name."

  (loop
     for operand in operands
     for i = 0 then (1+ i)
     collect (cons operand (mkstr "a" i))))

(defun store-in-public-nodes (node expr)
  "If NODE has a :PUBLIC-NAME attribute returns an assignment
   expression which stores EXPR in 'Tridash.nodes' under the key which
   value of the :PUBLIC-NAME attribute"

  (awhen (attribute :public-name node)
    (-<> (js-member +tridash-namespace+ "nodes")
         (js-element (js-string it))
         (js-call "=" <> expr))))


;;;; Node Compute Functions

(defun create-compute-function (context)
  "Generates the value computation function of CONTEXT. The anonymous
   function expression is returned."

  (symbol-macrolet ((values-var "values"))
    (let ((uses-old-value nil))
      (labels ((get-input (link)
                 (if (= (node-link-node link) :self)
                     (prog1
                         (if (lazy-node? context)
                             (js-call "old_value")
                             "old_value")
                       (setf uses-old-value t))

                     (make-link-expression context link values-var)))

               (make-body (fn)
                 (alet (make-function-body fn #'get-input)
                   ;; If NODE should be evaluated lazily, wrap the compute
                   ;; function in a thunk and return it.
                   (if (lazy-node? context)
                       (list (js-return (js-call +thunk-class+ (js-lambda nil it))))
                       it))))

        (with-slots (value-function) context
          (when value-function
            (js-lambda
             (list values-var)

             (let ((body (make-body value-function)))
               (if uses-old-value
                   (list
                    (js-var "old_value" (js-member "this" "value"))
                    body)
                   body)))))))))

(defun make-link-expression (context link &optional (values-var "values"))
  "Creates an expression which references the node with `NODE-LINK'
   LINK linked to the context CONTEXT."

  (with-accessors ((link-node node-link-node)) link
    (flet ((make-ref (node)
             (js-element values-var (dependency-index context node))))

      (match link-node
        ((cons 'async node)
         ;; Linked node is lazy => Evaluate the thunk.
         (cons 'async (js-call (make-ref node))))

        (_ (make-ref link-node))))))

(defvar *get-input* nil
  "Function which should return an expression that references the
   value of the dependency corresponding to the `NODE-LINK' object
   passed as an argument.")

(defvar *var-counter* 0
  "Variable identifier counter. Appended as a suffix to variables
   introduced in a function.")

(defvar *sub-functions* nil
  "Set of sub-function expressions for which code has already been
  generated. The mapped value is an expression (a variable name) with
  which the value of the expression can be accessed.")

(defun make-function-body (function *get-input*)
  "Generates the body of the value computation function
   FUNCTION. *GET-INPUT* is a function which should return an
   expression that references the value of the dependency
   corresponding to the `NODE-LINK' object passed as an argument."

  (let ((*var-counter* 0)
        (*sub-functions* (make-hash-map)))
    (make-statements function)))

(defun make-statements (fn)
  "Generates the statements making up the body of the value
   function/expression FN, the list of statements is returned. The
   list includes a statement which stores the result, computed by the
   expression, in *RETURN-VARIABLE*, or returns the result directly if
   *RETURN_VARIABLE* is NIL."

  (multiple-value-call #'add-to-block (make-expression fn)))

(defun add-to-block (block expression)
  "Creates a new block which contains BLOCK and a statement which
   returns the result of EXPRESSION, generated using MAKE-RETURN. The
   list of the block's statements is returned.

   The second return value is true if EXPRESSION is an asynchronous
   expression."

  (flet ((get-expression (expression)
           "If EXPRESSION is a CONS with the CAR equal to the symbol
            ASYNC, the expression stored in the CDR is
            returned. Otherwise EXPRESSION is returned as is."

           (match expression
             ((cons 'async expression)
              (values expression t))

             (_ expression))))

    (multiple-value-bind (expression async?)
        (get-expression expression)

      (values
       (list block
             (when expression
               (make-return (get-expression expression))))

       async?))))

(defun make-expression (fn &key ((:return-variable *return-variable*) *return-variable*) ((:tailp *in-tail-position*) *in-tail-position*))
  "Generates the code for a single expression
   FN. :RETURN-VARIABLE (bound to *RETURN-VARIABLE*) is the name of
   the variable in which the result should be stored (NIL if it should
   be returned directly). :TAILP (bound to *IN-TAIL-POSITION*) is a
   flag for whether FN occurs in tail position.

   Returns two values: a block, which computes the result and an
   expression for referencing that value. If FN is compiled to a
   single expression, the first return value is NIL and the second
   value is the expression. If :RETURN-VARIABLE is NIL and FN is not
   compiled to a single expression, the second return value is NIL as
   the block returned in the first value contains a return statement."

  (match fn
    ((list* operator operands)
     (make-operator-expression operator operands))

    ((sub-function- expression count)
     (if (> count 1)
         (aif (get fn *sub-functions*)
              (values nil it)
              (multiple-value-return (blk expr) (make-sub-function expression)
                (declare (ignore blk))
                (setf (get fn *sub-functions*) expr)))
         (make-expression expression)))

    ((type node-link)
     (values nil (funcall *get-input* fn)))

    ((eq :fail)
     (values (js-block (js-throw (js-new +end-update-class+))) nil))

    (_
     (values nil (make-literal fn)))))

(defun make-sub-function (fn)
  "Generates code which computes the value of the sub-function
   expression and stores it in a variable, which is returned as the
   expression (second return value)."

  (let ((var (var-name)))
    (multiple-value-bind (blk expr) (make-expression fn :return-variable var :tailp nil)
      (values
       (cond
         ((or (null expr) (= expr var))
          (js-block (js-var var) blk))

         (t
          (js-block blk (js-var var expr))))
       var))))


;;;; Literal Values

(defgeneric make-literal (literal)
  (:documentation
   "Generates a JS expression for the literal value LITERAL.")

  (:method (literal)
    literal)

  (:method ((literal string))
    (js-string literal)))


;;;; Special Operator Expressions

(defgeneric make-operator-expression (operator operands)
  (:documentation
   "Generates the code for the value expression with operator OPERATOR
    and operands OPERANDS. Returns two values: a block which computes
    the value of the expression (NIL if the expression can be compiled
    to a single JS expression) and an expression for referencing the
    value computed by the expression."))


;;; Conditions

(defmethod make-operator-expression ((operator (eql 'if)) operands)
  "Generates an IF block for a conditional expression with condition
   COND, if-true expression THEN and else expression ELSE."

  (destructuring-bind (cond then else) operands
    (let ((cond-var (var-name)))
      (multiple-value-bind (cond-block cond-expr)
          (make-expression cond :return-variable cond-var :tailp nil)

        (multiple-value-call
            #'combine-blocks

          (when cond-block
            (js-block (js-var cond-var) cond-block))

          (use-expression
           cond-expr

           (lambda (cond-expr)
             (multiple-value-bind (then-block then-async?)
                 (make-statements then)

               (multiple-value-bind (else-block else-async?)
                   (make-statements else)

                 (values
                  (js-block
                   (js-if cond-expr
                          (make-js-block then-block)
                          (make-js-block else-block)))

                  (if (and *return-variable* (or then-async? else-async?))
                      (cons 'async
                            (js-call (js-member "Promise" "resolve") *return-variable*))
                      *return-variable*)))))))))))


;;; Objects

(defmethod make-operator-expression ((operator (eql :object)) operands)
  "Generates an expression which returns an object containing each
   field-value pair in OPERANDS."

  (let ((keys (map #'first operands))
        (values (map #'second operands)))

    (make-operands
     values
     (lambda (values)
       (values
        nil
        (js-object (map #'list (map #'js-string keys) values)))))))

(defmethod make-operator-expression ((operator (eql :member)) operands)
  "Generates an expression which returns the value of a particular
   subnode (field) of an object node."

  (destructuring-bind (object member) operands
    (let ((object-var (var-name)))
      (multiple-value-bind (object-block object-expr)
          (make-expression object :return-variable object-var :tailp nil)

        (multiple-value-call #'combine-blocks

          (when object-block
            (js-block (js-var object-var) object-block))

          (use-expression
           object-expr

           (lambda (object)
             (values nil (js-element object (js-string member))))))))))


;;; Meta-Node Expressions

(defconstant +special-operators+
  (alist-hash-map
   (list (cons (id-symbol "if") 'if)))

  "Map mapping names of externally defined meta-nodes to special
   operator symbols for which there is a MAKE-OPERATOR-EXPRESSION
   method.")

(defmethod make-operator-expression ((meta-node external-meta-node) operands)
  "Generates code which invokes the externally defined meta-node
   META-NODE with operands OPERANDS. If the meta-node represents a
   special operator in *SPECIAL-OPERATORS* the appropriate
   MAKE-OPERATOR-EXPRESSION method is called otherwise the meta-node
   is treated as an ordinary function."

  (aif (get (name meta-node) +special-operators+)
       (make-operator-expression it operands)
       (call-next-method)))


(defmethod make-operator-expression (meta-node operands)
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

   If all operands are synchronous expressions: the block returned
   contains the statements of the block which computes the operands'
   values and the block returned by BODY-FN. The expression returned
   by BODY-FN is returned.

   If at least one of the operands is an asynchronous expression: the
   block returned is the block which computes the operand values, and
   the expression is a Promise with a then handler function that
   contains the block returned by BODY-FN followed by a return
   statement, which returns the expression returned by BODY-FN."

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


;;;; Returning Values

(defun make-return (expression)
  "Generates a statement which returns the value computed by the JS
   expression EXPRESSION. If *RETURN-VARIABLE* is non-NIL an
   assignment statement, which stores the value computed by EXPRESSION
   into the variable named by *RETURN-VARIABLE*, is generated
   otherwise a return statement is generated."

  (if *return-variable*
      (js-call "=" *return-variable* expression)
      (js-return expression)))

(defun var-name (&optional (prefix "var"))
  "Returns a new unused variable name."
  (mkstr prefix (incf *var-counter*)))


;;;; Promises for Asynchronous Expressions

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


;;;; Combining Blocks

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


;;;; Using Expressions

(defun use-expression (expression body-fn)
  "Calls BODY-FN with an expression which references the value
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
               (->> (make-body-block body-fn body-args)
                    (make-promise promise-args fn-args)
                    (cons 'async)
                    (values nil))
               (funcall body-fn body-args))))))))

(defun value-expression? (thing)
  "Returns true if THING is a JS expression. Unlike EXPRESSIONP, this
   returns true if THING is a CONS where the CAR is the symbol ASYNC
   and EXPRESSIONP returns true for the CDR."

  (match thing
    ((cons 'async thing)
     (expressionp thing))

    (_ (expressionp thing))))


;;;; Removing Redundant AST Nodes

(defgeneric strip-redundant (statement &key &allow-other-keys)
  (:documentation
   "Remove redundant AST nodes such as nested blocks which have no
    semantic significance and statement lists.

    If the :STRIP-BLOCK keyword argument is true and STATEMENT is a
    `JS-BLOCK', its statements are returned."))


;;; Expressions

(defmethod strip-redundant ((call js-call) &key)
  (make-js-call
   (strip-redundant (js-call-operator call))
   (mapcar #'strip-redundant (js-call-operands call))))

(defmethod strip-redundant ((new js-new) &key)
  (js-new
   (strip-redundant (js-new-operator new))
   (mapcar #'strip-redundant (js-new-operands new))))

(defmethod strip-redundant ((element js-element) &key)
  (js-element
   (strip-redundant (js-element-object element))
   (strip-redundant (js-element-element element))))

(defmethod strip-redundant ((member js-member) &key)
  (js-member
   (strip-redundant (js-member-object member))
   (js-member-field member)))


;;; Object and Array Literals

(defmethod strip-redundant ((array js-array) &key)
  (js-array (mapcar #'strip-redundant (js-array-elements array))))

(defmethod strip-redundant ((object js-object) &key)
  (flet ((strip-redundant (field)
           (destructuring-bind (key value) field
             (list (strip-redundant key) (strip-redundant value)))))
    (js-object (mapcar #'strip-redundant (js-object-fields object)))))


;;; Blocks

(defmethod strip-redundant ((if js-if) &key)
  (js-if (strip-redundant (js-if-condition if))
         (strip-redundant (js-if-then if))
         (strip-redundant (js-if-else if))))

(defmethod strip-redundant ((while js-while) &key)
  (js-while (strip-redundant (js-while-condition while))
            (strip-redundant (js-while-body while))))


(defmethod strip-redundant ((block js-block) &key strip-block)
  (let ((statements (strip-redundant (js-block-statements block))))
    (cond
      ((null (rest statements))
       (first statements))

      ((or strip-block (null statements))
       statements)

      (t (make-js-block statements)))))

(defmethod strip-redundant ((statements list) &key)
  (mappend (compose #'ensure-list (rcurry #'strip-redundant :strip-block t))
           statements))

(defmethod strip-redundant ((statements sequence) &key)
  (if (stringp statements)
      statements
      (strip-redundant (coerce statements 'list))))

(defmethod strip-redundant ((function js-function) &key)
  (js-function
   (js-function-name function)
   (mapcar #'strip-redundant (js-function-arguments function))
   (strip-redundant (js-function-statements function))))


;;; Statements

(defmethod strip-redundant ((return js-return) &key)
  (js-return (strip-redundant (js-return-value return))))

(defmethod strip-redundant ((var js-var) &key)
  (js-var (js-var-var var)
          (strip-redundant (js-var-value var))))

(defmethod strip-redundant ((throw js-throw) &key)
  (js-throw (strip-redundant (js-throw-expression throw))))


;;; Other

(defmethod strip-redundant (thing &key)
  thing)
