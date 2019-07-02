;;;; functions.lisp
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

;;;; Compile value function expressions to JS code

(in-package :tridash.backend.js)


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

(defconstant +js-primitive-operators+
  (alist-hash-map
   (list
    (cons (id-symbol "and") "Tridash.and")
    (cons (id-symbol "or") "Tridash.or")
    (cons (id-symbol "not") '("!" . t))

    (cons (id-symbol "+") '("+" . t))
    (cons (id-symbol "-") '("-" . t))
    (cons (id-symbol "*") '("*" . t))
    (cons (id-symbol "/") '("/" . t))
    (cons (id-symbol "<") '("<" . t))
    (cons (id-symbol ">") '(">" . t))
    (cons (id-symbol "<=") '("<=" . t))
    (cons (id-symbol ">=") '(">=" . t))
    (cons (id-symbol "=") '("===" . t))
    (cons (id-symbol "!=") '("!==" . t))

    (cons (id-symbol "int") "Tridash.cast_int")
    (cons (id-symbol "real") "Tridash.cast_real")
    (cons (id-symbol "string") "Tridash.cast_string")

    (cons (id-symbol "int?") "Tridash.is_int")
    (cons (id-symbol "real?") "Tridash.is_real")
    (cons (id-symbol "string?") "Tridash.is_string")

    (cons (id-symbol "inf?") "Tridash.is_inf")
    (cons (id-symbol "NaN?") "Tridash.is_nan")))

  "Map mapping tridash primitive operators to their corresponding
   JavaScript primitive operators.")

(defvar *in-tail-position* t
  "Boolean flag for whether the expression, currently being compiled,
   occurs in tail position of the value function, currently being
   compiled.")

(defvar *return-variable* nil
  "The return variable in which the result computed by the expression,
   currently being compiled, should be stored. If NIL the result
   should be returned directly using a return statement.")

(defvar *thunk* nil
  "Boolean flag for whether the current expression should be compiled
   to a thunk.")

(defvar *resolve* nil
  "Boolean flag for whether the value of the current expression should
   be resolved before being returned.")


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

  (match (meta-node-id meta-node)
    ((cons name resolve-args?)
     (-<>> (map #'resolve-expression operands)
           (if resolve-args? <> operands)
           (make-js-call name)))

    (name
     (make-js-call name operands))))


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
                   (if (and *in-tail-position* (eq node meta-node))
                       (prog1
                           (js-block
                            (js-call "=" (js-array (map #'cdr op-vars)) (js-array operands))
                            (js-continue))
                         (setf tail-recursive-p t))
                       (meta-node-call node operands))))

          (let* ((body (make-function-body value-function #'get-input)))

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
    (labels ((get-input (link)
               (match link
                 ((eql :self)
                  (js-members "self" "node" "value"))

                 (_ (make-link-expression context link values-var))))

             (make-body (fn)
               (let ((*thunk* t))
                 (make-function-body fn #'get-input))))

      (with-slots (value-function) context
        (when value-function
          (js-lambda
           (list values-var)

           (let ((body (make-body value-function)))
             (list
              (js-var "self" "this")
              body))))))))

(defun make-link-expression (context link &optional (values-var "values"))
  "Creates an expression which references the node with `NODE-LINK'
   LINK linked to the context CONTEXT."

  (js-element values-var (dependency-index context (node-link-node link))))


(defvar *get-input* nil
  "Function which should return an expression that references the
   value of the dependency corresponding to the `NODE-LINK' object
   passed as an argument.")

(defvar *var-counter* 0
  "Variable identifier counter. Appended as a suffix to variables
   introduced in a function.")

(defvar *current-expression-group-index* nil
  "Index of the expression-group currently being compiled.")


(defun make-function-body (function *get-input*)
  "Generates the body of the value computation function
   FUNCTION. *GET-INPUT* is a function which should return an
   expression that references the value of the dependency
   corresponding to the `NODE-LINK' object passed as an argument."

  (flet ((make-expression-group (group)
           (destructuring-bind (group var thunk index) group
             (list
              (js-call "=" var thunk)
              (when (and (expression-group-save group) (not (in-meta-node?)))
                (-<> (js-member "self" "saved_values")
                     (js-element index)
                     (js-call "=" <> var)))))))

    (let* ((*var-counter* 0)
           (*expression-groups* (make-hash-map))
           (*thunk* t)
           (statements (make-statements function)))

      (list*
       (map (compose #'js-var #'first) (map-values *expression-groups*))
       (map-to 'list #'make-expression-group *expression-groups*)
       statements))))

(defun make-statements (fn)
  "Generates the statements making up the body of the value
   function/expression FN, the list of statements is returned. The
   list includes a statement which stores the result, computed by the
   expression, in *RETURN-VARIABLE*, or returns the result directly if
   *RETURN_VARIABLE* is NIL."

  (multiple-value-call #'add-to-block (make-expression fn :thunk *thunk*)))

(defun add-to-block (block expression)
  "Creates a new block which contains BLOCK and a statement which
   returns the result of EXPRESSION, generated using MAKE-RETURN. The
   list of the block's statements is returned. If EXPRESSION is equal
   to *RETURN-VARIABLE* the return expression/statement is not added
   to BLOCK."

  (list block
        (when (and expression (/= expression *return-variable*))
          (make-return expression))))


;;;; Compiling Intermediate Expressions

(defgeneric make-expression (expression &key &allow-other-keys)
  (:documentation
   "Generates code for a single intermediate expression.

    :RETURN-VARIABLE (defaults to *RETURN-VARIABLE*) is the name of
    the variable in which the result should be stored (NIL if it
    should be returned directly).

    :TAILP (defaults to *IN-TAIL-POSITION) is a flag which is true if
    EXPRESSION occurs in tail position.

    Returns two values: a block, which computes the result and an
    expression for referencing that value. If EXPRESSION is compiled
    to a single expression, the first return value is NIL and the
    second value is the expression. If :RETURN-VARIABLE is NIL and FN
    is not compiled to a single expression, the second return value is
    NIL as the block returned in the first value contains a return
    statement."))

(defmethod make-expression :around ((expression t) &key ((:return-variable *return-variable*) *return-variable*) ((:thunk *thunk*)))
  "Wraps the compiled expression in a THUNK if :THUNK is true."

  (let* ((*return-variable* (and (not *thunk*) *return-variable*)))

    (multiple-value-bind (block expression)
        (let ((*thunk* nil)) (call-next-method))
      (if (and *thunk* (or block (not (non-computing? expression))))
          (values nil (thunk block expression))
          (values block expression)))))

(defun non-computing? (expression)
  "Returns true if the JS expression EXPRESSION does not compute a new
   value but simply references an existing value.

   Variable references, anonymous functions and literal values are
   always considered non-computing. Member and element expressions are
   considered non-computing if the object and element expressions are
   non-computing. Function calls, new expressions, object and array
   literals are not considered non-computing."

  (match expression
    ((type (or js-call js-new js-object js-array))
     nil)

    ((js-member- object field)
     (and (non-computing? object)
          field))

    ((js-element- object element)
     (and (non-computing? object)
          (non-computing? element)))

    ((type expression) t)))

(defun resolve-expression (expression)
  "Creates an expression which resolves the value of EXPRESSION."

  (js-call +resolve-function+ expression))

(defun thunk (block expression)
  "Creates an expression which creates a thunk that executes BLOCK and
   returns the result of EXPRESSION."

  (let ((*return-variable* nil))
    (js-new +thunk-class+ (list (js-lambda nil (add-to-block block expression))))))


;;; Node References

(defmethod make-expression ((link node-link) &key)
  (values nil (funcall *get-input* link)))

(defmethod make-expression ((self (eql :self)) &key)
  (values nil (funcall *get-input* self)))


;;; Meta-Node Functor Expressions

(defmethod make-expression ((expression functor-expression) &key)
  (with-accessors ((meta-node functor-expression-meta-node)
                   (arguments functor-expression-arguments))
      expression

    (make-functor-expression meta-node arguments)))

(defgeneric make-functor-expression (operator arguments)
  (:documentation
   "Generates code for a functor expression consisting of OPERATOR
    applied to ARGUMENTS."))

(defmethod make-functor-expression ((meta-node external-meta-node) arguments)
  (match (name meta-node)
    ((eql (id-symbol "if"))
     (make-if-expression arguments))

    ((eql (id-symbol "member"))
     (make-member-expression arguments))

    (_
     (call-next-method))))

(defmethod make-functor-expression ((meta-node meta-node) arguments)
  (make-operands
   arguments
   (strict-arguments meta-node)
   (lambda (expressions)
     (let ((call (make-meta-node-call meta-node expressions)))
       (if (expressionp call)
           (values nil call)
           (values call nil))))))

(defmethod make-functor-expression (operator arguments)
  (make-operands
   (cons operator arguments)
   (list t)
   (lambda (expressions)
     (destructuring-bind (fn &rest args) expressions
       (values nil (make-js-call (resolve-expression fn) args))))))


;;; If Expressions

(defun make-if-expression (arguments)
  "Generates an if-block for an IF functor expression with arguments
   ARGUMENTS."

  (destructuring-bind (cond then else)
      arguments

    (let ((cond-var (var-name)))
      (multiple-value-bind (cond-block cond-expr)
          (make-expression cond :return-variable cond-var)

        (multiple-value-call
            #'combine-blocks

          (when cond-block
            (js-block (js-var cond-var) cond-block))

          (let ((then-block (make-statements then))
                (else-block (make-statements else)))

            (values
             (js-block
              (js-if (resolve-expression cond-expr)
                     (make-js-block then-block)
                     (make-js-block else-block)))

             *return-variable*)))))))


;;; Fail and Catch Expressions

(defmethod make-expression ((fail fail-expression) &key)
  (cond
    ((in-meta-node?)
     (values nil "null"))

    ;; Check whether inside expression group
    (*current-expression-group-index*
     (values nil (get-saved-value *current-expression-group-index*)))

    (t
     (values (js-block (js-throw (js-new +end-update-class+))) nil))))

(defmethod make-expression ((expr catch-expression) &key)
  "Generates a TRY-CATCH block for :CATCH expressions."

  (with-accessors ((try catch-expression-main)
                   (catch catch-expression-catch))
      expr

    (let ((try (let ((*resolve* t)) (make-statements try)))
          (catch (make-statements catch)))

      (values
       (js-block
        (js-catch try catch))

       *return-variable*))))


;;; Object Expressions

(defmethod make-expression ((object object-expression) &key)
  "Generates an expression which returns an object containing each
   field-value pair in OPERANDS."

  (with-accessors ((entries object-expression-entries)) object
    (let ((keys (map #'first entries))
          (values (map #'second entries)))

      (make-operands
       values
       (make-list (length values) :initial-element t)
       (lambda (values)
         (values
          nil
          (js-object (map #'list (map #'js-string keys) values))))))))

(defun make-member-expression (arguments)
  "Generates an expression which returns the value of a particular
   subnode (field) of an object node."

  (destructuring-bind (object member) arguments

    (let ((object-var (var-name)))
      (multiple-value-bind (object-block object-expr)
          (make-expression object :return-variable object-var)

        (multiple-value-call #'combine-blocks

          (when object-block
            (js-block (js-var object-var) object-block))

          (values nil (js-element (resolve-expression object-expr) (js-string member))))))))


;;; Expression Groups

(defmethod make-expression ((group expression-group) &key)
  (with-accessors ((expression expression-group-expression)
                   (count expression-group-count)
                   (save expression-group-save))
      group

    (if (or (> count 1) (and (not (in-meta-node?)) save))
        (make-expression-group group)
        (make-expression expression))))

(defun make-expression-group (group)
  "Generates code which computes the value of the `EXPRESSION-GROUP'
   GROUP and stores the resulting expression in
   *EXPRESSION-GROUPS*. Returns the variable which references the
   value of the `EXPRESSION-GROUP'."

  (with-accessors ((expression expression-group-expression)
                   (save expression-group-save))
      group

    (aif (get group *expression-groups*)
         (values nil (car it))

         (let* ((*current-expression-group-index* (length *expression-groups*))
                (thunk (nth-value 1 (make-expression expression :thunk t))))

           (values
            nil
            (aprog1 (var-name)
              (->>
               (list it thunk *current-expression-group-index*)
               (setf (get group *expression-groups*)))))))))

(defun in-meta-node? ()
  "Returns true if currently compiling a meta-node function."

  (meta-node? *current-node*))

(defun get-saved-value (index)
  "Returns an expression which references the saved previous value of
   the expression-group with index INDEX."

  (js-element (js-member "self" "saved_values") index))


;;; Meta-Node References

(defmethod make-expression ((ref meta-node-ref) &key)
  "Generates an expression which returns a function that invokes the
   meta-node referenced by REF."

  (with-struct-slots meta-node-ref- (node outer-nodes)
      ref

    (if outer-nodes
        (make-operands
         outer-nodes
         nil
         (lambda (expressions)
           (values
            nil
            (-<> (js-call "Array.prototype.slice.call" "arguments")
                 (js-member "concat")
                 (js-call (js-array expressions))
                 (js-call (js-member (meta-node-id node) "apply") <>)
                 (js-return)
                 list
                 (js-lambda nil <>)))))

        (values nil (meta-node-id node)))))


;;; Literal Values

(defmethod make-expression ((string string) &key)
  (values nil (js-string string)))

(defmethod make-expression (literal &key)
  (values nil literal))

(defmethod make-expression ((null null) &key)
  (values nil "null"))


;;;; Arguments

(defun make-operands (operands strict-arguments body-fn)
  "Generates code which computes the value of each operand. BODY-FN is
   a function, which is called to generate the code which makes use of
   the operands' values. It is called with one argument: a list of JS
   expressions which reference the values of the operands, and should
   return two values: a block and an expression.

   Two values are returned, a block which computes the values of the
   operands, and an expression."

  (iter
    (for (strict? . strict-args) initially strict-arguments then strict-args)
    (for operand in operands)
    (for op-var = (var-name))

    (multiple-value-bind (op-block op-expr)
        (make-expression operand :return-variable op-var :thunk (not strict?))

      (when op-block
        (collect (js-var op-var) into block-statements)
        (collect op-block into block-statements))

      (collect op-expr into expressions))

    (finally
     (return
       (multiple-value-call
           #'combine-blocks
         (and block-statements (make-js-block block-statements))
         (funcall body-fn expressions))))))


;;;; Returning Values

(defun make-return (expression)
  "Generates a statement which returns the value computed by the JS
   expression EXPRESSION. If *RETURN-VARIABLE* is non-NIL an
   assignment statement, which stores the value computed by EXPRESSION
   into the variable named by *RETURN-VARIABLE*, is generated
   otherwise a return statement is generated."

  (let ((expression (if *resolve* (resolve-expression expression) expression)))
    (if *return-variable*
        (js-call "=" *return-variable* expression)
        (js-return expression))))

(defun var-name (&optional (prefix "var"))
  "Returns a new unused variable name."
  (mkstr prefix (incf *var-counter*)))


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

(defmethod strip-redundant ((catch js-catch) &key)
  (js-catch
   (strip-redundant (js-catch-try catch))
   (strip-redundant (js-catch-catch catch))))


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
