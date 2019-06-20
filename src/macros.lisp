;;;; macros.lisp
;;;;
;;;; Tridash Programming Language.
;;;; Copyright (C) 2019  Alexander Gutev
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

;;;; Implementation of user-defined macros.

(in-package :tridash.frontend)

(in-readtable lol-syntax)


(defvar *operand-vars* nil
  "The operands list of the meta-node currently being compiled to
   CL.")

(defvar *current-meta-node* nil
  "The meta-node currently being compiled to CL.")


;;;; Macro Attributes

(defmethod process-attribute (node (attribute (eql (id-symbol "MACRO"))) value)
  "Sets the internal :MACRO-FUNCTION attribute to a function that
   compiles the META-NODE to a CL function, calls it and calls
   PROCESS-DECLARATION on the result."

  (when (bool-value value)
    (setf (node-macro-function node)
          (make-macro-function node))))


;;;; Macro Function

(defun make-macro-function (meta-node)
  "Creates the macro-node function, which compiles META-NODE to a CL
   function, calls the function and processes the result."

  (lambda (operator operands table)
    (declare (ignore operator))
    (-> (call-tridash-meta-node meta-node operands)
        (process-declaration table :level *level*))))


;;;; Compiling CL function from Meta-Node

(defun call-tridash-meta-node (meta-node args)
  "Calls the meta-node META-NODE with arguments ARGS. If the meta-node
   has been compiled to a CL function, the function is called
   otherwise it is compiled to a CL function by
   COMPILE-META-NODE-FUNCTION."

  (apply
   (or (attribute 'cl-function meta-node)
       (compile-meta-node-function meta-node))
   args))

(defun compile-meta-node-function (meta-node)
  "Compiles the META-NODE meta-node to a CL function. Stores the
   compiled CL function in the :CL-FUNCTION attribute."

  (build-meta-node-graph meta-node (node-table *global-module-table*))
  (finish-build-meta-node meta-node)

  (setf (meta-node-cl-function meta-node)
        (compile nil (tridash->cl-function meta-node))))


(defun meta-node-cl-function (meta-node)
  "Returns the compiled CL function of the meta-node."

  (get :cl-function (attributes meta-node)))

(defun (setf meta-node-cl-function) (fn meta-node)
  "Sets the compiled CL function of META-NODE to FN."

  (setf (get :cl-function (attributes meta-node)) fn))


;;;; Compiling Tridash Expressions to CL

(defconstant +tridash-recur-tag+ 'recur
  "TAGBODY tag for tail-recursive self calls.")

(defun tridash->cl-function (meta-node)
  "Returns a CL LAMBDA expression which is compiled from META-NODE."

  (let ((*current-meta-node* meta-node)
        (*operand-vars* (make-hash-map)))

    (flet ((add-operand (operand)
             (ensure-get operand *operand-vars* (gensym (mkstr operand)))))

      `(lambda ,(map #'add-operand (operands meta-node))
         (block nil
           (tagbody
              ,+tridash-recur-tag+
              (return
                ,(->
                  (contexts meta-node)
                  map-values
                  first
                  value-function
                  (tridash->cl :tail-position-p t)))))))))


(defconstant +tridash-cl-functions+
  (alist-hash-map
   (symbol-mappings
    "if"
    "+" "-" "*" "/"
    "<" "<=" ">" ">=" "=" '("!=" /=)
    "and" "or" "not"
    '("int?" integerp)
    '("real?" floatp)
    '("string?" stringp)

    "cons" "list"))

  "Map mapping Tridash meta-node identifiers to the corresponding CL
   function identifiers.")

(defconstant +fail-catch-tag+ 'evaluate-fail
  "The CATCH tag symbol for `CATCH-EXPRESSIONS' which are compiled to
   CL CATCH expressions.")


(defgeneric tridash->cl (expression &key &allow-other-keys)
  (:documentation
   "Compiles the Tridash expression EXPR to a CL expression. If the
    :TAIL-POSITION-P argument is provided and is true, then EXPRESSION
    appears in tail position."))

(defmethod tridash->cl ((link node-link) &key)
  "Returns the variable corresponding to the linked node by looking up
   the node's identifier in *OPERAND-VARS*"

  (with-slots (name) (node-link-node link)
    (ensure-get name *operand-vars* (gensym (mkstr name)))))


(defmethod tridash->cl ((functor functor-expression) &key tail-position-p)
  "If the operator of the functor is an `EXTERNAL-META-NODE',
   generates a CL expression which calls the corresponding CL
   function, found by looking up the name of the meta-node in
   +TRIDASH-CL-FUNCTIONS+. If there is no corresponding CL function an
   error condition is signalled.

   If the operator is a `META-NODE', generates a
   CALL-TRIDASH-META-NODE expression."

  (with-struct-slots functor-expression- (meta-node arguments)
      functor

    (ematch meta-node
      ((external-meta-node name)
       (aif (get name +tridash-cl-functions+)
            (call-external-meta-node it arguments :tail-position-p tail-position-p)
            (error "External meta-node ~a not supported." meta-node)))

      ((guard (and (type meta-node) (eq *current-meta-node*))
              tail-position-p)
       (make-tail-call arguments))

      ((type meta-node)
       `(call-tridash-meta-node ,meta-node (list ,@(map #'tridash->cl arguments)))))))

(defun call-external-meta-node (name arguments &key tail-position-p)
  "Generates a CL function expression with operator NAME and ARGUMENTS
   being the TRIDASH expressions which are the
   arguments. TAIL-POSITION-P should be true if the expression appears
   in tail position."

  (case name
    (if
     (destructuring-bind (cond then &optional else) arguments
       `(if ,(tridash->cl cond)
            ,(tridash->cl then :tail-position-p tail-position-p)
            ,(tridash->cl else :tail-position-p tail-position-p))))

    ((and or)
     `(,name ,@(map #'tridash->cl (butlast arguments))
             ,(tridash->cl (last arguments) :tail-position-p tail-position-p)))

    (otherwise
     (list* name (map #'tridash->cl arguments)))))

(defun make-tail-call (arguments)
  "Generates a tail self call with arguments ARGUMENTS."

  `(progn
     (psetf ,@(mappend #2`(,(get a1 *operand-vars*) ,(tridash->cl a2))
                       (operands *current-meta-node*) arguments))
     (go ,+tridash-recur-tag+)))



(defmethod tridash->cl ((if if-expression) &key tail-position-p)
  (with-struct-slots if-expression- (condition then else)
      if

    `(if ,(tridash->cl condition)
         ,(tridash->cl then :tail-position-p tail-position-p)
         ,(tridash->cl else :tail-position-p tail-position-p))))

(defmethod tridash->cl ((object object-expression) &key)
  "Generates a CL expression that creates a `HASH-MAP'."

  (flet ((make-entry (pair)
           (destructuring-bind (key value) pair
             `(cons ',key ,(tridash->cl value)))))

    `(alist-hash-map (list ,@(map #'make-entry (object-expression-entries object))))))

(defmethod tridash->cl ((member member-expression) &key)
  "Generates a GET CL function expression."

  (with-struct-slots member-expression- (object key)
      member

    `(get ',key ,(tridash->cl object))))

(defmethod tridash->cl ((expr catch-expression) &key tail-position-p)
  "Generates a CL CATCH expression with the tag symbol given by
   +FAIL-CATCH-TAG+. If the CATCH expression returns the catch tag
   identifier, the expression in the `CATCH' slot is evaluate."

  (with-struct-slots catch-expression- (main catch)
      expr

    (with-gensyms (result)
      `(let ((,result (catch ',+fail-catch-tag+ ,(tridash->cl main))))
         (if (eq ,result ',+fail-catch-tag+)
             ,(tridash->cl catch :tail-position-p tail-position-p)
             ,result)))))

(defmethod tridash->cl ((fail fail-expression) &key)
  "Generates a CL THROW expression which throws the value of
   +FAIL-CATCH-TAG+."

  `(throw ',+fail-catch-tag+ ',+fail-catch-tag+))

(defmethod tridash->cl ((group expression-group) &key tail-position-p)
  (tridash->cl (expression-group-expression group) :tail-position-p tail-position-p))

(defmethod tridash->cl (literal &key)
  (match literal
    ((or (type number) (type string))
     literal)

    (_ `',literal)))


;;;; Macro-Writing

(defmethod process-functor ((operator (eql +quote-operator+)) args table)
  "Returns the raw argument unprocessed."

  (declare (ignore table))

  (match-syntax (operator any) args
    ((list thing)
     thing)))
