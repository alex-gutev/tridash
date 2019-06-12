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

(defvar *operand-vars* nil
  "The operands list of the meta-node currently being compiled to
   CL.")


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

(defun tridash->cl-function (meta-node)
  "Returns a CL LAMBDA expression which is compiled from META-NODE."

  (let ((*operand-vars* (make-hash-map)))
    (flet ((add-operand (operand)
             (ensure-get operand *operand-vars* (gensym (mkstr operand)))))

      `(lambda ,(map #'add-operand (operands meta-node))
         ,(->
           (contexts meta-node)
           map-values
           first
           value-function
           tridash->cl)))))

(defun tridash->cl (expr)
  "Compiles the Tridash expression EXPR to a CL expression."

  (match expr
    ((list* operator operands)
     (tridash-functor->cl operator operands))

    ((sub-function- expression)
     (tridash->cl expression))

    ((node-link node)
     (ensure-get (name node) *operand-vars* (gensym (mkstr (name node)))))

    (_ expr)))


(defconstant +tridash-cl-functions+
  (alist-hash-map
   (symbol-mappings
    "if"
    "+"
    "-"
    "*"
    "/"
    "<"
    "<="
    ">"
    ">="
    "="
    '("!=" '/=)
    "and"
    "or"
    "not"
    '("int?" integerp)
    '("real?" floatp)
    '("string?" stringp)))

  "Map mapping Tridash meta-node identifiers to the corresponding CL
   function identifiers.")

(defgeneric tridash-functor->cl (operator operands)
  (:documentation
   "Compiles the Tridash functor expression (OPERATOR OPERANDS) to a
    CL expression."))

(defmethod tridash-functor->cl ((operator (eql 'if)) operands)
  (cons 'if operands))

(defmethod tridash-functor->cl ((operator (eql :object)) operands)
  "Compiles the :OBJECT expression to a CL expression that creates a
   `HASH-MAP'."

  (flet ((make-entry (pair)
           (destructuring-bind (key value) pair
             `(cons ',key ,(tridash->cl value)))))
    `(alist-hash-map (list ,@(map #'make-entry operands)))))

(defmethod tridash-functor->cl ((operator (eql :member)) operands)
  "Compiles the :MEMBER expression to a GET function expression."

  (destructuring-bind (object member) operands
    `(get ',member ,(tridash->cl object))))

(defmethod tridash-functor->cl ((meta-node external-meta-node) operands)
  "Generates code which invokes the externally defined meta-node
   META-NODE with operands OPERANDS. If the meta-node represents a
   special operator in *SPECIAL-OPERATORS* the appropriate
   TRIDASH-FUNCTOR->CL method is called otherwise the meta-node
   is treated as an ordinary function."


  (aif (get (name meta-node) +tridash-cl-functions+)
       (list* it (map #' tridash->cl operands))
       (error "External meta-node ~a not supported." meta-node)))

(defmethod tridash-functor->cl (meta-node operands)
  "Compiles the meta-node functor expression to a
   CALL-TRIDASH-META-NODE CL function expression."

  `(call-tridash-meta-node ,meta-node (list ,@(map #'tridash->cl operands))))
