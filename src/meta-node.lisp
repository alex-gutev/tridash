;;;; meta-node.lisp
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

;;;; `meta-node' class definition.

(in-package :tridash.frontend)

(in-readtable cut-syntax)


(defclass meta-node (node)
  ((operands
    :initarg :operands
    :initform nil
    :accessor operands
    :documentation
    "List containing the symbols naming the meta-node's operands.")

   (outer-nodes
    :initform (make-hash-map)
    :accessor outer-nodes
    :documentation
    "Map of nodes (found in outer node tables) referenced from the
     meta-node's definition. Each key is the outer `NODE' object with
     the corresponding value being the name of the local node to
     which it is bound.")

   (meta-node-references
    :initform (make-hash-set)
    :accessor meta-node-references
    :documentation
    "Set of meta-nodes of which there are instances in the
     meta-node's definition.")

   (definition
    :initarg :definition
    :initform nil
    :accessor definition
    :documentation
    "The graph corresponding to the body of the meta-node. Prior to
     the graph being built the list of node declarations, making up
     the body, are stored in this slot. After the meta-node's
     definition is built, this slot contains a `FLAT-NODE-TABLE'
     containing the nodes in the meta-node's body.")

   (instances
    :initform (make-hash-set)
    :accessor instances
    :documentation
    "Set of the meta-node's instances, stored in `INSTANCE'
     objects."))

  (:documentation
   "Stores the definition of a meta-node."))

(defclass external-meta-node (meta-node)
  ()

  (:documentation
   "Node stub for a meta-node which is defined externally."))


;;; Instances

(defstruct (instance (:constructor instance (node context meta-node expression)))
  "Stores information about a meta-node instance.

   NODE and CONTEXT are the node, and corresponding context, in which
   the instance is contained.

   META-NODE is the meta-node in which NODE is contained. NIL if NODE
   is at global scope.

   EXPRESSION is the expression with the CONTEXT's value function that
   contains the meta-node."

  node context meta-node expression)

(defmethod equalp ((a instance) (b instance))
  (and (= (instance-node a) (instance-node b))
       (= (instance-context a) (instance-context b))
       (= (instance-meta-node a) (instance-meta-node b))
       (eq (instance-expression a) (instance-expression b))))

(defmethod hash ((inst instance))
  (with-struct-slots instance- (node meta-node context expression)
      inst

    (-> (hash node)
        (* 31)
        (+ (hash context))
        (* 31)
        (+ (hash meta-node))
        (* 31)
        (+ (hash expression))
        (* 31)
        (mod most-positive-fixnum))))


;;; Utilities

(defun process-meta-node (fn)
  "Returns a function of one argument, a meta-node, that applies FN on
   the definition of the meta-node. If the definition of the meta-node
   is NIL, FN is not applied on it."

  (lambda (meta-node)
    (awhen (definition meta-node)
      (funcall fn it))))

(defun external-meta-node (name operands &optional attributes)
  "Creates an `EXTERNAL-META-NODE' with name NAME, operand identifiers
   OPERANDS. ATTRIBUTES is a list of attributes to add to the node
   where each element is a list of the form (ATTRIBUTE VALUE)."

  (aprog1 (make-instance 'external-meta-node
                         :name (-> name
                                   symbol-name
                                   string-downcase
                                   id-symbol)
                         :operands operands)

    (doseq ((attribute value) attributes)
      (setf (attribute attribute it) value))))

(defun external-meta-nodes (defs)
  "Creates a map of `EXTERNAL-META-NODE's where the keys are the
   meta-node identifiers and the values are the `EXTERNAL-META-NODE'
   object. DEFS is a list of the meta-node's to create, where each
   item is a list of the form (NAME OPERANDS ATTRIBUTES), on which
   EXTERNAL-META-NODE is applied. The name of the meta-node is the
   downcased SYMBOL-NAME of NAME interned in the TRIDASH.SYMBOLS
   package."

  (alist-hash-map (map #L(cons (first %1) (apply #'external-meta-node %1)) defs)))


;;; Macro Functions

(defun node-macro-function (meta-node)
  "Returns the meta-nodes macro function if it is a macro."

  (get :macro-function (attributes meta-node)))

(defun (setf node-macro-function) (fn meta-node)
  "Sets the macro function of a meta-node. This essentially makes
   META-NODE a macro."

  (setf (get :macro-function (attributes meta-node)) fn))


;;; Predicates

(defun meta-node? (x)
  "Returns true if X is a `meta-node'."
  (typep x 'meta-node))

(defun external-meta-node? (x)
  "Returns true if X is an externally defined meta-node."
  (typep x 'external-meta-node))


;;; Meta-Node Attributes

(defun target-meta-node (meta-node)
  "Returns the name of the meta-node to use for the binding from the
   meta-node instance to the meta-node operands."

  (attribute :target-node meta-node))


;;; Operand Type Symbols

(defconstant +optional-argument+ +def-operator+
  "Symbol indicating an optional argument.")

(defconstant +rest-argument+ +outer-operator+
  "Symbol indicating a rest argument.")

(defconstant +outer-node-argument+ 'ex
  "Symbol indicating an argument in which the value of an outer-node
   is passed.")


;;; Outer Node References

(defun unique-node-name (hash-table prefix)
  "Generates a new unique node identifier. The identifier generated is
   a CONS with with the CAR set to PREFIX and the CDR set to the
   number of entries in HASH-TABLE."

  (cons prefix (length hash-table)))

(defun outer-node-name (meta-node)
  "Generates a new name for a local node which will be used to
   reference an outer node."

  (unique-node-name (outer-nodes meta-node) +outer-node-argument+))

(defun outer-node (node outer-module meta-node)
  "Returns the name of the local node, in META-NODE, which is bound to
   the outer node NODE, from the module OUTER-MODULE."

  (with-slots (outer-nodes) meta-node
    (cdr (ensure-get node outer-nodes (cons outer-module (outer-node-name meta-node))))))


;;; Operands

(defun operand-node-names (meta-node)
  "Returns the names of the operand nodes of META-NODE."

  (map
   (lambda (operand)
     (match operand
       ((or (list* (eql +optional-argument+) name _)
            (list (eql +rest-argument+) name)
            name)
        name)))
   (operands meta-node)))

(defun optional-operand-values (meta-node)
  "Returns the default values for the optional arguments of
   META-NODE."

  (flet ((optional? (operand)
           (match operand
             ((list* (eql +optional-argument+) _)
              t))))

    (->> meta-node
         operands
         (remove-if-not #'optional?)
         (map #'third))))

(defun check-arity (meta-node arguments)
  "Checks that the correct number of arguments are given to
   META-NODE. Signals an `ARITY-ERROR' condition if the number of
   arguments is incorrect."

  (unless (correct-arity? meta-node arguments)
    (error 'arity-error
           :meta-node meta-node
           :arity (meta-node-arity meta-node)
           :arguments (length arguments))))

(defun correct-arity? (meta-node arguments)
  "Returns true if the ARGUMENTS contains the correct number of
   arguments to META-NODE."

  (destructuring-bind (min . max) (meta-node-arity meta-node)
    (if max
        (<= min (length arguments) max)
        (<= min (length arguments)))))

(defun meta-node-arity (meta-node)
  "Returns a CONS where the CAR is the minimum number of arguments and
   the CDR is the maximum number of arguments, NIL if there is no
   maximum, that META-NODE accepts."

  (reduce
   (lambda (arity operand)
     (match operand
       ;; Outer Nodes
       ((cons (eql +outer-node-argument+) _)
        (return-from meta-node-arity arity))

       ;; Rest Arguments
       ((list (eql +rest-argument+) _)
        (return-from meta-node-arity (cons (car arity) nil)))

       ;; Optional Arguments
       ((list* (eql +optional-argument+) _ _)
        (destructuring-bind (min . max) arity
          (cons min (1+ max))))

       ;; Required Arguments
       (_
        (let ((arity (1+ (car arity))))
          (cons arity arity)))))

   (operands meta-node)

   :initial-value (cons 0 0)))
