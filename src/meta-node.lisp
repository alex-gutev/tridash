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
  ()

  (:documentation
   "Base class representing a meta-node."))

(defclass meta-node-spec (meta-node)
  ((operands
    :initarg :operands
    :initform nil
    :accessor operands
    :documentation
    "List containing the symbols naming the local nodes to which the
     values of the arguments are bound.")

   (definition
    :initarg :definition
    :initform nil
    :accessor definition
    :documentation
    "List of the node declarations comprising the body."))

  (:documentation
   "Represents a meta-node which has not been built yet. Contains all
    declarations necessary for building the body."))

(defclass built-meta-node (meta-node)
  ((operands
    :initarg :operands
    :initform nil
    :accessor operands
    :documentation
    "List containing the local nodes to which the values of the
     arguments are bound.")

   (outer-nodes
    :initform (make-hash-map)
    :accessor outer-nodes
    :documentation
    "Map mapping nodes, declared in an outer node table, to the
     corresponding local nodes within the definition.")

   (meta-node-references
    :initform nil
    :accessor meta-node-references
    :documentation
    "Set of meta-nodes used within the definition.")

   (definition
    :initarg :definition
    :initform nil
    :accessor definition
    :documentation
    "The `MODULE' storing the nodes comprising the definition."))

  (:documentation
   "Represents a meta-node which has been built, however the final
    processing steps have not been performed."))

(defclass final-meta-node (built-meta-node)
  ()

  (:documentation
   "Represents a meta-node which has been built with all the final
    processing steps performed."))

(defclass external-meta-node (meta-node)
  ((operands
    :initarg :operands
    :initform nil
    :accessor operands
    :documentation
    "List containing the symbols naming the local nodes to which the
     values of the arguments are bound."))

  (:documentation
   "Represents a meta-node which is defined externally, not in Tridash
    source code."))


(defun external-meta-node (name operands &optional attributes)
  "Creates an `EXTERNAL-META-NODE' with name NAME, operand identifiers
   OPERANDS. ATTRIBUTES is a list of attributes to add to the node
   where each element is a list of the form (ATTRIBUTE VALUE)."

  (let ((name (if (symbolp name)
                  (-> name
                      symbol-name
                      string-downcase
                      id-symbol)
                  (id-symbol name))))
    (aprog1 (make-instance 'external-meta-node
                           :name name
                           :operands operands)

      (doseq ((attribute value) attributes)
        (setf (attribute attribute it) value)))))

(defun external-meta-nodes (defs)
  "Creates a map of `EXTERNAL-META-NODE's where the keys are the
   meta-node identifiers and the values are the `EXTERNAL-META-NODE'
   object. DEFS is a list of the meta-nodes to create, where each item
   is a list of the form (NAME OPERANDS ATTRIBUTES), on which
   EXTERNAL-META-NODE is applied. The name of the meta-node is the
   downcased SYMBOL-NAME of NAME interned in the TRIDASH.SYMBOLS
   package."

  (map-to
   'hash-map
   (lambda (def)
     (match def
       ((list*
         (or (list sym name)
             (and sym name))
         def)

        (cons sym (apply #'external-meta-node (cons name def))))))
   defs))


(defun macro-node (name operands operator)
  "Creates an `EXTERNAL-META-NODE', with identifier NAME, which has a
   macro function that expands to a functor declaration with operator
   OPERATOR."

  (aprog1 (external-meta-node name operands)
    (setf (node-macro-function it)
          (lambda (op operands module)
            (declare (ignore op))
            (-> (cons operator operands)
                (process-macro-expansion module))))))

(defun process-macro-expansion (declaration module)
  "Process DECLARATION, in module MODULE, which is assumed to be the
   result of a macro expansion."

  (process-declaration declaration
                       module
                       :level *level*
                       :add-outer nil))


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

(defun target-transform-node (meta-node)
  (handler-case
      (aprog1 (attribute :target-transform meta-node)
        (when it (compile-meta-node-function it)))

    (compile-meta-node-loop-error ())))


;;; Operand Type Symbols

(defconstant +optional-argument+ (id-symbol ":")
  "Symbol indicating an optional argument.")

(defconstant +rest-argument+ (id-symbol "..")
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

(defmethod outer-nodes ((meta-node external-meta-node))
  (make-hash-map))


;;; Operands

(defgeneric operand-node-names (meta-node)
  (:documentation
   "Returns the names of the operand nodes of META-NODE.")

  (:method ((meta-node meta-node))
    (map
     (lambda (operand)
       (match operand
         ((or (list* (eql +optional-argument+) name _)
              (list (eql +rest-argument+) name)
              name)
          name)))
     (operands meta-node)))

  (:method ((meta-node built-meta-node))
    (map #'name (call-next-method))))

(defun outer-node-operand-names (meta-node)
  "Returns the names of the outer node operands, in the order they
   should appear in the argument list, following the last argument."

  (map
   (lambda (node)
     (name (get node (outer-nodes meta-node))))
   (outer-node-references meta-node)))


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


;;; Print Methods

(defmethod print-object ((node meta-node) stream)
  (format stream "<Meta-Node: ~a>" (name node)))

(defmethod print-object ((node external-meta-node) stream)
  (format stream "<External-Meta-Node: ~a>" (name node)))
