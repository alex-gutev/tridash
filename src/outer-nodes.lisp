;;;; outer-nodes.lisp
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

;;;; Determines which meta-nodes reference which outer nodes

(in-package :tridash.frontend)

(defgeneric outer-node-references (meta-node)
  (:documentation
   "Returns the list of outer nodes referenced by META-NODE, as they
    appear in the operands list.

    Updates the meta-node's operand list if it has not been updated
    already.")

  (:method ((meta-node external-meta-node))
    nil))

(defmethod outer-node-references ((meta-node meta-node))
  (with-slots (outer-nodes operands definition attributes) meta-node
    (unless (get :outer-operands attributes)
      (find-outer-node-references meta-node)

      (setf (get :outer-operands attributes)
            (coerce outer-nodes 'alist)))

    (map #'car (get :outer-operands attributes))))

(defgeneric find-outer-node-references (meta-node &optional visited)
  (:documentation
   "Augments the set of outer-nodes, referenced from within META-NODE,
    with the set of outer-nodes, referenced by each meta-node used
    within the definition of META-NODE. Also augments the outer node
    references of the meta-node's nested in it. The first return value
    is the new set of outer nodes and the second return value is true
    if the set is complete otherwise it is NIL.")

  (:method ((meta-node meta-node-spec) &optional (visited (make-hash-set)))
    (build-meta-node meta-node)
    (find-outer-node-references meta-node visited))

  (:method ((meta-node external-meta-node) &optional visited)
    (declare (ignore visited))
    nil)

  (:method ((meta-node final-meta-node) &optional visited)
    (declare (ignore visited))

    (values (outer-nodes meta-node) t)))

(defmethod find-outer-node-references ((meta-node built-meta-node) &optional (visited (make-hash-set)))
  (nadjoin meta-node visited)

  (with-slots (definition meta-node-references outer-nodes) meta-node
    (labels ((outer-node-refs (meta-node)
               "Returns the OUTER-NODES set of META-NODES."

               (unless (visited? meta-node)
                 (multiple-value-bind (refs complete?)
                     (find-outer-node-references meta-node visited)

                   (when complete?
                     (erase meta-node-references meta-node))

                   refs)))

             (union-refs (a b)
               "Merges the OUTER-NODES set B into A."

               (if b
                   (map-into a #'car b)
                   a))

             (add-outer-ref (node)
               "Adds NODE to the OUTER-NODES set of META-NODE if it is
                not defined within a sub-module of the definition of
                META-NODE."

               (let* ((module (home-module node)))
                 (unless (>= (depth module) (depth definition))
                   (add-outer-node node definition meta-node))))

             (visited? (meta-node)
               (memberp meta-node visited)))

      ;; Get all outer-node references
      (let ((refs (reduce #'union-refs meta-node-references
                          :key #'outer-node-refs
                          :initial-value (hash-set))))

        ;; Kludge: The meta-node may have already been fully built if
        ;; it is used by another node in its definition
        ;;
        ;; To resolve this issue all meta-nodes used by META-NODE, and
        ;; all their used meta-nodes, should be fully built.
        (unless (typep meta-node 'final-meta-node)
          (foreach #'add-outer-ref refs)
          (foreach (rcurry #'find-outer-node-references visited) (meta-nodes definition))))

      (values outer-nodes (emptyp meta-node-references)))))

(defun used-meta-nodes (nodes)
  "Returns the set of `META-NODES' which are used in the value
   functions of the nodes in NODES"

  (let ((meta-nodes (make-hash-set)))
    (labels
        ((walk-node (node)
           (when (node? node)
             (foreach #'walk-context (map-values (contexts node)))))

         (walk-context (context)
           (walk-expression #'add-used-meta-nodes (value-function context)))

         (add-used-meta-nodes (expression)
           (match expression
             ((functor-expression- (meta-node (and (type meta-node) meta-node)))
              (add-meta-node meta-node))

             ((meta-node-ref- node)
              (add-meta-node node))

             (_ t)))

         (add-meta-node (node)
           (nadjoin node meta-nodes)))

      (foreach #'walk-node nodes)
      meta-nodes)))


(defun update-meta-node-instances (node meta-node)
  "Updates each `FUNCTOR-EXPRESSION' and `META-NODE-REF' in each
   context of NODE, to pass outer nodes as arguments.

   META-NODE is the meta-node in which NODE is contained."

  (let (context-id)
    (declare (special context-id))
    (labels
        ((update-context (context)
           "Update the VALUE-FUNCTION of the `NODE-CONTEXT' CONTEXT."

           (destructuring-bind (context-id . context) context
             (declare (special context-id))
             (with-slots (value-function) context
               (setf value-function
                     (update-expression value-function)))))

         (update-expression (expression)
           (match expression
             ((functor-expression- (meta-node (and (type meta-node) meta-node)) arguments)
              (map-expression!
               #'update-expression

               (functor-expression
                meta-node
                arguments
                :outer-nodes
                (outer-node-links meta-node))))

             ((meta-node-ref node optional)
              (map-expression!
               #'update-expression

               (meta-node-ref node
                              :optional optional
                              :outer-nodes (outer-node-links node))))

             (_
              (map-expression! #'update-expression expression))))

         (outer-node-links (meta-node)
           "Bind each outer node, referenced by META-NODE, to NODE and
            return the list of the binding `NODE-LINK' objects."

           ;; Determine outer node references
           (outer-node-references meta-node)

           (let* ((nodes (coerce (outer-nodes meta-node) 'alist))
                  (links (bind-operands node (operand-nodes (map #'car nodes)) :context context-id)))

             (alist-hash-map (pairlis (map #'car nodes) links))))

         (operand-nodes (nodes)
           "Returns the nodes local to META-NODE which reference the
            outer nodes NODES. If META-NODE is NIL, NODES is returned
            directly."

           (if meta-node
               (with-slots (outer-nodes definition) meta-node
                 (map
                  (lambda (node)
                    ;; If node not found in outer node
                    ;; references, assume it is defined in the
                    ;; meta-node's node table.
                    (or (get node outer-nodes) node))
                  nodes))
               nodes)))

      (foreach #'update-context (contexts node)))))
