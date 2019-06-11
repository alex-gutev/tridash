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

(defun outer-node-references (meta-node &optional (visited (make-hash-set)))
  "Augments the set of outer-nodes, referenced from within META-NODE,
   with the set of outer-nodes, referenced by each meta-node used
   within the definition of META-NODE. Also augments the outer node
   references of the meta-node's nested in it. The first return value
   is the new set of outer nodes and the second return value is true
   if the set is complete otherwise it is NIL."

  (nadjoin meta-node visited)

  (with-slots (definition outer-nodes meta-node-references) meta-node
    (flet ((add-outer-nodes (refs)
             "Adds the outer-node references REFS to the outer-nodes
              set of META-NODE. Excludes outer-nodes which are defined
              within a sub-table of the definition of META-NODE."

             (doseq ((node . ref) refs)
               (let ((table (car ref)))
                 (unless (>= (depth table) (depth definition))
                   (ensure-get node outer-nodes (cons table (outer-node-name meta-node)))))))

           (visited? (meta-node)
             (memberp meta-node visited)))

      (doseq (meta-node-ref meta-node-references)
        (unless (visited? meta-node-ref)
          (multiple-value-bind (refs complete?)
              (outer-node-references meta-node-ref visited)

            (add-outer-nodes refs)
            (when complete?
              (erase meta-node-references meta-node-ref)))))

      (when definition
        (foreach #'outer-node-references (map-values (meta-nodes definition))))

      (values outer-nodes (emptyp meta-node-references)))))

(defun add-outer-node-operands (meta-node)
  "Appends the outer nodes referenced by META-NODE to the operands
   list of META-NODE and updates all instances to pass the values of
   the nodes as arguments."

  (labels ((update-instances (nodes)
             "Updates each instance of META-NODE to pass the values of
              each node in NODES as additional arguments."

             (foreach (rcurry #'update-instance nodes) (instances meta-node)))

           (update-instance (instance nodes)
             "Binds each node in NODES to INSTANCE and appends the
              values of NODES to the argument list of the meta-node,
              within the value function of INSTANCE. If INSTANCE is
              located inside another meta-node, the local nodes which
              reference NODES are bound to INSTANCE instead of NODES
              themselves."

             (destructuring-bind (node context meta-node) instance
               (update-context node context (operand-nodes nodes meta-node))))

           (update-context (node context-id operands)
             "Adds OPERANDS as operands to the context, of NODE, with
              identifier CONTEXT-ID, and appends them to the context's
              value function."

             (appendf (value-function (context node context-id))
                      (bind-operands node operands :context context-id)))

           (operand-nodes (nodes meta-node)
             "Returns the nodes local to META-NODE which reference the
              outer nodes NODES. If META-NODE is NIL, NODES is
              returned directly."

             (if meta-node
                 (with-slots (outer-nodes definition) meta-node
                   (map
                    (lambda (node)
                      ;; If node not found in outer node
                      ;; references, assume it is defined in the
                      ;; meta-node's node table.
                      (if-let ((name (cdr (get node outer-nodes))))
                        (ensure-node name definition)
                        node))
                    nodes))
                 nodes)))

    (with-slots (outer-nodes operands definition) meta-node
      (let ((names (map #'cdr (map-values outer-nodes))))
        (appendf operands names)
        (add-operand-nodes names definition))

      (update-instances (map-keys outer-nodes))

      (when definition
        (foreach #'add-outer-node-operands (map-values (meta-nodes definition)))))))
