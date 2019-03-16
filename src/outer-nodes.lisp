;;;; outer-nodes.lisp
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

;;;; Determines which meta-nodes reference which outer nodes

(in-package :tridash.frontend)


(defun find-outer-node-references (node-table)
  "Determines the outer nodes referenced by all meta-nodes, and their
   sub meta-nodes, in NODE-TABLE. The OUTER-NODES slot, of each
   meta-node, is populated after calling this function."

  (let (visited)
    (declare (special visited))
    (labels ((get-outer-node-references (meta-node)
               "Determines the outer-nodes which are referenced within
                the definition of META-NODE."

               (let ((visited (make-hash-set)))
                 (declare (special visited))
                 (outer-node-references meta-node)))

             (outer-node-references (meta-node)
               "Augments the set of outer-nodes, referenced from
                within META-NODE, with the set of outer-nodes,
                referenced by each meta-node used within the
                definition of META-NODE, which are not in the visited
                SET. The first return value is the new set of outer
                nodes and the second return value is true if the set
                is complete otherwise it is NIL."

               (nadjoin meta-node visited)

               (with-slots (outer-nodes meta-node-references) meta-node
                 (doseq (meta-node-ref meta-node-references)
                   (unless (visited? meta-node-ref)
                     (multiple-value-bind (refs complete?)
                         (outer-node-references meta-node-ref)

                       (add-outer-nodes meta-node refs)
                       (when complete?
                         (erase meta-node-references meta-node-ref)))))

                 (values outer-nodes (emptyp meta-node-references))))

             (add-outer-nodes (meta-node refs)
               "Adds the outer-node references REFS to the outer-nodes
                set of META-NODE. Excludes outer-nodes which are
                defined within a sub-table of the definition of
                META-NODE."

               (with-slots (definition outer-nodes) meta-node
                 (doseq ((node . ref) refs)
                   (let ((table (car ref)))
                     (unless (>= (depth table) (depth definition))
                       (ensure-get node outer-nodes (cons table (outer-node-name meta-node))))))))

             (visited? (meta-node)
               (memberp meta-node visited)))

      (foreach #'get-outer-node-references (map-values (meta-nodes node-table)))
      (foreach (process-meta-node #'find-outer-node-references) (map-values (meta-nodes node-table))))))

(defun add-outer-node-operands (meta-nodes)
  "To each meta-node in META-NODES: appends the outer nodes referenced
   by it to the OPERANDS list and updates all instances of the
   meta-node to pass the values of the referenced nodes as arguments."

  (labels ((add-outer-node-operands (meta-node)
             "Appends the outer nodes referenced by META-NODE to the
              OPERANDS list of META-NODE and updates all instances to
              pass the values of the nodes as arguments."

             (with-slots (outer-nodes operands definition) meta-node
               (let ((names (mapcar #'cdr (map-values outer-nodes))))
                 (appendf operands names)
                 (add-operand-nodes names definition))

               (update-instances meta-node (map-keys outer-nodes))))

           (update-instances (meta-node nodes)
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

    (foreach #'add-outer-node-operands meta-nodes))

  (foreach (process-meta-node (compose #'add-outer-node-operands #'map-values #'meta-nodes)) meta-nodes))
