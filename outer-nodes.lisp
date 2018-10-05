;;;; outer-nodes.lisp
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

;;;; Determines which meta-nodes reference which outer nodes

(in-package :metalink.frontend)


(defun find-outer-node-references (node-table)
  "Determines the outer nodes referenced by all meta-nodes, and their
   sub meta-nodes, in NODE-TABLE. The OUTER-NODES slot, of each
   meta-node, is populated after calling this function."

  (let (visited)
    (declare (special visited))
    (labels ((get-outer-node-references (meta-node)
               (let ((visited (make-hash-table :test #'eq)))
                 (declare (special visited))
                 (outer-node-references meta-node)))

             (outer-node-references (meta-node)
               (ensure-gethash meta-node visited t)

               (with-slots (outer-nodes meta-node-references) meta-node
                 (dohash (meta-node-ref nil meta-node-references)
                   (unless (visited? meta-node-ref)
                     (multiple-value-bind (refs complete?)
                         (outer-node-references meta-node-ref)

                       (add-outer-nodes meta-node refs)
                       (when complete?
                         (remhash meta-node-ref meta-node-references)))))

                 (values outer-nodes (zerop (hash-table-count meta-node-references)))))

             (add-outer-nodes (meta-node refs)
               (with-slots (definition outer-nodes) meta-node
                 (dohash (node info refs)
                   (let ((table (car info)))
                     (unless (>= (depth table) (depth definition))
                       (ensure-gethash node outer-nodes (cons table (outer-node-name meta-node))))))))

             (visited? (meta-node)
               (gethash meta-node visited)))

      (maphash-values #'get-outer-node-references (meta-nodes node-table))
      (maphash-values (compose #'find-outer-node-references #'definition) (meta-nodes node-table)))))

(defun add-outer-node-operands (node-table)
  "To each meta-node in node-table, appends the outer nodes referenced
   by it to the OPERANDS list and updates all instances of the
   meta-node to pass the values of the referenced nodes as arguments."

  (labels ((add-outer-node-operands (meta-node)
             "Appends the outer nodes referenced by META-NODE to the
              OPERANDS list of META-NODE and updates all instances to
              pass the values of the nodes as arguments."

             (with-slots (outer-nodes operands definition) meta-node
               (multiple-value-bind (nodes info) (hash-table-keys-values outer-nodes)
                 (let ((names (mapcar #'cdr info)))
                   (appendf operands names)
                   (add-local-nodes names definition))

                 (update-instances meta-node nodes))))

           (update-instances (meta-node nodes)
             "Updates each instance of META-NODE to pass the values of
              each node in NODES as additional arguments."

             (mapc (rcurry #'update-instance nodes meta-node) (instances meta-node)))

           (update-instance (instance nodes fn)
             "Binds each node in NODES to INSTANCE and appends the
              values of NODES to the argument list of the meta-node,
              within the value function of INSTANCE. If INSTANCE is
              located inside another meta-node, the local nodes which
              reference NODES are bound to INSTANCE instead of NODES
              themselves."

             (destructuring-bind (node . meta-node) instance
               (let ((nodes (operand-nodes nodes meta-node)))
                 (appendf (value-function node fn) (bind-operands node nodes)))))

           (operand-nodes (nodes meta-node)
             "Returns the nodes local to META-NODE which reference the
              outer nodes NODES. If META-NODE is NIL, NODES is
              returned directly."

             (if meta-node
                 (with-slots (outer-nodes definition) meta-node
                   (iter (for node in nodes)
                         (for name = (cdr (gethash node outer-nodes)))
                         ;; If node not found in outer node
                         ;; references, assume it is defined in the
                         ;; meta-nodes node table.
                         (if name
                             (collect (ensure-node name definition))
                             (collect node))))
                 nodes)))

    (maphash-values #'add-outer-node-operands (meta-nodes node-table)))
  (maphash-values (compose #'add-outer-node-operands #'definition) (meta-nodes node-table)))
