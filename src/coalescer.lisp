;;;; coalescer.lisp
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

;;;; Functions for coalescing successive nodes into one, removing
;;;; unreachable nodes and checking the graph structure.

(in-package :tridash.frontend)

(in-readtable cut-syntax)


(defun coalesce-nodes (graph)
  "Coalesces successive nodes, which only have a single observer, into
   single nodes"

  (let ((visited (make-hash-table :test #'eq)))
    (labels
        ((begin-coalesce (node)
           "Begins node coalescing starting from the node NODE. Clears
            the visited set."

           (clrhash visited)
           (coalesce-observers node))

         (coalesce-observers (node)
           "Performs node coalescing on the observer nodes of NODE."

           (setf (gethash node visited) t)
           (maphash-keys #'coalesce-node (observers node)))

         (coalesce-node (node)
           "Coalesces the node NODE (into its observer node) if it
            only has a single observer, after node coalescing is
            performed on its observer nodes and redundant 2-way
            bindings are removed."

           (unless (gethash node visited)
             (setf (gethash node visited) t)

             (coalesce-observers node)

             (unless (input-node? node)
               (remove-redundant-2-way-links node)
               (eliminate-node node))))

         (remove-redundant-2-way-links (node)
           (when (= (hash-table-count (contexts node)) 1)
             (maphash-keys (curry #'remove-observer node) (dependencies node))))


         (eliminate-node (node)
           "Removes the node NODE if it has a single observer. Its
            dependencies are merged into the dependency set of its
            observer node, it's replaced with its observer node in the
            observer sets of its dependencies, and the node referenced
            within the `node-link' objects of its observer nodes, is
            replaced with the value-function of NODE."

           (when (coalesce? node)
             (remove-node (name node) graph)
             (merge-dependencies node (first (observer-list node)))))

         (coalesce? (node)
           "Returns true if NODE can be coalesced."

           (and (may-coalesce? node)
                (= (observers-count node) 1)
                (<= (hash-table-count (contexts node)) 1)))

         (merge-dependencies (node observer)
           "Merges the dependencies of NODE into the dependency set of
            OBSERVER. NODE is removed from the dependency set of
            OBSERVER and the observer sets of its
            dependencies. References to NODE within the value function
            of OBSERVER are replaced with the value function of NODE,
            this assumes that NODE only has a single value function."

           (with-slots (dependencies) observer
             (let* ((context (first (hash-table-values (contexts node))))
                    (link (gethash node dependencies))
                    (context-id (node-link-context link)))

               (with-slots (operands value-function) (context observer context-id)
                 ;; Update link NODE -> OBSERVER to store VALUE-FUNCTION of NODE
                 (setf (node-link-node link) (value-function context))

                 ;; Remove NODE from DEPENDENCIES of OBSERVER
                 (remhash node dependencies)
                 (remhash node operands)

                 ;; Add all dependencies of NODE to dependencies of OBSERVER
                 (dohash (dependency link (dependencies node))
                   ;; Check if OBSERVER Already has DEPENDENCY as a dependency
                   (when-let ((old-link (gethash dependency dependencies)))
                     (unless (gethash dependency operands)
                       (error 'ambiguous-context-error :node observer))

                     (setf (node-link-node old-link) link))

                   ;; Add DEPENDENCY to dependencies of OBSERVER
                   (setf (gethash dependency dependencies) link)
                   (setf (gethash dependency operands) link)
                   (setf (node-link-context link) context-id)

                   (with-slots (observers) dependency
                     ;; Remove NODE from observers of DEPENDENCY
                     (remhash node observers)
                     ;; Add OBSERVER to observers of DEPENDENCY
                     (setf (gethash observer (observers dependency)) link))))))))

      (when graph
        (mapc #'begin-coalesce (input-nodes graph))
        (maphash-values (compose #'coalesce-nodes #'definition) (meta-nodes graph))

        (maphash-values #'coalesce-node-links (all-nodes graph))))))

(defun may-coalesce? (node)
  "Returns true if NODE may be coalesced into another node. Returns
   false if the node has the :NO-COALESCE attribute set to T."

  (null (attribute :no-coalesce node)))

(defun coalesce-node-links (node)
  "Replaces the `node-link' objects, within the value functions of
   NODE, which do not directly reference another node, with their
   contents."

  (when (node? node)
    (with-slots (contexts) node
      (labels ((remove-node-links (fn)
                 "Replaces all `node-link' objects (within the value
                  function FN), which do not directly reference
                  another node, with their contents."

                 (match fn
                   ((list* meta-node operands)
                    (list* meta-node (mapcar #'remove-node-links operands)))

                   ((node-link- (node (and fn (not (type node)) (not (type symbol)))))
                    (remove-node-links fn))

                   (_ fn))))

        (dohash (nil context contexts)
          (with-slots (value-function) context
            (setf value-function (remove-node-links value-function))))))))


(defun remove-unreachable-nodes (module-table)
  "Removes unreachable nodes in each module in MODULE-TABLE. An
   unreachable node is a node which is not reachable from any
   input-node of any module.

   Unreachable nodes are also removed from the sub-graphs of each
   meta-node in each module."

  (let ((visited (make-hash-table :test #'eq)))
    (labels ((mark (node)
               (unless (visited? node)
                 (setf (gethash node visited) t)
                 (maphash-keys #'mark (observers node))))

             (sweep (name node node-table)
               (unless (or (visited? node) (attribute :no-remove node))
                 (remhash name (nodes node-table))
                 (remhash name (all-nodes node-table))

                 (awhen (some #'visited? (observer-list node))
                   (error 'dependency-not-reachable :dependency node :node it))))

             (visited? (node)
               (and (gethash node visited)
                    node))

             (mark-in-module (node-table)
               (mapc #'mark (input-nodes node-table)))

             (sweep-in-module (node-table)
               (maphash (rcurry #'sweep node-table) (nodes node-table)))

             (mark-sweep-in-meta-nodes (node-table)
               (maphash-values #'mark-sweep-in-meta-node (meta-nodes node-table)))

             (mark-sweep-in-meta-node (meta-node)
               (with-slots (definition) meta-node
                 (when definition
                   (mark-in-module definition)
                   (sweep-in-module definition)

                   (mark-sweep-in-meta-nodes definition)
                   (maphash-values #'mark-sweep-in-meta-node (meta-nodes definition))))))

      (with-slots (modules) module-table
        (maphash-values #'mark-in-module modules)
        (maphash-values #'sweep-in-module modules)

        (maphash-values #'mark-sweep-in-meta-nodes modules)))))


(defun fold-constant-nodes (graph)
  "Performs constant node folding on all nodes in GRAPH and in the
   meta-node definitions in GRAPH.

   Removes all constant nodes and replaces links to the nodes with the
   constant values. Constant nodes are nodes which only have an :INIT
   context."

  (flet ((fold-constant-nodes (graph)
           (when graph
             (fold-value-nodes graph))))

    (fold-constant-nodes graph)
    (maphash-values (compose #'fold-constant-nodes #'definition) (meta-nodes graph))))

(defun fold-value-nodes (graph)
  "Removes all constant nodes and replaces links to the nodes with the
   constant values. Constant nodes are nodes which only have an :INIT
   context."

  (labels ((value-nodes ()
             (remove-if-not #'value-node? (hash-table-values (nodes graph))))

           (value-node? (node)
             (unless (input-node? node)
               (with-slots (contexts) node
                 (and (= (hash-table-count contexts) 1)
                      (-> (first (hash-table-values contexts))
                          (operands)
                          (hash-table-count)
                          (zerop))))))

           (fold-value (node)
             (when (value-node? node)
               (let ((obs (observer-list node)))
                 (-> (contexts node)
                     (hash-table-values)
                     (first)
                     (value-function)
                     (replace-dependency node))

                 (clrhash (observers node))
                 (mapc #'fold-value obs))))

           (replace-dependency (value node)
             (iter
               (for (obs link) in-hashtable (observers node))

               (remhash node (dependencies obs))
               (setf (node-link-node link) value)

               (let ((context (context obs (node-link-context link))))
                 (remhash node (operands context))))))

    (mapc #'fold-value (value-nodes))))

(defun check-structure (graph)
  "Checks the structure of GRAPH making sure there are no cycles and
   no node has an ambiguous context. If a cycle or ambiguous context
   is detected, an error condition is signaled."

  (with-slots (nodes all-nodes) graph
    (let ((visited (make-hash-table :test #'eq)))
      (labels ((check-meta-node-graph (graph)
                 "Check the structure of the meta-node graph GRAPH."

                 (when graph
                   (check-structure graph)))

               (begin-walk (node)
                 "Clears the visited set and begins the traversal at
                  NODE in the :INPUT context."

                 (clrhash visited)
                 (visit node (context node :input)))

               (visit (node context)
                 "Visits node NODE in the context CONTEXT."

                 (aif (gethash node visited)
                      (check-context node context it)
                      (visit-observers node context)))

               (check-context (node new-context marker)
                 "Checks that if node is visited for a second time, it
                  is not part of a cycle and it is visited in the same
                  context."

                 (destructuring-bind (temp? old-context) marker
                   (cond
                     (temp?
                      (error 'node-cycle-error :node node))

                     ((not (eq old-context new-context))
                      (error 'ambiguous-context-error :node node)))))

               (visit-observers (node context)
                 "Visits the observers of NODE in the context CONTEXT."

                 ;; Mark Temporarily
                 (setf (gethash node visited) (list t context))

                 (with-slots (operands) context
                   (iter
                     (for (observer link) in-hashtable (observers node))
                     (for context = (context observer (node-link-context link)))
                     (when (not (in-hash? observer operands))
                       (visit observer context))))

                 ;; Mark Permanently
                 (setf (car (gethash node visited)) nil)))

        (mapc #'begin-walk (input-nodes graph))
        (maphash-values (compose #'check-meta-node-graph #'definition) (meta-nodes graph))))))
