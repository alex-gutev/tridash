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

;;;; Coalesce successive nodes into a single node.

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


(defun remove-all-unreachable-nodes (graph)
  "Removes all unreachable nodes in GRAPH and in the definitions of
   each meta-node in GRAPH."

  (flet ((remove-unreachable-nodes (graph)
           (when graph
             (remove-unreachable-nodes graph))))
    (remove-unreachable-nodes graph)
    (maphash-values (compose #'remove-unreachable-nodes #'definition) (meta-nodes graph))))

(defun remove-unreachable-nodes (graph)
  "Removes all nodes which are not reachable from an input node."

  (with-slots (nodes all-nodes) graph
    (let ((visited (make-hash-table :test #'eq)))
      (labels ((mark (node)
                 (unless (gethash node visited)
                   (setf (gethash node visited) t)
                   (maphash-keys #'mark (observers node))))

               (sweep (name node)
                 (unless (or (gethash node visited) (attribute :no-remove node))
                   (remhash name nodes)
                   (remhash name all-nodes))))

        (mapc #'mark (input-nodes graph))
        (maphash #'sweep nodes)))))
