;;;; wait-set.lisp
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

;;;; Determines which nodes need to inform other nodes of a value
;;;; change.

(in-package :metalink.frontend)


(defun build-wait-sets (graph)
  "Builds the wait sets of each node in GRAPH and the sub-graphs of
   each meta-node."

  (labels
      ((begin-walk (start)
         "Traverses the graph beginning at the node START."

         (let ((reachable-set (make-hash-table :test #'eq)))
           (declare (special reachable-set))
           (add-all-reachable start reachable-set)
           (walk-observers start (list start))))

       (walk-observers (node stack)
         "Walks the observers of NODE. And marks NODE as visited
            with the path to the node being STACK."

         (unless (visited? node)
           (mark-node node stack)

           (let-if ((stack (cons node stack) stack))
               (and (> (observers-count node) 1) (not (eq (first stack) node)))

             (maphash-keys (rcurry #'walk-node stack) (observers node)))))

       (walk-node (node stack)
         "Visits the node NODE. If NODE has multiple dependencies
            and all dependencies have been visited, they are added to
            the wait set of the last node which is common to the paths
            to all dependencies. If not all dependencies are reachable
            but all reachable dependencies have been visited: the
            reachable dependencies are added to the wait-set of the
            input node (the start node of the current traversal)."

         (cond
           ((n-ary-node? node)
            (multiple-value-bind (deps all-deps?)
                (reachable-deps (dependency-list node))

              (awhen (all-marked? deps stack)
                ;; Augment the wait-set of either the last node before the
                ;; paths to the dependency nodes diverge, or the first node
                ;; in the path (if not all dependencies are reachable from
                ;; it).

                ;; If the last node before the paths diverge can
                ;; function as an input node, do not augment its wait
                ;; set, instead augment the wait set of the first node
                ;; in the path

                (augment-wait-set
                 (if (and all-deps? (not (input-node? (first it))))
                     (first it)
                     (lastcar it))
                 node deps)

                (walk-observers node it))))

           (t (walk-observers node stack))))

       (reachable-deps (nodes)
         "Returns the list of nodes in NODES which are reachable
            from the input node. The second return value is true if
            all nodes in NODES are reachable."

         (declare (special reachable-set))

         (let* ((deps (remove-if-not (rcurry #'in-hash? reachable-set) nodes)))
           (values deps (same-length? deps nodes))))

       (visited? (node)
         "Returns the path to NODE if it has been visited, NIL otherwise."

         (declare (special reachable-set))
         (gethash node reachable-set))

       (mark-node (node stack)
         "Marks NODE as visited with the path to the node STACK."

         (let-if ((stack (rest stack) stack))
             (and (rest stack)
                  (eq (first stack) node))

           (declare (special reachable-set))
           (setf (gethash node reachable-set) stack)))

       (all-marked? (nodes stack)
         "Returns the initial path which is common to the paths to
            all nodes in NODES and the path STACK. If not all nodes
            have been visited the return value is NIL."

         (reduce
          (lambda (stack node)
            (aif (visited? node)
                 (common-path it stack)
                 (return-from all-marked? nil)))
          nodes :initial-value stack)))

    (mapc #'begin-walk (input-nodes graph))
    (maphash-values (compose #'build-wait-sets #'definition) (meta-nodes graph))))


(defun add-all-reachable (start reachable-set)
  "Adds all nodes reachable from START to the hash-table
   REACHABLE-SET."

  (labels ((walk (node)
             (unless (adjoin-hash node reachable-set)
               (mapc #'walk (observer-list node)))))
    (walk start)))


;;;; Utility Functions

(defun common-path (path1 path2)
  "Returns the initial path which is common to the paths (PATH1 and
   PATH2) to two nodes, that is the node at the front of the list is
   the last common node before the paths diverge."

  (aif (mismatch path1 path2 :from-end t)
       (subseq path1 it)
       path1))

(defun augment-wait-set (node obs-node nodes)
  "Adds the dependency nodes NODES, of the observer node OBS-NODE, to
   the wait set of NODE."

  (with-accessors ((wait-set wait-set)) node
    (setf (gethash obs-node wait-set)
          nodes)))
