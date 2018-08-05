;;;; coalescer.lisp
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

;;;; Coalesce successive nodes into a single node.

(in-package :metalink.frontend)

(defun coalesce-nodes (graph)
  "Coalesces successive nodes, which only have a single observer, into
   single nodes"

  (let ((visited (make-hash-table :test #'eq)))
    (labels
        ((coalesce-node (node)
           (unless (gethash node visited)
             (setf (gethash node visited) t)

             (mapc #'coalesce-node (observer-list node))

             (when (= (observers-count node) 1)
               (remhash (name node) (nodes graph))
               (remhash (name node) (all-nodes graph))

               (let ((observer (first (observer-list node))))
                 (merge-dependencies node observer)
                 (replace-operand node observer)))))

         (replace-operand (operand node)
           (with-slots (dependencies) node
             (mapc (rcurry #'replace-link-node (or (value-function operand)
                                                   (name operand)))
                   (ensure-list (gethash node (observers operand))))))

         (replace-link-node (link replacement)
           (setf (node-link-node link) replacement))

         (merge-dependencies (dependency observer)
           (with-slots (dependencies) observer
             (remhash dependency dependencies)

             ;; For each dependency
             (dohash (node link (dependencies dependency))

               ;; Add dependency NODE to dependencies of OBSERVER
               (slet (gethash node dependencies)
                 (setf it (union (ensure-list it) (ensure-list link))))

               (with-slots (observers) node
                 ;; Add OBSERVER to observers of dependency node NODE
                 (slet (gethash observer observers)
                   (setf it (union (ensure-list it) (ensure-list link))))

                 (remhash dependency observers))))))

      (mapc #'coalesce-node (input-nodes graph))
      (maphash-values (compose #'coalesce-nodes #'definition) (meta-nodes graph))
      (maphash-values #'coalesce-node-links (all-nodes graph)))))

(defun coalesce-node-links (node)
  "Replaces the `node-link' objects, within the VALUE-FUNCTION of
   NODE, which do not directly reference another node, with the value
   functions stored in them. Merges multiple `node-link' objects which
   refer to the same node into one `node-link' object and amends the
   dependency set of NODE such that each node in it is mapped to a
   single `node-link' object."

  (with-slots (dependencies observers value-function) node
    (labels ((remove-node-links (fn)
               (match fn
                 ((list* meta-node operands)
                  (list* meta-node (mapcar #'remove-node-links operands)))

                 ((type node-link)
                  (let ((node (node-link-node fn)))
                    (typecase node
                      (node
                       (gethash node dependencies))

                      (symbol fn)

                      (otherwise (remove-node-links node)))))

                 (_ fn)))

             (make-new-node-link (dependency)
               (alet (setf (gethash dependency dependencies) (node-link dependency))
                 (setf (gethash node (observers dependency)) it))))

      (maphash-keys #'make-new-node-link dependencies)
      (setf value-function (remove-node-links value-function)))))
