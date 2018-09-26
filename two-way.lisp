;;;; two-way.lisp
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

;;;; Remove redundant two-way bindings. A redundant two-way binding is
;;;; a two-way binding in which there will never be a data flow in at
;;;; least one of its directions.

(in-package :metalink.frontend)

(defun remove-redundant-links (graph)
  "Remove redundant two-way bindings."

  (labels ((remove-redundant-links ()
             "Removes all redundant 2-way links from all nodes in
              GRAPH."

             (let (changed)
               (iter (for (nil node) in-hashtable (nodes graph))
                     (for num-deps = (dependencies-count node))

                     (remove-links node)

                     (unless (= num-deps (dependencies-count node))
                       (setf changed t)))
               changed))

           (remove-links (node)
             "If NODE is not an input node, that is it has
              dependencies which are one-way bound, all its 2-way
              dependencies are removed from its dependency set."

             (if (input-node? node)
                 (setf (value-function node) nil)
                 (maphash (curry #'remove-2-way-link node) (dependencies node))))

           (remove-2-way-link (node dependency link)
             "Removes DEPENDENCY from the dependency set of NODE if
              its link LINK is a two-way link."

             (when (node-link-2-way-p link)
               (remove-condition node link)

               (remhash dependency (dependencies node))
               (remhash node (observers dependency))

               (setf (node-link-2-way-p (gethash dependency (observers node))) nil)))

           (remove-condition (node link)
             "Removes LINK from the VALUE-FUNCTION of NODE."
             (removef (value-function node) link)))

    (loop while (remove-redundant-links))))
