;;;; analyze.lisp
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

(in-package :metalink.backend.js)

(defun find-lazy-nodes (node-table)
  "Determines which nodes in NODE-TABLE should be evaluated lazily. A
   node should be evaluated lazily if each of its observers should be
   evaluated lazily or the binding to the observer node is
   conditional. Returns a hash-table where the value corresponding to
   each node is T if the node should be evaluated lazily, NIL
   otherwise."

  (let ((lazy-nodes (make-hash-table :test #'eq)))
    (labels ((lazy? (node)
               "Returns true if NODE should be evaluated lazily."

               (ensure-gethash
                node lazy-nodes
                (when (plusp (observers-count node))
                  (loop
                     for observer being the hash-key of (observers node)
                     always (or (not (unconditional-binding? node observer))
                                (lazy? observer))))))

             (unconditional-binding? (dependency observer)
               "Returns true if the value of DEPENDENCY is used
                unconditionally in the value function of OBSERVER."

               (has-node dependency (value-function observer)))

             (has-node (dependency fn)
               "Returns true if DEPENDENCY is used unconditionally in FN."

               (match fn
                 ((node-link- node)
                  (eq node dependency))

                 ((list* 'if cond _)
                  (has-node dependency cond))

                 ((list* _ operands)
                  (some (curry #'has-node dependency) operands)))))

      (maphash-values #'lazy? (nodes node-table))
      lazy-nodes)))
