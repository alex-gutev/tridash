;;;; analyze.lisp
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

(in-package :tridash.backend.js)

(defun find-lazy-nodes (module-table)
  "Determines which nodes in all modules in MODULE-TABLE, and in which
   contexts, should be evaluated lazily. A node should be evaluated
   lazily, in a particular context, if each of its observers in the
   context should be evaluated lazily or the binding to the observer
   node is conditional. Returns a hash-table where each key is a node
   context and the corresponding value is T if the node should be
   evaluated lazily in the context, NIL otherwise."

  (let ((lazy-nodes (make-hash-table :test #'eq)))
    (labels ((find-lazy-nodes (node-table)
               (maphash-values #'lazy-node? (nodes node-table)))

             (lazy-node? (node)
               (maphash-values (curry #'lazy? node) (contexts node)))

             (lazy? (node context)
               "Returns true if NODE should be evaluated lazily in the
                context CONTEXT."

               (ensure-gethash
                context lazy-nodes
                (let ((observers (context-observers node context)))
                  (when observers
                    (loop for (observer link) in observers
                       always
                         (or (not (unconditional-binding? link observer))
                             (->> (node-link-context link)
                                  (context observer)
                                  (lazy? observer))))))))

             (context-observers (node context)
               "Returns a list of the observer nodes of NODE at
                context CONTEXT."

               (with-slots (operands) context
                 (iter (for (observer link) in-hashtable (observers node))
                       (unless (gethash observer operands)
                         (collect (list observer link))))))

             (unconditional-binding? (link observer)
               "Returns true if the value of the node with link LINK
                is used unconditionally in the value function of
                OBSERVER."

               (let ((context (context observer (node-link-context link))))
                 (aprog1 (has-node link (value-function context))
                   (unless it
                     (setf (node-link-node link)
                           (cons 'async (node-link-node link)))))))

             (has-node (link fn)
               "Returns true if DEPENDENCY is used unconditionally in FN."

               (match fn
                 ((type node-link)
                  (eq link fn))

                 ((list* 'if cond _)
                  (has-node link cond))

                 ((list* (or 'and 'or) first _)
                  (has-node link first))

                 ((list* _ operands)
                  (some (curry #'has-node link) operands)))))

      (maphash-values #'find-lazy-nodes (modules module-table))
      lazy-nodes)))
