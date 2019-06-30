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

(defun find-lazy-nodes (table &optional (lazy-nodes (make-hash-map)))
  "Determines which nodes, and in which contexts, should be evaluated
   lazily. A node should be evaluated lazily, in a particular
   contexts, of each of its observers, in that context, should be
   evaluated lazily or the binding to the observer node is
   condition. Returns a map which each key is a node context and the
   corresponding value is T if the node should be evaluated lazily in
   the context, NIL otherwise."

  (labels ((lazy-node? (node)
             (foreach (curry #'lazy? node) (map-values (contexts node))))

           (lazy? (node context)
             "Returns true if NODE should be evaluated lazily in the
              context CONTEXT."

             (unless (and (input-node? node)
                          (= (get :input (contexts node)) context))

               (ensure-get
                context lazy-nodes

                (let ((observers (context-observers node context)))
                  (unless (emptyp observers)
                    (aprog1
                        (every #'lazy-link? observers)
                      (when it (make-async-links observers))))))))

           (lazy-link? (observer)
             "Returns true if the value of the node is only
              conditionally used by the observer."

             (destructuring-bind (observer . link) observer
               (or (not (unconditional-binding? link observer))
                   (->> (node-link-context link)
                        (context observer)
                        (lazy? observer)))))

           (make-async-links (observers)
             "Change the links to the observer nodes to asynchronous
              links."

             (doseq ((obs . link) observers)
               (declare (ignore obs))

               (setf (node-link-node link)
                     (cons 'async (node-link-node link)))))

           (context-observers (node context)
             "Returns a list of the observer nodes of NODE at context
              CONTEXT."

             (with-slots (operands) context
               (remove-if (compose (rcurry #'memberp operands) #'car) (observers node))))

           (unconditional-binding? (link observer)
             "Returns true if the value of the node with link LINK is
              used unconditionally in the value function of OBSERVER."

             (->> link
                  node-link-context
                  (context observer)
                  value-function
                  (has-node link)))

           (has-node (link expression)
             "Returns true if DEPENDENCY is used unconditionally in FN."

             (walk-expression
              (lambda (expression)
                (match expression
                  ((type node-link)
                   (return-from has-node (= link expression)))

                  ((functor-expression-
                    (meta-node (guard (external-meta-node name)
                                      (member name '("and" "or" "if" :key #'id-symbol))))
                    arguments)

                   (return-from has-node (has-node link (first arguments))))

                  (_ t)))
              expression)))

    (foreach #'lazy-node? (nodes table))
    lazy-nodes))
