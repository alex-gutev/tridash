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


;;;; Coalescing

;;; Coalescing Nodes

(defun coalesce-all (table)
  "Perform all coalescing steps on the `FLAT-NODE-TABLE' TABLE:
   coalesce nodes, check structure, remove unreachable nodes and
   coalesce `NODE-LINK' objects."

  (with-slots (nodes input-nodes) table
    ;; Coalesce successive nodes with a single observer
    (coalesce-nodes input-nodes)

    ;; Check for cycles and coalesce multiple contexts activated by a
    ;; common ancestor node.
    (check-structure input-nodes)

    ;; Fold Constant Nodes
    (fold-constant-nodes nodes)

    ;; Remove nodes not reachable from any input node.
    (remove-unreachable-nodes input-nodes nodes)

    ;; Replace node linkes which no longer point to actual nodes with
    ;; expression blocks
    (coalesce-node-links nodes)))

(defun coalesce-nodes (input-nodes)
  "Coalesces successive nodes, which only have a single observer, into
   single nodes. INPUT-NODES is the set of all INPUT-NODES."

  (let ((visited (make-hash-set)))
    (labels
        ((begin-coalesce (node)
           "Begins node coalescing starting from the node NODE. Clears
            the visited set."

           (clear visited)
           (coalesce-observers node))

         (coalesce-observers (node)
           "Performs node coalescing on the observer nodes of NODE."

           (nadjoin node visited)
           (foreach #'coalesce-node (map-keys (observers node))))

         (coalesce-node (node)
           "Coalesces the node NODE (into its observer node) if it
            only has a single observer, after node coalescing is
            performed on its observer nodes and redundant 2-way
            bindings are removed."

           (unless (memberp node visited)
             (nadjoin node visited)
             (coalesce-observers node)

             (unless (input-node? node)
               (remove-redundant-2-way-links node)
               (eliminate-node node))

             (erase visited node)))

         (remove-redundant-2-way-links (node)
           (when (= (length (contexts node)) 1)
             (foreach (curry #'remove-observer node) (map-keys (dependencies node)))))


         (eliminate-node (node)
           "Removes the node NODE if it has a single observer. Its
            dependencies are merged into the dependency set of its
            observer node, it's replaced with its observer node in the
            observer sets of its dependencies, and the node referenced
            within the `node-link' objects of its observer nodes, is
            replaced with the value-function of NODE."

           (when (coalesce? node)
             (aif (first (map-keys (observers node)))
                  (merge-dependencies node it)
                  (remove-from-dependencies node))

             ;; Clear observer, dependency and context sets to prepare
             ;; the node for removal as it is now not reachable from
             ;; any input node
             (clear (observers node))
             (clear (dependencies node))
             (clear (contexts node))))

         (coalesce? (node)
           "Returns true if NODE can be coalesced."

           (or
            (and (may-coalesce? node)
                 (= (length (observers node)) 1)
                 (<= (length (contexts node)) 1))

            (and *meta-node*
                 (not (= node *meta-node*))
                 (emptyp (observers node)))))

         (merge-dependencies (node observer)
           "Merges the dependencies of NODE into the dependency set of
            OBSERVER. NODE is removed from the dependency set of
            OBSERVER and the observer sets of its
            dependencies. References to NODE within the value function
            of OBSERVER are replaced with the value function of NODE,
            this assumes that NODE only has a single value function."

           (with-slots (dependencies) observer
             (let* ((context (first (map-values (contexts node))))
                    (link (get node dependencies))
                    (context-id (node-link-context link)))

               (with-slots (operands value-function) (context observer context-id)
                 ;; Update link NODE -> OBSERVER to store VALUE-FUNCTION of NODE
                 (setf (node-link-node link) (value-function context))

                 ;; Remove NODE from DEPENDENCIES of OBSERVER
                 (erase dependencies node)
                 (erase operands node)

                 ;; Add all dependencies of NODE to dependencies of OBSERVER
                 (doseq ((dependency . link) (dependencies node))

                   ;; Check if OBSERVER already has DEPENDENCY as a dependency
                   (when-let ((old-link (get dependency dependencies)))
                     (unless (get dependency operands)
                       (error 'ambiguous-context-error :node observer))

                     (setf (node-link-node old-link) link))

                   ;; Add DEPENDENCY to dependencies of OBSERVER
                   (setf (get dependency dependencies) link)
                   (setf (get dependency operands) link)
                   (setf (node-link-context link) context-id)

                   (with-slots (observers) dependency
                     ;; Remove NODE from observers of DEPENDENCY
                     (erase observers node)
                     ;; Add OBSERVER to observers of DEPENDENCY
                     (setf (get observer (observers dependency)) link)))))))

         (remove-from-dependencies (node)
           "Removes NODE from the observer set of each of its
            dependency nodes."

           (doseq (dep (map-keys (dependencies node)))
             (erase (observers dep) node))))

      (foreach #'begin-coalesce input-nodes))))

(defun may-coalesce? (node)
  "Returns true if NODE may be coalesced into another node. Returns
   false if the node has the :NO-COALESCE attribute set to T."

  (null (attribute :no-coalesce node)))


;;; Coalescing Node Links

(defun coalesce-node-links (nodes)
  "Replaces the `node-link' objects, within the value functions of
   NODE, which do not directly reference another node, with their
   contents. NODES is the set of all nodes."

  (flet ((coalesce-links-in-node (node)
           (with-slots (contexts) node
             (doseq ((id . context) contexts)
               (declare (ignore id))

               (with-slots (value-function) context
                 (setf value-function (coalesce-links-in-expression value-function)))))))

    (foreach #'coalesce-links-in-node nodes)))


(defgeneric coalesce-links-in-expression (expression)
  (:documentation
   "Coalesces node links in the expression EXPRESSION."))

(defmethod coalesce-links-in-expression ((link node-link))
  "If LINK points to an expression, instead of a NODE, returns an
   `EXPRESSION-BLOCK' containing the referenced expression."

  (with-accessors ((node node-link-node)) link
    (match node
      ((type node-link)
       (coalesce-links-in-expression node))

      ((type expression-block)
       (incf (expression-block-count node))
       node)

      ((not (type node))
       (->> (coalesce-links-in-expression node)
            expression-block
            (setf node)))

      (_ link))))

(defmethod coalesce-links-in-expression ((expression t))
  (map-expression! #'coalesce-links-in-expression expression))


;;;; Constant Folding and Removing Redundant Nodes

(defun remove-unreachable-nodes (input-nodes nodes)
  "Removes all unreachable nodes from NODES. An unreachable node is a
   node which is not reachable from any node in INPUT-NODES."

  (let ((visited (make-hash-set)))
    (labels ((mark (node)
               (unless (visited? node)
                 (nadjoin node visited)
                 (foreach #'mark (map-keys (observers node)))))

             (sweep (node)
               (unless (or (visited? node) (attribute :no-remove node))
                 (erase nodes node)

                 (awhen (some #'visited? (map-keys (observers node)))
                   (error 'dependency-not-reachable-error :dependency node :node it))))

             (visited? (node)
               (and (memberp node visited)
                    node)))

      (foreach #'mark input-nodes)
      (foreach #'sweep nodes))))

(defun remove-unused-meta-nodes (nodes meta-nodes)
  "Returns the set of `META-NODE's which are actually used. A
   meta-node is considered used if it appears as the operator of a
   `FUNCTOR-EXPRESSION' or is referenced by a `META-NODE-REF' in the
   value computation function of at least one node in NODES or in the
   function of a used `META-NODE' in META-NODES."

  (let ((visited (make-hash-set))
        (used (make-hash-set)))

    (labels ((visit-node (node)
               "Visits NODE and marks all meta-nodes, appearing with
                the value-function of each context, as used."

               (unless (visited? node)
                 (nadjoin node visited)

                 (-> #'visit-context
                     (foreach (map-values (contexts node))))

                 (foreach #'visit-node (map-keys (observers node)))))

             (visit-meta-node (node)
               "Visits `META-NODE' NODE and marks all meta-nodes,
                appearing with the meta-node's function as used."

               (unless (visited? node)
                 (nadjoin node visited)
                 (-> node
                     contexts
                     first
                     cdr
                     visit-context)))

             (visit-context (context)
               "Visits the context CONTEXT and marks the meta-nodes
                appearing in its VALUE-FUNCTION as used."

               (with-slots (value-function) context
                 (when context
                   (mark-used-meta-nodes value-function))))

             (visited? (node)
               "Returns true if NODE has been visited."
               (and (memberp node visited)
                    node))

             (mark-used-meta-nodes (expression)
               "Marks the meta-nodes appearing in EXPRESSION as used."

               (walk-expression #'mark-meta-nodes expression))

             (mark-meta-nodes (expression)
               "If EXPRESSION is a `FUNCTOR-EXPRESSION' or a
                `META-NODE-REF', marks the meta-node as used."

               (match expression
                 ((functor-expression- (meta-node (and (type meta-node) meta-node)))
                  (add-used-meta-node meta-node))

                 ((meta-node-ref- (node))
                  (add-used-meta-node node)))
               t)

             (add-used-meta-node (meta-node)
               "Adds META-NODE to the USED set and visits its."

               (nadjoin meta-node used)
               (visit-meta-node meta-node))

             (visit-if-no-remove (meta-node)
               "Adds META-NODE to the USED set if it has the
                :NO-REMOVE attribute set to true."

               (when (attribute :no-remove meta-node)
                 (add-used-meta-node meta-node))))

      (foreach #'visit-node nodes)
      (foreach #'visit-if-no-remove meta-nodes)
      used)))

(defun fold-constant-nodes (nodes)
  "Removes all constant nodes, from the set NODES, and replaces links
   to the nodes with the constant values. Constant nodes are nodes
   which only have an :INIT context."

  (labels ((value-node? (node)
             (unless (input-node? node)
               (let ((contexts (remove-if (curry #'coalescable? node)
                                          (map-values (contexts node)))))
                 (and (= (length contexts) 1)
                      (constant-context? (first contexts))
                      (value-function (first contexts))))))

           (coalescable? (node context)
             (with-slots (observers) node
               (with-slots (operands) context
                 (unless (or (emptyp operands)
                             (emptyp observers))
                   (-> (rcurry #'memberp operands)
                       (every (map-keys observers)))))))

           (constant-context? (context)
             (emptyp (operands context)))

           (fold-value (node)
             (awhen (value-node? node)
               (let ((obs (observers node)))
                 (replace-dependency it node)

                 (foreach #'fold-value (map-keys obs))
                 (clear (observers node))
                 (clear (contexts node)))))

           (replace-dependency (value node)
             (doseq ((obs . link) (observers node))
               (erase (dependencies obs) node)
               (erase (observers obs) node)

               (setf (node-link-node link) value)

               (let ((context (context obs (node-link-context link))))
                 (erase (operands context) node)))))

    (foreach #'fold-value (remove-if-not #'value-node? nodes))))


;;;; Structure Checking

(defun check-structure (input-nodes)
  "Checks the structure of all nodes ensuring there are no cycles and
   no ambiguous contexts. If a cycle or ambiguous context is detected,
   an error condition is signaled. INPUT-NODES is the set of all input
   nodes."

  (let ((visited (make-hash-map)))
    (labels ((begin-walk (node)
               "Clears the visited set and begins the traversal at
                NODE in the :INPUT context."

               (clear visited)
               (visit node (context node :input)))

             (visit (node context)
               "Visits node NODE in the context CONTEXT."

               (aif (get node visited)
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

                   ((/= old-context new-context)
                    (error 'ambiguous-context-error :node node)))))

             (visit-observers (node context)
               "Visits the observers of NODE in the context CONTEXT."

               ;; Mark Temporarily
               (setf (get node visited) (list t context))

               (with-slots (operands) context
                 (foreach #'visit-observer (remove-if (compose (rcurry #'memberp operands) #'car) (observers node))))

               ;; Mark Permanently
               (setf (car (get node visited)) nil))

             (visit-observer (observer)
               (destructuring-bind (observer . link) observer
                 (visit observer (context observer (node-link-context link))))))

      (foreach #'begin-walk input-nodes))))
