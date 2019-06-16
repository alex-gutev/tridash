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
             (merge-dependencies node (first (map-keys (observers node))))

             ;; Clear observer, dependency and context sets to prepare
             ;; the node for removal as it is now not reachable from
             ;; any input node
             (clear (observers node))
             (clear (dependencies node))
             (clear (contexts node))))

         (coalesce? (node)
           "Returns true if NODE can be coalesced."

           (and (may-coalesce? node)
                (= (length (observers node)) 1)
                (<= (length (contexts node)) 1)))

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
                     (merge-contexts observer context-id (node-link-context old-link))
                     (setf (node-link-node old-link) link))

                   ;; Add DEPENDENCY to dependencies of OBSERVER
                   (setf (get dependency dependencies) link)
                   (setf (get dependency operands) link)
                   (setf (node-link-context link) context-id)

                   (with-slots (observers) dependency
                     ;; Remove NODE from observers of DEPENDENCY
                     (erase observers node)
                     ;; Add OBSERVER to observers of DEPENDENCY
                     (setf (get observer (observers dependency)) link))))))))

      (foreach #'begin-coalesce input-nodes))))


(defun merge-contexts (node id1 id2)
  "Merges the context with id ID2 into the context with ID1,
   if they are not the same context."

  (flet ((merge-context-functions (context1 context2)
           "Returns a function which is a `CATCH-EXPRESSION' with the
            function of CONTEXT1 as the main expression and the
            function of CONTEXT2 as the expression which is evaluated
            when the main expression fails."

           ;; Wrap both context functions in `NODE-LINK' objects in
           ;; order for them to be wrapped in `EXPRESSION-GROUP'
           ;; objects.

           (catch-expression
            (node-link (value-function context1))
            (node-link (value-function context2)))))

    (unless (= id1 id2)
      (let ((context1 (context node id1))
            (context2 (context node id2)))

        (with-slots (operands) context1
          (doseq ((operand . link) (operands context2))
            (setf (node-link-context link) id1)
            (ensure-get operand operands link)))

        (cond
          ((< (order context1) (order context2))
           (setf (value-function context1)
                 (merge-context-functions context1 context2)))

          (t
           (setf (order context1) (order context2))
           (setf (value-function context1)
                 (merge-context-functions context2 context1))))

        (erase (contexts node) id2)))))

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


(defgeneric coalesce-links-in-expression (expression &key save)
  (:documentation
   "Coalesces node links in the expression EXPRESSION."))

(defmethod coalesce-links-in-expression ((link node-link) &key (save t))
  "If LINK points to an expression, instead of a NODE, returns an
   `EXPRESSION-GROUP' containing the referenced expression."

  (labels ((should-save? (expr)
             "Checks whether the value of the expression should be
              saved, for future value updates."

             (walk-expression
              (lambda (expression)
                (match expression
                  ((type fail-expression)
                   (return-from should-save? t))

                  ((not (type expression-group))
                   t)

                  ((catch-expression catch)
                   (return-from should-save? (should-save? catch)))))
              expr)))

    (with-accessors ((node node-link-node)) link
      (match node
        ((type node-link)
         (coalesce-links-in-expression node :save save))

        ((type expression-group)
         (incf (expression-group-count node))
         node)

        ((not (type node))
         (let ((expr (coalesce-links-in-expression node :save save)))
           (->> (expression-group expr :save (and save (should-save? expr)))
                (setf node))))

        (_ link)))))

(defmethod coalesce-links-in-expression ((catch catch-expression) &key (save t))
  "Coalesces `NODE-LINK's in both the main and catch expression,
   however only the values of the catch expression are saved if
   necessary."

  (catch-expression
   (coalesce-links-in-expression (catch-expression-main catch) :save nil)
   (coalesce-links-in-expression (catch-expression-catch catch) :save save)))

(defmethod coalesce-links-in-expression ((expression t) &key (save t))
  (map-expression! (rcurry #'coalesce-links-in-expression :save save) expression))


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
                   (error 'dependency-not-reachable :dependency node :node it))))

             (visited? (node)
               (and (memberp node visited)
                    node)))

      (foreach #'mark input-nodes)
      (foreach #'sweep nodes))))

(defun fold-constant-nodes (nodes)
  "Removes all constant nodes, from the set NODES, and replaces links
   to the nodes with the constant values. Constant nodes are nodes
   which only have an :INIT context."

  (labels ((value-node? (node)
             (unless (input-node? node)
               (with-slots (contexts) node
                 (and (= (length contexts) 1)
                      (-> contexts
                          first
                          cdr
                          operands
                          emptyp)))))

           (fold-value (node)
             (when (value-node? node)
               (let ((obs (observers node)))
                 (-> (contexts node)
                     first
                     cdr
                     value-function
                     (replace-dependency node))

                 (foreach #'fold-value (map-keys obs))
                 (clear (observers node)))))

           (replace-dependency (value node)
             (doseq ((obs . link) (observers node))
               (erase (dependencies obs) node)
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
                    (error 'node-cycle-error :node node)))))

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
