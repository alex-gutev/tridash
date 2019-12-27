;;;; coalescer.lisp
;;;;
;;;; Tridash Programming Language.
;;;; Copyright (C) 2018-2019  Alexander Gutev
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



;;; Node Coalescing

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

    ;; Replace node links which no longer point to actual nodes with
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
             (foreach (curry #'remove-observer node) (map-keys (dependencies node)))

             (doseq (link (map-values (dependencies node)))
               (setf (node-link-two-way-p link) nil))))


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
                 (strong-link? node)
                 (<= (length (contexts node)) 1))

            (and (attribute :non-coalescable-self-reference node)
                 (no-self-references? node))

            (and *meta-node*
                 (not (= node *meta-node*))
                 (emptyp (observers node)))))

         (no-self-references? (node)
           (->> node
                contexts
                map-values
                (notany (curry #'self-references node))))

         (self-references (node context)
           (walk-expression
            (lambda (expression)
              (match expression
                ((functor-expression-
                  (meta-node (eq (get :previous-value *core-meta-nodes*)))
                  (arguments (list (eq node))))

                 (return-from self-references nil))

                (_ t)))

            (value-function context))
           t)

         (strong-link? (node)
           "Returns true if NODE is bound to each of its observers via
            a strong (not weak) binding."

           (-> node
               observers
               first
               cdr
               node-link-weak-p
               not))

         (merge-dependencies (node observer)
           "Merges the dependencies of NODE into the dependency set of
            OBSERVER. NODE is removed from the dependency set of
            OBSERVER and the observer sets of its
            dependencies. References to NODE within the value function
            of OBSERVER are replaced with the value function of NODE,
            this assumes that NODE only has a single value function."

           (with-slots (dependencies) observer
             (let* ((context (first (map-values (contexts node))))
                    (node-link (get node dependencies))
                    (context-id (node-link-context node-link)))

               (with-slots (operands value-function) (context observer context-id)
                 ;; Update link NODE -> OBSERVER to store VALUE-FUNCTION of NODE
                 (setf (node-link-node node-link) (value-function context))

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

                   (cond
                     ((= dependency observer)
                      ;; DEPENDENCY and OBSERVER are the same node.
                      ;; The link (DEPENDENCY -> OBSERVER) has to be
                      ;; replaced with the value function of OBSERVER.

                      (setf (node-link-node link)
                            (->> (node-link-context node-link)
                                 (context observer)
                                 value-function
                                 cyclic-reference))

                      (erase (observers dependency) node))

                     (t

                      ;; Add DEPENDENCY to dependencies of OBSERVER
                      (setf (get dependency dependencies) link)
                      (setf (get dependency operands) link)
                      (setf (node-link-context link) context-id)

                      (with-slots (observers) dependency
                        ;; Remove NODE from observers of DEPENDENCY
                        (erase observers node)
                        ;; Add OBSERVER to observers of DEPENDENCY
                        (setf (get observer (observers dependency)) link)))))))))

         (remove-from-dependencies (node)
           "Removes NODE from the observer set of each of its
            dependency nodes."

           (doseq (dep (map-keys (dependencies node)))
             (erase (observers dep) node))))

      (foreach #'begin-coalesce input-nodes))))

(defun may-coalesce? (node)
  "Returns true if NODE may be coalesced into another node. Returns
   false if the node has the :NO-COALESCE attribute set to T."

  (or *meta-node* ; Ignore NO-COALESCE when in meta-node
      (null (attribute :no-coalesce node))))


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
       (let ((expression node)
             (block (setf node (expression-block nil))))
         (setf (expression-block-expression block)
               (coalesce-links-in-expression expression))
         block))

      (_ link))))

(defmethod coalesce-links-in-expression ((cycle cyclic-reference))
  "Coalesces `NODE-LINK' objects in the EXPRESSION of the cyclic
   reference CYCLE. Since the EXPRESSION part of the cyclic reference
   has already been visited (as CYCLE is contained in it), the
   `NODE-LINK' in it will already point to an `EXPRESSION-BLOCK'
   object with which it will be replaced"

  (-> cycle
      cyclic-reference-expression
      coalesce-links-in-expression
      cyclic-reference))

(defmethod coalesce-links-in-expression ((expression t))
  (map-expression! #'coalesce-links-in-expression expression))



;;; Constant Folding and Removing Redundant Nodes

(defun remove-unreachable-nodes (input-nodes nodes)
  "Removes all unreachable nodes from NODES. An unreachable node is a
   node which is not reachable from any node in INPUT-NODES."

  (let ((visited (make-hash-set)))
    (labels ((mark (node)
               (unless (visited? node)
                 (nadjoin node visited)
                 (foreach #'mark (map-keys (observers node)))))

             (sweep (node)
               (unless (or (visited? node)
                           (attribute :no-remove node)
                           (meta-node? node))
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
               "Visits NODE and marks all meta-nodes, appearing within
                the value-function of each context, as used."

               (unless (visited? node)
                 (nadjoin node visited)

                 (-> #'visit-context
                     (foreach (map-values (contexts node))))

                 (foreach #'visit-node (map-keys (observers node)))))

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

             (visit-meta-node (node)
               "Visits `META-NODE' NODE and marks all meta-nodes,
                appearing within the meta-node's function as used."

               (unless (visited? node)
                 (nadjoin node visited)
                 (-> node
                     contexts
                     first
                     cdr
                     visit-context)))

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

  (foreach #'fold-value (remove-if-not #'value-node? nodes)))

(defun value-node? (node)
  "Returns true if NODE is a constant node."

  (flet ((coalescable? (node context)
           (with-slots (observers) node
             (with-slots (operands) context
               (unless (or (emptyp operands)
                           (emptyp observers))
                 (-> (rcurry #'memberp operands)
                     (every (map-keys observers)))))))

           (constant-context? (context)
             (emptyp (operands context))))

    (unless (input-node? node)
      (let ((contexts (remove-if (curry #'coalescable? node)
                                 (map-values (contexts node)))))
        (and (= (length contexts) 1)
             (constant-context? (first contexts))
             (value-function (first contexts)))))))

(defun fold-value (node)
  "If NODE is a constant node, replace the `NODE-LINK's, in its
   observer nodes, with its constant value. The contexts and observers
   of NODE are cleared in order for it to be removed during the
   removal of unreachable nodes."

  (flet ((replace-dependency (value node)
           (doseq ((obs . link) (observers node))
             (erase (dependencies obs) node)
             (erase (observers obs) node)

             (setf (node-link-node link) value)

             (let ((context (context obs (node-link-context link))))
               (fold-constant-outer-node-operands context)
               (erase (operands context) node)))))

    (awhen (value-node? node)
      (with-slots (observers) node
        (unless (emptyp observers)
          (replace-dependency it node)

          (foreach #'fold-value (map-keys observers))
          (clear (observers node))
          (clear (contexts node)))))))


;;; Folding Constant Outer Node References

(defun fold-constant-outer-node-operands (context)
  "Marks outer nodes as constants, in the meta-nodes from which they
   are referenced, in each meta-node instance of the value function of
   CONTEXT."

  (labels ((fold-in-expression (expression)
             (match expression
               ((functor-expression- (meta-node (and (type meta-node) meta-node))
                                     (arguments (place arguments))
                                     (outer-nodes (place outer-nodes)))

                (setf outer-nodes (mark-constants meta-node outer-nodes)))

               ((meta-node-ref- node outer-nodes)
                (setf outer-nodes (mark-constants node outer-nodes))))
             t)

           (mark-constants (meta-node outer-nodes)
             (remove-if (curry #'mark-argument meta-node) outer-nodes))

           (mark-argument (meta-node outer-node)
             (destructuring-bind (node . expression) outer-node
               (when (constant-expression? expression)
                 (mark-constant-outer-node meta-node node expression)
                 t)))

           (constant-expression? (expression)
             (walk-expression
              (lambda (expression)
                (match expression
                  ((node-link- (node (type node)))
                   (return-from constant-expression? nil)))
                t)
              expression)
             t))

    (walk-expression #'fold-in-expression (value-function context))))

(defgeneric mark-constant-outer-node (meta-node node value)
  (:documentation
   "Marks the local node, which is bound to the value of the outer
    node NODE as a constant node with value VALUE."))

(defmethod mark-constant-outer-node ((meta-node built-meta-node) node value)
  (with-slots (definition outer-nodes attributes) meta-node
    (when-let ((local-node (get node outer-nodes)))
      (with-slots (input-nodes) definition
        (when (input-node? local-node)
          ;; Remove node from inputs
          (erase input-nodes local-node)
          (erase (contexts local-node) :input)

          (setf (attribute :input local-node) nil)
          (setf (value-function (context local-node :init)) value)

          local-node)))))

(defmethod mark-constant-outer-node ((meta-node final-meta-node) node value)
  (declare (ignore value))

  (with-slots (outer-nodes attributes definition) meta-node
    (with-slots (nodes input-nodes) definition
      (awhen (call-next-method)
        (erase outer-nodes node)

        (slet (get :outer-operands attributes)
          (setf it (remove node it :key #'car)))

        (fold-value it)

        ;; Remove constant nodes which have been folded
        (remove-unreachable-nodes input-nodes nodes)

        ;; Coalesce node links, pointing to expressions rather than
        ;; nodes, which are a result of folding the current node.
        (coalesce-node-links nodes)))))


(defun remove-constant-outer-nodes (meta-node)
  "Removes constant nodes from the outer node references of
   META-NODE."

  (with-slots (outer-nodes attributes) meta-node
    (setf outer-nodes (remove-if-not #'input-node? outer-nodes :key #'cdr))

    (slet (get :outer-operands attributes)
      (setf it (remove-if-not #'input-node? it :key #'cdr)))))



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
               "Checks that if node is visited for a second time it is
                visited in the same context."

               (destructuring-bind (temp? old-context) marker
                 (when (/= old-context new-context)
                   (error 'ambiguous-context-error :node node))))

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
