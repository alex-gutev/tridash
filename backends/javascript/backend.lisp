;;;; backend.lisp
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

;;;; JavaScript Backend

(in-package :metalink.backend.js)


(defconstant +node-class+ "MetaLinkNode"
  "Runtime node class name.")

(defvar *node-table-var* "node_table"
  "Global node table variable.")


(defvar *node-link-indices* nil
  "Hash-table storing the dependency indices of each dependency of
   each node. Each key is a `NODE' object and the corresponding value
   is a hash-table mapping `NODE-LINK' objects (of the dependency
   nodes) to their dependency index.")

(defvar *meta-node-ids*
  "Hash-table mapping `META-NODE' objects to global meta-node function
   identifiers.")

(defvar *meta-node-types* (make-hash-table :test #'eq))


;;;; Compilation

(defmethod compile-nodes ((backend (eql :javascript)) table)
  "Compile the node and meta-node definitions, in the `NODE-TABLE'
   TABLE, to JavaScript."

  (let ((*node-link-indices* (make-hash-table :test #'eq))
        (*meta-node-ids* (make-hash-table :test #'eq)))

    (output-code (make-preamble))
    (output-code (generate-code table))))


(defvar *lazy-nodes* nil
  "Hash-table of nodes which should be evaluated lazily. If a node
  should be evaluated it is present in the table with the
  corresponding value T.")

(defun lazy-node? (node)
  "Returns true if NODE should be evaluated lazily. NODE should be
   evaluated lazily if it is present in *LAZY-NODES* (with value T)
   and has a non-NIL value function."

  (and (gethash node *lazy-nodes*)
       (value-function node)))

(defun generate-code (table)
  "Generates the JavaScript code for the node and meta-node
   definitions in the `NODE-TABLE' TABLE."

  (let ((*lazy-nodes* (find-lazy-nodes table)))
    (with-slots (nodes meta-nodes) table
      (list

       (create-nodes nodes)
       (create-meta-nodes meta-nodes)

       (init-nodes nodes)))))

(defun make-preamble ()
  "Creates the code which should appear before any node
   definitions. Currently this contains only the declaration of the
   node table variable."

  (js-var *node-table-var* (js-object)))


;;;; Creating nodes

(defun create-nodes (nodes)
  "Generate the node creation code of each `NODE' in NODES."

  (loop
     for node being the hash-value of nodes
     append (create-node node)))

(defun create-node (node)
  "Generate the node creation code of NODE. This includes the creation
   of the node, its dependency queues and its value computation
   function, however it does not include the binding of the node to
   its observers."

  (let ((path (node-path node)))
    (establish-dependency-indices node)

    (list
     (js-call '= path (js-new +node-class+))

     (aand (dependencies-count node)
           (plusp it)
           (js-call (js-member path "make_dependencies") it))

     (awhen (create-compute-function node)
       (js-call '= (js-member path "compute") it)))))

(defun establish-dependency-indices (node)
  "Establishes the dependency indices of the dependencies of NODE, and
   adds them to *NODE-LINK-INDICES*."

  (dohash (dependency link (dependencies node))
    (dependency-index node link)))

(defun dependency-index (node link)
  "Returns the dependency index of the dependency (of NODE)
   corresponding to the `NODE-LINK' object LINK. If LINK does not have
   a dependency index, a new index is assigned to it."

  (let ((links (ensure-gethash node *node-link-indices* (make-hash-table :test #'eq))))
    (ensure-gethash link links (hash-table-count links))))


;;;; Creating meta-nodes

(defun create-meta-nodes (meta-nodes)
  "Generates the meta-node functions of each `META-NODE' in META-NODES."

  (loop
     for meta-node being the hash-value of meta-nodes
     collect (create-meta-node meta-node)))

(defun create-meta-node (meta-node)
  "Generates the meta-node function of META-NODE."

  (with-slots (definition operands value-function) meta-node
    (case (meta-node-type meta-node)
      (sync
       (create-function-meta-node meta-node))
      (async
       (create-async-meta-node meta-node)))))


(defun async-mete-node? (meta-node)
  (eq (meta-node-type meta-node) 'async))

(defgeneric meta-node-type (meta-node)
  (:documentation
   "Returns SYNC if the meta-node computes its value synchronously,
    ASYNC if the meta-node computes its value asynchronously.")

  (:method ((meta-node symbol)) 'sync))

(defmethod meta-node-type ((meta-node meta-node))
  "Returns SYNC if `META-NODE' has no subnodes other than its operand
   nodes, that is the entire subgraph has been coalesced into the
   value function. Returns ASYNC if it has other subnodes besides the
   operand nodes."

  (with-slots (value-function operands definition) meta-node
    (ensure-gethash
     meta-node
     *meta-node-types*

     (cond
       ;; If meta-node has no subnodes other than the operand nodes, it
       ;; can be coalesced to a single function
       ((and value-function
             (= (hash-table-count (nodes definition))
                (length operands)))

        'sync)

       (t 'async)))))


;;;; Generate dispatch methods

(defun init-nodes (nodes)
  "Generates the initialization code of each `NODE' in NODES."

  (loop
     for node being the hash-value of nodes
     append (init-node node)))

(defun init-node (node)
  "Generates the initialization code of NODE. This includes the
   binding of the node to its observers and the initialization of the
   wait set."

  (list
   (bind-observers node)
   (init-wait-set node)))

(defun bind-observers (node)
  "Generates code which binds the `NODE' NODE to its observers."

  (with-slots (observers) node
    (js-call
     (js-member (node-path node) "add_observers")
     (js-array
      (iter (for (observer link) in-hashtable (observers node))
            (for index = (dependency-index observer link))
            (collect (list (js-array (list (node-path observer) index)))))))))

(defun init-wait-set (node)
  "Generates the wait-set initialization code of the `NODE' NODE."

  (let ((wait-nodes (remove-duplicates (flatten (hash-table-values (wait-set node))))))
    (when wait-nodes
      (js-call '=
               (js-member (node-path node) "wait_nodes")
               (js-array (mapcar #'node-path wait-nodes))))))
