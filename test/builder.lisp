;;;; builder.lisp
;;;;
;;;; Tridash Programming Language.
;;;; Copyright (C) 2019  Alexander Gutev
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

;;;; Unit tests for the graph builder

(in-package :tridash.test)

(cl-interpol:enable-interpol-syntax)

(plan nil)


(subtest "Test Node Builder"
  (labels ((build-nodes (string modules)
             (with-input-from-string (in string)
               (build-parsed-nodes (make-parser in) modules)))

           (get-node (name node-table)
             "Returns the node with identifier NAME in
              NODE-TABLE. Checks that such a node actually exists and
              that its NAME slot has the correct identifier."

             (diag (format nil "Check node ~a" name))
             (with-slots (nodes) node-table
               (let* ((id (node-id name))
                      (node (gethash id nodes)))
                 (is-type node 'node (format nil "Node ~a created" name))
                 (is (name node) id (format nil "Name of ~a is ~a" name name))

                 node)))

           (node-id (name)
             "Converts a string/list node identifier to an identifier
              symbol."

             (typecase name
               (cons (mapcar #'node-id name))
               (otherwise (id-symbol name))))

           (test-binding (src target &optional (context src))
             "Tests that a binding between node SRC and TARGET has
              been established in the context CONTEXT."

             (diag (format nil "Test binding ~a -> ~a" (name src) (name target)))

             (let ((link (gethash target (observers src))))
               (is-type link 'node-link "Node link created")
               (is link (gethash src (dependencies target))
                   (format nil "Node link added to dependencies of ~a" (name target)))

               (is (node-link-node link) src "Node link points to correct node")
               (is (node-link-context link) context "Link context is correct")

               (let ((id context)
                     (context (gethash context (contexts target))))
                 (is-type context 'node-context (format nil "Node context ~a created" id))
                 (is (gethash src (operands context)) link
                     (format nil "~a added to context operands" (name src))))

               link))

           (value-fn-equal (a b)
             "Value function equality comparison. Returns true if the
              value function A is equivalent to the value function B."

             (multiple-value-match (values a b)
               (((cons a as) (cons b bs))
                (and (value-fn-equal a b)
                     (value-fn-equal as bs)))

               (((node-link- (node node-a) (context context-a))
                 (node-link- (node node-b) (context context-b)))

                (and (eq node-a node-b)
                     (eq context-a context-b)))

               ((_ _) (equal a b))))

           (test-value-function (node context fn)
             "Tests that the context CONTEXT of node NODE has value
              function FN."

             (diag (format nil "Test value function of ~a in context ~a" (name node) context))

             (let ((context (gethash context (contexts node))))
               (is (value-function context) fn :test #'value-fn-equal))))

    (subtest "Simple Atom Nodes"
      (let ((modules (make-instance 'module-table)))
        (build-nodes "a;b;c" modules)

        (with-slots (node-table) modules
          (get-node "a" node-table)
          (get-node "b" node-table)
          (get-node "c" node-table))))

    (subtest "Bindings"
      (let ((modules (make-instance 'module-table)))
        (build-nodes "a -> b; c -> b" modules)

        (with-slots (node-table) modules
          (let ((a (get-node "a" node-table))
                (b (get-node "b" node-table))
                (c (get-node "c" node-table)))

            (->> (test-binding a b)
                 (test-value-function b a))

            (->> (test-binding c b)
                 (test-value-function b c)))))

      (let ((modules (make-instance 'module-table)))
        (build-nodes "a -> (b -> c)" modules)

        (with-slots (node-table) modules
          (let ((a (get-node "a" node-table))
                (b (get-node "b" node-table))
                (c (get-node "c" node-table))
                (b->c (get-node '("->" "b" "c") node-table)))

            (->> (test-binding a b->c)
                 (test-value-function b->c a))

            (->>
             `(if ,(test-binding b->c c b)
                  ,(test-binding b c)
                  ,(node-link :self))
             (test-value-function c b))))))

    (subtest "Functor Nodes"
      (let ((modules (make-instance 'module-table)))
        (build-source-file #p"./modules/core.trd" modules)
        (build-nodes "a + b -> output; int(x) -> z" modules)

        (with-slots (node-table) modules
          (let ((fn (gethash (id-symbol "+") (meta-nodes node-table)))
                (a (get-node "a" node-table))
                (b (get-node "b" node-table))
                (a+b (get-node '("+" "a" "b") node-table))
                (output (get-node "output" node-table)))

            (test-binding a+b output)

            (->>
             `(,fn
               ,(test-binding a a+b fn)
               ,(test-binding b a+b fn))
             (test-value-function a+b fn)))

          (let ((fn (gethash (id-symbol "int") (meta-nodes node-table)))
                (x (get-node "x" node-table))
                (int-x (get-node '("int" "x") node-table))
                (z (get-node "z" node-table)))

            (test-binding int-x z)

            (->> `(,fn ,(test-binding x int-x fn))
                 (test-value-function int-x fn))

            (->> `(,fn ,(test-binding int-x x))
                 (test-value-function x int-x))))))

    (subtest "Special Operators"
      (subtest "op Operator - Registering Infix Operators"
        (flet ((test-op (id operators prec assoc)
                 (diag (format nil "Test infix operator ~a" id))

                 (let ((op (gethash (id-symbol id) operators)))
                   (ok op "Operator registered")
                   (is (length op) 2 "Operator details added")
                   (is (first op) prec "Operator precedence")
                   (is (second op) assoc "Operator associativity"))))

          (let ((modules (make-instance 'module-table)))
            (build-nodes ":op(+, 50, left); :op(-, 70); :op(*, 100, right)" modules)

            (with-slots (operator-nodes) (node-table modules)

              (test-op "+" operator-nodes 50 :left)
              (test-op "-" operator-nodes 70 :right)
              (test-op "*" operator-nodes 100 :right))))

        (subtest "Errors"
          (flet ((test-error (str)
                   (is-error (build-nodes str (make-instance 'module-table)) 'semantic-error)))

            (test-error ":op()")
            (test-error ":op(*)")
            (test-error ":op(*,*)")
            (test-error ":op(*,9,x)")
            (test-error ":op(\"*\", 10, left)")
            (test-error "a -> :op(*, 10, left)")
            (test-error ":op(*, 10, left) -> a"))))

      (subtest "Meta-Node Definitions"
        (let ((modules (make-instance 'module-table)))
          (build-source-file #p"./modules/core.trd" modules)
          (build-nodes "add(x,y) : x + y; add(a,b)" modules)

          (with-slots (node-table) modules
            (let ((add (gethash (id-symbol "add") (meta-nodes node-table)))
                  (a (get-node "a" node-table))
                  (b (get-node "b" node-table))
                  (add-ab (get-node '("add" "a" "b") node-table)))

              (is-type add 'meta-node)
              (is (operands add) (decls '!\x '!\y))
              (is (definition add) (decls '(!+ !\x !\y)))

              (->>
               `(,add ,(test-binding a add-ab add)
                      ,(test-binding b add-ab add))
               (test-value-function add-ab add)))))))))

(finalize)

(cl-interpol:disable-interpol-syntax)
