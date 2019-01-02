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
  (labels ((get-node (name node-table)
             "Returns the node with identifier NAME in
              NODE-TABLE. Checks that such a node actually exists and
              that its NAME slot has the correct identifier."

             (with-slots (nodes) node-table
               (let* ((id (node-id name))
                      (node (gethash id nodes)))
                 (is-type node 'node)
                 (is (name node) id)

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

             (let ((link (gethash target (observers src))))
               (is-type link 'node-link)
               (is link (gethash src (dependencies target)))

               (is (node-link-node link) src)
               (is (node-link-context link) context)

               (let ((context (gethash context (contexts target))))
                 (is (gethash src (operands context)) link))

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

             (let ((context (gethash context (contexts node))))
               (is (value-function context) fn :test #'value-fn-equal))))

    (subtest "Simple Atom Nodes"
      (with-input-from-string (in "a;b;c")
        (let ((modules (make-instance 'module-table)))
          (build-parsed-nodes (make-parser in) modules)

          (with-slots (node-table) modules
            (get-node "a" node-table)
            (get-node "b" node-table)
            (get-node "c" node-table)))))

    (subtest "Bindings"
      (with-input-from-string (in "a -> b; c -> b")
        (let ((modules (make-instance 'module-table)))
          (build-parsed-nodes (make-parser in) modules)

          (with-slots (node-table) modules
            (let ((a (get-node "a" node-table))
                  (b (get-node "b" node-table))
                  (c (get-node "c" node-table)))

              (->> (test-binding a b)
                   (test-value-function b a))

              (->> (test-binding c b)
                   (test-value-function b c))))))

      (with-input-from-string (in "a -> (b -> c)")
        (let ((modules (make-instance 'module-table)))
          (build-parsed-nodes (make-parser in) modules)

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
               (test-value-function c b)))))))

    (subtest "Functor Nodes"
      (let ((modules (make-instance 'module-table)))
        (build-source-file #p"./modules/core.trd" modules)

        (with-input-from-string (in "a + b -> output")
          (build-parsed-nodes (make-parser in) modules)

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
               (test-value-function a+b fn)))))))))

(finalize)

(cl-interpol:disable-interpol-syntax)
