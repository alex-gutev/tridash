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

(defun node-id (name)
  "Converts a string/list node identifier to an identifier symbol."

  (typecase name
    (cons (mapcar #'node-id name))
    (otherwise (id-symbol name))))

(defun test/get-node (name node-table)
  "Retrieves the node with name NAME from NODE-TABLE and checks that
   its NAME slot matches NAME."

  (diag (format nil "Check node ~a" name))

  (let ((id (node-id name)))
    (aprog1 (gethash id (all-nodes node-table))
      (is-type it 'node "Is Node")
      (is (name it) id "Node Name"))))

(defmacro! with-nodes ((&rest nodes) o!modules &body body)
  "Binds the nodes to variables. Each element of NODES is of the
   form (VAR NAME) where VAR is the variable to which the node is
   bound and NAME designates the node's name."

  (flet ((make-binding (node)
           (destructuring-bind (var name) node
             `(,var (test/get-node ',(node-id name) (node-table ,g!modules))))))
    `(let ,(mapcar #'make-binding nodes)
       ,@body)))

(subtest "Test Node Builder"
  (labels ((build-nodes (string modules)
             (with-input-from-string (in string)
               (build-parsed-nodes (make-parser in) modules)))

           (test-nodes (modules &rest nodes)
             (mapc (rcurry #'test/get-node (node-table modules)) nodes))

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
               (is (value-function context) fn :test #'value-fn-equal)))

           (test-node-function (node context fn &rest operands)
             "Tests the node NODE has a context CONTEXT with the value
              function being FN applied to operands OPERANDS."

             (->>
              (list* fn (mapcar (rcurry #'test-binding node context) operands))
              (test-value-function node context)))

           (test-error (str)
             "Tests that building the source STR results in a
              `SEMANTIC-ERROR'."

             (is-error (build-nodes str (make-instance 'module-table)) 'semantic-error str)))

    (subtest "Simple Atom Nodes"
      (let ((modules (make-instance 'module-table)))
        (build-nodes "a;b;c" modules)

        (test-nodes modules "a" "b" "c")))

    (subtest "Bindings"
      (let ((modules (make-instance 'module-table)))
        (build-nodes "a -> b; c -> b" modules)

        (with-nodes ((a "a") (b "b") (c "c")) modules
          (->> (test-binding a b)
               (test-value-function b a))

          (->> (test-binding c b)
               (test-value-function b c))))

      (let ((modules (make-instance 'module-table)))
        (build-nodes "a -> (b -> c)" modules)

        (with-nodes ((a "a") (b "b") (c "c") (b->c ("->" "b" "c"))) modules
          (->> (test-binding a b->c)
               (test-value-function b->c a))

          (->>
           `(if ,(test-binding b->c c b)
                ,(test-binding b c)
                ,(node-link :self))
           (test-value-function c b)))))

    (subtest "Functor Nodes"
      (let ((modules (make-instance 'module-table)))
        (build-source-file #p"./modules/core.trd" modules)
        (build-nodes "a + b -> output; int(x) -> z" modules)

        (with-nodes ((fn "+") (a "a") (b "b")
                     (a+b ("+" "a" "b")) (output "output"))
            modules

          (test-binding a+b output)
          (test-node-function a+b fn fn a b))

        (with-nodes ((fn "int") (x "x") (z "z") (int-x ("int" "x"))) modules
          (test-binding int-x z)
          (test-node-function int-x fn fn x)
          (test-node-function x int-x fn int-x))))

    (subtest "Special Operators"
      (subtest ":op Operator - Registering Infix Operators"
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
          (test-error ":op()")
          (test-error ":op(*)")
          (test-error ":op(*,*)")
          (test-error ":op(*,9,x)")
          (test-error ":op(\"*\", 10, left)")
          (test-error "a -> :op(*, 10, left)")
          (test-error ":op(*, 10, left) -> a")))

      (subtest ":attribute Operator - Node Attributes"
        (let ((modules (make-instance 'module-table)))
          ;; Build Nodes
          (build-nodes "add(a, b) : +(a, b); node1; node2" modules)

          ;; Build Attribute Declarations
          (build-nodes ":attribute(node1, no-coalesce, 1)" modules)
          (build-nodes ":attribute(add, \"public-name\", \"sum\")" modules)
          (build-nodes ":attribute(node2, input, 1)" modules)

          (with-nodes ((add "add") (node1 "node1") (node2 "node2")) modules
            (is (attribute :no-coalesce node1) 1)
            (is (attribute :public-name add) "sum")
            (is (attribute :input node2) 1)

            (ok (input-node? node2) "(INPUT-NODE? node2)")
            (ok (member node2 (input-nodes (node-table modules))) "node2 in input-nodes")))

        (subtest "Errors"
          (test-error ":attribute()")
          (test-error "node; :attribute(node, attribute)")
          (test-error "node; :attribute(node, 1, 2)")
          (test-error ":attribute(1, attribute, value)")
          (test-error ":attribute(non-existant-node, public-name, \"node\")")

          (test-error "node; :attribute(node, input, 1) -> x")
          (test-error "node; x -> :attribute(node, input, 1)"))))

    (subtest "Meta-Nodes"

     (subtest "Meta-Node Definitions"
       (let ((modules (make-instance 'module-table)))
         (build-source-file #p"./modules/core.trd" modules)
         (build-nodes "add(x,y) : x + y; add(a,b)" modules)

         (with-nodes ((add "add") (a "a") (b "b") (add-ab ("add" "a" "b"))) modules
           (is-type add 'meta-node)
           (is (operands add) (decls '!\x '!\y))
           (is (definition add) (decls '(!+ !\x !\y)))

           (test-node-function add-ab add add a b)))

       (subtest "Errors"
         (test-error "x : y")
         (test-error "{x; y} : z")
         (test-error "{w; x}(y) : z")
         (test-error ":(x)")
         (test-error ":()")
         (test-error ":(x,y,z)")

         (test-error "(g(x,y) : f(x,y)) -> z")
         (test-error "z -> (g(x,y) : f(x,y))")

         (diag "Redefining Special Operators")
         (test-error "->(x, y) : fn(x, y)")
         (test-error ":(x, y) : z")
         (test-error ":extern(x) : x")
         (test-error ":op(a, b) : f(b, a)")
         (test-error "..(x,z) : g(x, z)")
         (test-error ".(a) : h(a)")
         (test-error ":attribute(m, n) : f(m,n)")
         (test-error ":module(m) : m")
         (test-error ":import(x) : x")
         (test-error ":use(z) : z")
         (test-error ":export(y) : h(y)")
         (test-error ":in(x, y) : add(x, y)")

         (let ((modules (make-instance 'module-table)))
           ;; Test node name collisions with meta-nodes
           (build-nodes "a;b" modules)
           (is-error (build-nodes #1="a(x,y) : add(x,y)" modules) 'semantic-error #1#)

           (with-slots (node-table) modules
             (build-nodes "f(x) : x" modules)

             (with-nodes ((f "f")) modules
               (is-type f 'meta-node)
               (is (operands f) (decls '!\x))
               (is (definition f) (decls '!\x)))

             (build-nodes "f(x, y) : +(x, y)" modules)

             ;; Test meta-node redefinitions
             (with-nodes ((f "f")) modules
               (is-type f 'meta-node)
               (is (operands f) (decls '!\x '!\y))
               (is (definition f) (decls '(!+ !\x !\y))))

             ;; Test name collisions with atom nodes
             (is-error (build-nodes "f" modules) 'semantic-error "f")))))

     (subtest "External Meta-Node Definitions"
       (let ((modules (make-instance 'module-table)))
         (build-nodes ":extern(add, sub); add(a,b); sub(a,b)" modules)

         (with-nodes ((add "add") (sub "sub")
                      (a "a") (b "b")
                      (add-ab ("add" "a" "b")) (sub-ab ("sub" "a" "b")))
             modules

           (is-type add 'external-meta-node)
           (is-type sub 'external-meta-node)

           (is (definition add) nil)
           (is (definition sub) nil)

           (test-node-function add-ab add add a b)
           (test-node-function sub-ab sub sub a b)))

       (subtest "Errors"
         (test-error "x -> :extern(y)")
         (test-error ":extern(y) -> x"))))))

(finalize)

(cl-interpol:disable-interpol-syntax)
