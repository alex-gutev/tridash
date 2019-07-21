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

;;;; HTML File Builder Test

(defpackage :tridash.test.builder.html
  (:use :generic-cl
        :alexandria
        :anaphora
        :cl-arrows
        :iterate
        :optima
        :optima.ppcre
        :prove
        :named-readtables

        :tridash.interface
        :tridash.parser
        :tridash.frontend
        :tridash.builder.html

        :tridash.test.util
        :tridash.test.builder)

  (:shadowing-import-from :generic-cl
                          :emptyp
                          :multiply
                          :accumulate)

  (:shadowing-import-from :prove :fail)

  (:import-from :lol
                :defmacro!
                :lol-syntax)

  (:import-from :tridash.builder.html
                :build-html-file))


(in-package :tridash.test.builder.html)

(in-readtable lol-syntax)


(defvar *html-node-aliases* nil
  "Hash map storing aliases to html nodes, used by HTML-NODE=.")

(defun test-html-node (node id tag attribute)
  "Tests that NODE is an HTML-NODE with the ELEMENT-ID equal to ID,
   the tag equal to TAG and the attribute ATTRIBUTE."

  (subtest (format nil "Test HTML-NODE ~a" node)
    (with-slots (element-id tag-name html-attribute) node
      (is-type! node 'html-node "Is HTML-NODE")
      (isf element-id id "Element ID is ~a" id)
      (isf tag-name tag "Tag is ~a" tag)
      (isf html-attribute attribute "HTML attribute is ~a" attribute)

      (ok (attribute :no-coalesce node) "Has NO-COALESCE attribute"))))


(defgeneric html-node= (got expected)
  (:documentation
   "Returns true if the HTML node GOT is equivalent to the HTML node
    EXPECTED."))

(defun html= (got expected)
  "Returns true if the HTML output with root-node GOT is equivalent to
   the expected output EXPECTED."

  (let ((*html-node-aliases* (make-hash-map)))
    (html-node= got expected)))

(defgeneric html-attr= (got expected)
  (:documentation
   "Returns true if the attribute value GOT is equal to the attribute
    value EXPECTED.

    If EXPECTED is a string beginning with '$' it signifies an
    alias. If an aliased node already exists in *HTML-NODE-ALIASES*
    the value of GOT is compared to the value of the aliased node
    otherwise GOT is added to *HTML-NODE-ALIASES* under the key
    EXPECTED, and true is returned.")

  (:method (got (expected string))
    (if (= (char expected 0) #\$)
        (html-attr= (ensure-get expected *html-node-aliases* got) got)
        (call-next-method)))

  (:method (a b)
    (= a b)))

(defmethod html-node= ((got plump:element) (expected plump:element))
  "Returns true if the HTML node GOT has the same tag-name, each
   attribute of EXPECTED has the same value as the corresponding
   attribute in GOT and each child of GOT is equal (by HTML-NODE=) to
   the corresponding child of EXPECTED."

  (and (= (plump:tag-name got) (plump:tag-name expected))
       (every (lambda (pair)
                (destructuring-bind (attribute . value) pair
                  (html-attr= (plump:attribute got attribute) value)))
              (plump:attributes expected))
       (call-next-method)))

(defmethod html-node= ((got plump:text-node) (expected plump:text-node))
  "Returns true if the text content of the HTML text nodes GOT and
   EXPECTED are equal."

  (= (plump:text got) (plump:text expected)))

(defmethod html-node= ((got plump:nesting-node) (expected plump:nesting-node))
  "Returns true if GOT and EXPECTED have the same child nodes
   by (HTML-NODE=)."

  (and (= (length (plump:children got)) (length (plump:children expected)))
       (every #'html-node= (plump:children got) (plump:children expected))))

(defmethod html-node= ((a plump:doctype) (b plump:doctype))
  t)

(defmethod html-node= (a b)
  (= a b))


(defconstant +white-space-chars+
  '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout)

  "Set of whitespace characters.")

(defun space-char-p (char)
  "Returns true if CHAR is a space character."

  (memberp char +white-space-chars+))


(defgeneric strip-empty-text-nodes (node)
  (:documentation
   "Removes text-nodes from NODE, and its children, which contain only
    whitespace. NODE is returned."))

(defmethod strip-empty-text-nodes ((node plump:nesting-node))
  (flet ((empty-text-node (node)
           (and (plump:text-node-p node)
                (every #'space-char-p (plump:text node)))))
    (with-accessors ((children plump:children)) node
      (setf children (delete-if #'empty-text-node children))
      (foreach #'strip-empty-text-nodes children))

    node))

(defmethod strip-empty-text-nodes (node)
  node)


(defun parse-html-file (path)
  "Parse the HTML file at path PATH and removes all whitespace
   text-nodes. Returns the root node of the HTML file."

  (strip-empty-text-nodes (plump:parse path)))


(defmacro with-html-nodes ((&rest nodes) table &body body &environment env)
  "Tests that each node in NODES is an `HTML-NODE', within the table
   TABLE, and that it has the correct attributes.

   Each element in NODES is a list of the form: (SYM ID TAG).

   SYM is the symbol to which the node id be bound.

   ID is either a string in which case it is interpreted as both the
   node name and the ID of the HTML element and the HTML attribute is
   assumed to be NIL. If it is a list with the first element being the
   subnode operator '.', in which case the second element is
   interpreted as the HTML id and the third is interpreted as the HTML
   attribute. Each node id is treated as a member reference of the
   node with identifier equal to the expansion of the symbol macro
   HTML-COMPONENT-NODE-ID.

   TAG is the tag name of the HTML node.

   BODY is a list of forms which are evaluated in an environment where
   the nodes in NODES are bound to their corresponding symbols."

  (labels ((make-node-binding (node)
             (list (first node)
                   (get-node-name (second node))))

           (get-node-name (node)
             (match node
               ((list (eql +subnode-operator+) id attribute)
                (list +subnode-operator+ (get-node-name id) attribute))

               (name
                (list +subnode-operator+ (macroexpand 'html-component-node-id env) name))))

           (make-html-test (node)
             (ematch node
               ((list node
                      (or (list (eql +subnode-operator+) id attribute)
                          id)
                      tag)
                `(test-html-node ,node ,id ,tag ,attribute)))))

    `(with-nodes ,(mapcar #'make-node-binding nodes) ,table
       ,@(mapcar #'make-html-test nodes)
       ,@body)))

(defmacro test-html-node-function (node &rest attributes)
  "Tests that the value function of the HTML element node NODE is an
   :OBJECT function with key-value pairs ATTRIBUTES. Each element of
   ATTRIBUTES is of the form (ATTRIBUTE NODE) where ATTRIBUTE is the
   HTML attribute identifier (automatically converted to a symbol
   identifier with NODE-ID) and NODE is the HTML attribute node."

  `(has-value-function
    ,(mapcar #'second attributes)
    ,node
    `(:object ,,@(mapcar #`(list ',(node-id (first a1)) ,(second a1)) attributes))))

(defun test-html-attribute-function (node element-node attribute)
  "Test that the value function of the HTML attribute NODE is a
   :MEMBER expression for the key ATTRIBUTE (automatically converted
   to a symbol identifier with NODE-ID) of the object node NODE."

  (has-value-function (element-node) node `(:member ,element-node ,(node-id attribute))))

(defvar *html-component-node* nil
  "The identifier name of the HTML component node of the current
   file.")

(defmacro! html-file-test ((module-table name file &key (core nil)) &body body)
  "Builds the nodes in the HTML file FILE, into a module table which
   is bound to the symbol MODULE-TABLE and evaluates the forms in
   BODY.

   NAME is the name of the `HTML-COMPONENT-NODE' which is created. The
   symbol-macro HTML-COMPONENT-NODE-ID, which expands to NAME, is
   established in the environment in which the forms in BODY are
   evaluated.

   If :CORE is provided and is true, the core module is built prior to
   building the HTML file.

   Finally checks that the root-node returned by BUILD-HTML-FILE is
   equivalent to the root-node of the parsed HTML file, which is at
   the path FILE with the extension replaced by .OUT.HTML."

  (flet ((out-file (path)
           (with-accessors
                 ((dir pathname-directory)
                  (name pathname-name)
                  (type pathname-type))
               path
             (make-pathname :directory dir
                            :name (concatenate-to 'string name ".out")
                            :type type))))

    (match name
      ((or (list module name)
           name)

       `(progn
          (diag ,(format nil "Test file: ~a" file))

          (with-module-table ,module-table
            ,(when core '(build-core-module))

            (-<>> ,(if module (format nil "~a.~a" module name) name)
                  (cons "node-name")
                  list
                  alist-hash-map
                  (list ,file)
                  (build-source-file <> ,module-table))

            (symbol-macrolet ((html-component-node-id ,name))
              (let ((,g!comp-node (test/get-node ',name ,module-table)))
                (is-type! ,g!comp-node 'html-component-node)

                ,@body
                (is (strip-empty-text-nodes (element-node ,g!comp-node))
                    (parse-html-file ,(out-file file))
                    :test #'html=)))))))))


(plan nil)

(subtest "Simple HTML node bindings"
  (html-file-test (modules "main" #p"test/builders/html/input/test1.html")
    (with-nodes ((name "name"))
        modules
      (with-html-nodes ((input-name "input-name" "input")
                        (input-name.value (:subnode "input-name" "value") "input"))
          modules

        (test-binding input-name.value name)
        (test-binding name input-name.value)

        (test-html-node-function input-name ("value" input-name.value))
        (test-html-attribute-function input-name.value input-name "value")))))

(subtest "Automatic Creation of SPAN HTML nodes"
  (html-file-test (modules "main" #p"test/builders/html/input/test2.html")
    (with-nodes ((first "first") (last "last")) modules
      (with-html-nodes ((input-first "input-first" "input")
                        (input-first.value (:subnode "input-first" "value") "input")
                        (input-last "input-last" "input")
                        (input-last.value (:subnode "input-last" "value") "input"))
          modules

        (test-binding input-first.value first)
        (test-binding input-last.value last)

        (test-html-node-function input-first ("value" input-first.value))
        (test-html-node-function input-last ("value" input-last.value))

        (test-html-attribute-function input-first.value input-first "value")
        (test-html-attribute-function input-last.value input-last "value")))))

(subtest "Bindings in Tridash code tags"
  (html-file-test (modules "main" #p"test/builders/html/input/test3.html")
    (with-nodes ((name "name")) modules
      (with-html-nodes ((input-name "input-name" "input")
                        (input-name.value (:subnode "input-name" "value") "input")

                        (heading "heading-name" "h1")
                        (heading.content (:subnode "heading-name" "textContent") "h1"))
          modules

        (test-binding input-name.value name)
        (test-binding name heading.content)

        (test-html-node-function input-name ("value" input-name.value))
        (test-html-node-function heading ("textContent" heading.content))

        (test-html-attribute-function input-name.value input-name "value")
        (test-html-attribute-function heading.content heading "textContent")))))

(subtest "Inline Functors"
  (html-file-test (modules "main" #p"test/builders/html/input/test4.html" :core t)
    (with-nodes ((a ((":in" "core" "to-int") "a"))
                 (b ((":in" "core" "to-int") "b"))
                 (a+b ((":in" "core" "+") "a" "b")))
        modules

      (with-html-nodes ((input-a.value (:subnode "input-a" "value") "input")
                        (input-b.value (:subnode "input-b" "value") "input")
                        (sum.value (:subnode "sum" "value") "input"))
          modules

        (test-binding input-a.value a)
        (test-binding a input-a.value)

        (test-binding input-b.value b)
        (test-binding b input-b.value)

        (test-binding a+b sum.value)))))

(subtest "Multiple Inline Declarations"
  (subtest "In Attributes"
    (html-file-test (modules "main" #p"test/builders/html/input/test5.html" :core t)
      (with-nodes ((a ((":in" "core" "to-int") "a"))
                   (b ((":in" "core" "to-int") "b"))
                   (a+b ((":in" "core" "+") "a" "b"))
                   (sum "sum"))
          modules

        (with-html-nodes ((input-a.value (:subnode "input-a" "value") "input")
                          (input-b.value (:subnode "input-b" "value") "input")
                          (sum.value (:subnode "sum" "value") "input"))
            modules

          (test-binding input-a.value a)
          (test-binding a input-a.value)

          (test-binding input-b.value b)
          (test-binding b input-b.value)

          (test-binding a+b sum)

          (test-binding sum sum.value)))))

  (subtest "In SPAN elements"
    (html-file-test (modules "main" #p"test/builders/html/input/test6.html" :core t)
      (with-nodes ((a ((":in" "core" "to-int") "a"))
                   (b ((":in" "core" "to-int") "b"))
                   (a+b ((":in" "core" "+") "a" "b"))
                   (sum "sum"))
          modules

        (with-html-nodes ((input-a.value (:subnode "input-a" "value") "input")
                          (input-b.value (:subnode "input-b" "value") "input"))
            modules

          (test-binding input-a.value a)
          (test-binding a input-a.value)

          (test-binding input-b.value b)
          (test-binding b input-b.value)

          (test-binding a+b sum)

          (subtest "Test Sum <SPAN> node"
            (let ((obs (first (map-keys (observers sum)))))
              (with-slots (tag-name html-attribute) obs
                (is-type! obs 'html-node "Is HTML-NODE")
                (is tag-name "span" "Tag is SPAN")
                (is html-attribute "textContent" "Bound to attribute textContent")))))))))

(subtest "Self-Node Reference from Meta-Nodes"
  (html-file-test (modules "main" #p"test/builders/html/input/test7.html" :core t)
    (let ((modules (finish-build-graph modules)))
      (with-nodes ((result "result")) modules
        (with-html-nodes ((input-a.value (:subnode "input-a" "value") "input")
                          (input-b.value (:subnode "input-b" "value") "input"))
            modules

          (test-binding input-a.value result)
          (test-binding input-b.value result)))))

  (html-file-test (modules ("mod" "main") #p"test/builders/html/input/test8.html" :core t)
    (let ((modules (finish-build-graph modules)))
      (with-nodes ((result "result")) modules
        (with-html-nodes ((input-a.value (:subnode "input-a" "value") "input")
                          (input-b.value (:subnode "input-b" "value") "input"))
            modules

          (test-binding input-a.value result)
          (test-binding input-b.value result))))))

(finalize)
