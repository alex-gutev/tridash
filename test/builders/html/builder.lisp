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
      (isf tag tag-name "Tag is ~a" tag)
      (isf attribute attribute "HTML attribute is ~a" attribute)

      (ok (attribute :no-coalesce node) "Has NO-COALESCE attribute"))))


(defun html= (got expected)
  "Returns true if the HTML output with root-node GOT is equivalent to
   the expected output EXPECTED."

  (let ((*html-node-aliases* (make-hash-map)))
    (html-node= got expected)))

(defgeneric html-node= (got expected)
  (:documentation
   "Returns true if the HTML node GOT is equivalent to the HTML node
    EXPECTED."))

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


(defconstant +white-space-chars+
  '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout)

  "Set of whitespace characters.")

(defun space-char-p (char)
  "Returns true if CHAR is a space character."

  (memberp char +white-space-chars+))


(defun parse-html-file (path)
  "Parse the HTML file at path PATH and removes all whitespace
   text-nodes. Returns the root node of the HTML file."

  (strip-empty-text-nodes (plump:parse path)))


(defmacro with-html-nodes ((&rest nodes) table &body body)
  "Tests that each node in NODES is an `HTML-NODE', within the table
   TABLE, and that it has the correct attributes.

   Each element in NODES is a list of the form: (SYM ID TAG).

   SYM is the symbol to which the node id be bound.

   ID is either a string in which case it is interpreted as both the
   node name and the ID of the HTML element and the HTML attribute is
   assumed to be NIL. If it is a list with the first element being the
   subnode operator '.', in which case the second element is
   interpreted as the HTML id and the third is interpreted as the HTML
   attribute.

   TAG is the tag name of the HTML node.

   BODY is a list of forms which are evaluated in an environment where
   the nodes in NODES are bound to their corresponding symbols."

  (flet ((make-node-binding (node)
           (list (first node) (second node)))

         (make-html-test (node)
           (ematch node
             ((list node
                    (or (list "." id attribute)
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
    `(:object ,,@(mapcar #`(list ',(node-id (first a1)) ,(second a1)) attributes))
    :test #'object-fn-equal))

(defun test-html-attribute-function (node element-node attribute)
  "Test that the value function of the HTML attribute NODE is a
   :MEMBER expression for the key ATTRIBUTE (automatically converted
   to a symbol identifier with NODE-ID) of the object node NODE."

  (has-value-function (element-node) node `(:member ,element-node ,(node-id attribute))))

(plan nil)

(deftest html-file-builder
  (subtest "Simple HTML node bindings"
    (with-module-table modules
      (let ((root-node (build-html-file #p"test/builders/html/input/test1.html" modules)))
        (with-nodes ((name "name"))
            modules

          (with-html-nodes ((input-name "input-name" "input")
                            (input-name.value ("." "input-name" "value") "input"))
              modules

            (test-binding input-name.value name)
            (test-binding name input-name.value)

            (test-html-node-function input-name ("value" input-name.value))
            (test-html-attribute-function input-name.value input-name "value"))

          (is (strip-empty-text-nodes root-node)
              (parse-html-file #p"test/builders/html/input/test1.out.html")
              :test #'html=)))))

  (subtest "Automatic Creation of SPAN HTML nodes"
    (with-module-table modules
      (let ((root-node (build-html-file #p"test/builders/html/input/test2.html" modules)))

        (with-nodes ((first "first") (last "last")) modules
          (with-html-nodes ((input-first "input-first" "input")
                            (input-first.value ("." "input-first" "value") "input")
                            (input-last "input-last" "input")
                            (input-last.value ("." "input-last" "value") "input"))
              modules

            (test-binding input-first.value first)
            (test-binding input-last.value last)

            (test-html-node-function input-first ("value" input-first.value))
            (test-html-node-function input-last ("value" input-last.value))

            (test-html-attribute-function input-first.value input-first "value")
            (test-html-attribute-function input-last.value input-last "value")))

        (is (strip-empty-text-nodes root-node)
            (parse-html-file #p"test/builders/html/input/test2.out.html")
            :test #'html=)))))

(run-test 'html-file-builder)

(finalize)
