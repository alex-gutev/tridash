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

(defvar *html-node-aliases* nil
  "Hash map storing aliases to html nodes, used by HTML-NODE=.")

(defun test-html-node (node id tag attribute)
  "Tests that NODE is an HTML-NODE with the ELEMENT-ID equal to ID,
   the tag equal to TAG and the attribute ATTRIBUTE."

  (subtest (format nil "Test HTML-NODE ~a" node)
    (with-slots (element-id tag-name html-attribute) node
      (is-type! node 'html-node "Is HTML-NODE")
      (is element-id id "Element ID is ~a" id)
      (is tag tag-name "Tag is ~a" tag)
      (is attribute attribute "HTML attribute is ~a" attribute)

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


(plan nil)

(deftest html-file-builder
  (subtest "Simple HTML node bindings"
    (with-module-table modules
      (let ((root-node (build-html-file #p"test/builders/html/input/test1.html" modules)))
        (with-nodes ((name "name")
                     (input-name "input-name")
                     (input-name.value ("." "input-name" "value")))
            modules

          (test-html-node input-name "input-name" "input" nil)
          (test-html-node input-name.value "input-name" "input" "value")

          (test-binding input-name.value name)
          (test-binding name input-name.value)

          (has-value-function
           (input-name.value) input-name
           `(:object (,(node-id "value") ,input-name.value)))

          (has-value-function
           (input-name) input-name.value
           `(:member ,input-name ,(node-id "value"))))

        (is (strip-empty-text-nodes root-node)
            (parse-html-file #p"test/builders/html/input/test1.out.html")
            :test #'html=))))

  (subtest "Automatic Creation of SPAN HTML nodes"
    (with-module-table modules
      (let ((root-node (build-html-file #p"test/builders/html/input/test2.html" modules)))
        (with-nodes ((first "first") (last "last")

                     (input-first "input-first")
                     (input-first.value ("." "input-first" "value"))

                     (input-last "input-last")
                     (input-last.value ("." "input-last" "value")))
            modules

          (test-html-node input-first "input-first" "input" nil)
          (test-html-node input-first.value "input-first" "input" "value")

          (test-html-node input-last "input-last" "input" nil)
          (test-html-node input-last.value "input-last" "input" "value")

          (test-binding input-first.value first)
          (test-binding input-last.value last)

          (has-value-function
           (input-first.value) input-first
           `(:object (,(node-id "value") ,input-first.value)))

          (has-value-function
           (input-first) input-first.value
           `(:member ,input-first ,(node-id "value")))

          (has-value-function
           (input-last.value) input-last
           `(:object (,(node-id "value") ,input-last.value)))

          (has-value-function
           (input-last) input-last.value
           `(:member ,input-last ,(node-id "value"))))

        (is (strip-empty-text-nodes root-node)
            (parse-html-file #p"test/builders/html/input/test2.out.html")
            :test #'html=)))))

(run-test 'html-file-builder)

(finalize)
