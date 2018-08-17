;;;; html.lisp
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

;;;; Extract nodes from html files

(in-package :metalink.backend.js)


(defclass html-node (node)
  ((tag-name
    :initarg :tag-name
    :accessor tag-name
    :documentation
    "The name of the HTML tag.")

   (attribute
    :initarg :attribute
    :accessor attribute
    :documentation "The name of the attribute of the tag.")

   (element-id
    :initarg :element-id
    :accessor element-id
    :documentation
    "Id of the corresponding HTML element."))

  (:documentation
   "An `HTML-NODE' is a node which corresponds directly to an HTML
    element."))

(defun build-graph-from-html (stream)
  "Extract node definitions from an HTML file, and build the graph."

  (let ((graph (make-instance 'node-table)))
    (labels ((array-parser (array next)
               "Returns a function which when called returns the next
                element of ARRAY. NEXT is the function to call after
                all elements of ARRAY have been returned."

               (let ((index 0)
                     (length (length array)))
                 (lambda ()
                   (if (< index length)
                       (prog1 (aref array index)
                         (incf index))
                       (funcall next)))))

             (make-html-nodes (html-nodes)
               "Returns a function which when called generates (and
                returns) a node declaration for the next HTML node in
                HTML-NODES."

               (let ((index 0)
                     (length (length html-nodes)))
                 (lambda ()
                   (when (< index length)
                     (prog1
                         (make-html-node (aref html-nodes index))
                       (incf index))))))

             (make-html-node (html-node)
               "Generates a node declaration for the HTML node
                HTML-NODE."

               (destructuring-bind (html-id tag attribute node-name) html-node
                 (declare (ignore tag))
                 (let ((html-node (make-html-node-name html-id attribute))
                       (node (gethash node-name (nodes graph))))
                   (list* +bind-operator+
                          (if (plusp (dependencies-count node))
                              (list node html-node)
                              (list html-node node))))))

             (make-html-node-name (id attribute)
               "Generates the name of a node corresponding to the
                attribute ATTRIBUTE of the HTML node with id ID."

               (id-symbol
                (if attribute
                    (mkstr (list +subnode-operator+ id attribute))
                    id)))

             (mark-html-node (html-node)
               "Changes the class of the `NODE' object, corresponding
                to HTML-NODE, to `HTML-NODE'"

               (destructuring-bind (html-id tag attribute node-name) html-node
                 (declare (ignore node-name))

                 (let ((node (gethash (make-html-node-name html-id attribute) (nodes graph))))
                   (change-class node 'html-node
                                 :tag-name tag
                                 :attribute attribute
                                 :element-id html-id))))

             (mark-dependencies-no-coalesce (node)
               (maphash-keys #'mark-no-coalesce (dependencies node)))

             (mark-no-coalesce (node)
               (setf (gethash :no-coalesce (attributes node)) t)))

      (multiple-value-bind (nodes html-nodes) (preprocess-html stream)
        (build-graph (array-parser nodes (make-html-nodes html-nodes)) graph)

        (iter (for html-node in-vector html-nodes)
              (mark-html-node html-node))

        (coalesce-nodes graph)
        (build-wait-sets graph)

        graph))))


(defun preprocess-html (stream)
  "Extract node declarations from an HTML file. Returns two values: An
   array of the parsed node declarations and an array of the HTML
   nodes, where each element is of the form (ID TAG ATTRIBUTE NODE)
   where ID is the HTML id of the element, TAG is the tag-name of the
   element, ATTRIBUTE is the attribute to which the node named NODE is
   bound."

  (let ((root-node (plump:parse stream))
        (nodes (make-array 0 :adjustable t :fill-pointer t))
        (html-nodes (make-array 0 :adjustable t :fill-pointer t)))

    (plump:traverse root-node (rcurry #'walk-html-node nodes html-nodes))
    (values nodes html-nodes)))

(defgeneric walk-html-node (node nodes html-node-ids)
  (:documentation
   "Traverse the HTML node NODE. Node declarations appearing within
   the HTML node are extracted and appended to the NODES array, the
   HTML nodes (in which the node declarations appear) are appended to
   the HTML-NODE-IDS."))

(defmethod walk-html-node ((element plump:element) nodes html-nodes)
  "Traverses HTML elements. Node declarations are extracted from each
   attribute, of the element, and appended to the NODES array, the
   HTML nodes in which they appear are appended to HTML-NODES. "

  (let* ((attributes (plump:attributes element))
         (html-id (gethash "id" attributes)))

    (dohash (key value attributes)
      (awhen (extract-metalink-node value)
        (vector-push-extend it nodes)
        (vector-push-extend (list html-id (plump:tag-name element) key it) html-nodes)

        (setf (gethash key attributes) "")))))

(defmethod walk-html-node ((node plump:text-node) nodes html-nodes)
  "Traverses HTML text nodes. Node declarations are extracted from the
   text contained in the text node and appended to the NODES
   array. The HTML node corresponding to the textContent attribute of
   the parent element (of the text node) is appended to the HTML-NODES
   array."

  (with-accessors ((parent plump:parent) (text plump:text)) node
    (let* ((attributes (and (plump:element-p parent) (plump:attributes parent)))
           (tag-name (and (plump:element-p parent) (plump:tag-name parent)))
           (html-id (when attributes (gethash "id" attributes))))

      (awhen (extract-metalink-node text)
        (vector-push-extend it nodes)
        (vector-push-extend (list html-id tag-name "textContent" it) html-nodes)

        (setf text "")))))

(defmethod walk-html-node (node nodes html-nodes)
  (declare (ignore node nodes html-nodes)))


(defun extract-metalink-node (value)
  "Extracts node declaration from the string VALUE, where value is
   either the value of an HTML attribute or the text content of an
   HTML text node. Returns the parsed node declaration if any, NIL if
   VALUE does not contain any node declarations."

  (awhen (search "{" value :test #'char=)
    (let* ((start (1+ it))
           (end (or (search "}" value :test #'char=) (length value))))
      (with-input-from-string (in value :start start :end end)
        (let ((parser (make-parser in)))
          (funcall parser))))))


;;;; Compiling HTML nodes

(defmethod create-node ((node html-node))
  "Generates the node definitions for NODES which correspond directly
   to HTML elements. Either the set_value method is overridden to
   update the value of the HTML element's attribute (if the node
   functions as an observer) or an event listener is attached to the
   element which updates the value of the node (if the node functions
   as a dependency)."

  (with-slots (element-id tag-name attribute value-function) node
    (let ((code (call-next-method))
          (path (node-path node)))

      (list
       code
       (make-onloaded-method
        (list
         (js-call '= (js-member path "html_element")
                  (js-call (js-member "document" "getElementById")
                           (js-string element-id)))
         (unless value-function
           (make-event-listener node))))

       (when value-function
         (js-call '=
                  (js-member path "set_value")
                  (make-set-value node)))))))

(defun make-onloaded-method (body)
  "Generates code which executes the code BODY, after the DOM has been
   constructed."

  (js-call (js-member "document" "addEventListener")
           (js-string "DOMContentLoaded")
           (js-lambda nil body)))

(defun make-set-value (node)
  "Generates a function which sets the attribute (stored in the
   ATTRIBUTE slot) of the element corresponding to the HTML node
   NODE."

  (with-slots (attribute) node
    (js-lambda
     (list "value")
     (list
      (js-call '=
               (js-members "this" "html_element" attribute)
               "value")))))


(defparameter *html-events*
  (alist-hash-table
   '((("input" "value") . "change"))
   :test #'equalp)

  "Hash table containing the \"change\" event names of HTML tag
   attributes. Each key is a list of two values: the tag name and the
   attribute name, with the corresponding value being the change event
   name. The hash-table uses the EQUALP test thus the tag names and
   attributes can be specified as case-insensitive strings.")

(defun make-event-listener (node)
  "Generates code which attaches an event listener to an attribute of
   an HTML element (stored in NODE), which updates the value of NODE."

  (let ((path (node-path node)))
    (with-slots (tag-name attribute) node
      (js-call (js-members path "html_element" "addEventListener")
               (js-string (gethash (list tag-name attribute) *html-events*))
               (js-lambda
                nil
                (list
                 (js-call (js-member path "set_value")
                          (js-member "this" "value"))))))))
