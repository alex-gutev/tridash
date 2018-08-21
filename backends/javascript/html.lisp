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

(in-readtable lol-syntax)


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


(defvar *global-node-table* nil
  "The `NODE-TABLE' containing all node definitions extracted from the
   HTML file and source files linked using script tags.")


;;;; Building Graph

(defun build-graph-from-html (stream)
  "Extract node definitions from an HTML file, and build the
   graph. Returns two values: the global node table and the HTML root
   node."

  (let ((*global-node-table* (make-instance 'node-table)))
    (labels ((make-html-nodes (html-nodes)
               "Returns a function which when called generates (and
                returns) a node declaration for the next HTML node in
                HTML-NODES."

               (iter (for html-node in-vector html-nodes)
                     (build-node (make-html-node html-node) *global-node-table*)))

             (make-html-node (html-node)
               "Generates a node declaration for the HTML node
                HTML-NODE."

               (destructuring-bind (html-id tag attribute node-name) html-node
                 (declare (ignore tag))
                 (let ((html-node (make-html-node-name html-id attribute))
                       (node (gethash node-name (nodes *global-node-table*))))
                   (list* +bind-operator+
                          (if (plusp (dependencies-count node))
                              (list node html-node)
                              (list html-node node))))))

             (make-html-node-name (id attribute)
               "Generates the name of a node corresponding to the
                attribute ATTRIBUTE of the HTML node with id ID."

               (list +subnode-operator+ (id-symbol id) (id-symbol attribute)))

             (mark-html-node (html-node)
               "Changes the class of the `NODE' object, corresponding
                to HTML-NODE, to `HTML-NODE'"

               (destructuring-bind (html-id tag attribute node-name) html-node
                 (declare (ignore node-name))

                 (mark-html-element tag html-id)

                 (let ((node (gethash (make-html-node-name html-id attribute) (nodes *global-node-table*))))

                   (change-class node 'html-node
                                 :tag-name tag
                                 :attribute attribute
                                 :element-id html-id))))

             (mark-html-element (tag id)
               (let* ((name (id-symbol id))
                      (node (gethash name (nodes *global-node-table*))))

                 (remove-observers node)
                 (if (plusp (dependencies-count node))
                     (change-class node 'html-node
                                   :tag-name tag
                                   :attribute nil
                                   :element-id id)
                     (remhash name (nodes *global-node-table*)))))

             (remove-observers (html-node)
               (maphash-keys (rcurry #'remove-observer html-node) (observers html-node))
               (clrhash (observers html-node)))

             (remove-observer (observer node)
               (remhash node (dependencies observer))))

      (multiple-value-bind (root-node html-nodes) (preprocess-html stream)
        (make-html-nodes html-nodes)
        (finish-build-graph *global-node-table*)

        (iter (for html-node in-vector html-nodes)
              (mark-html-node html-node))

        (coalesce-nodes *global-node-table*)
        (build-wait-sets *global-node-table*)

        (values *global-node-table* root-node)))))


;;;; Process HTML Files

(defun preprocess-html (stream)
  "Extract node declarations from an HTML file and add the `NODE'
   objects built to the `NODE-TABLE' *GLOBAL-NODE-TABLE*. Returns two
   values: the root HTML node and an array of the HTML nodes
   containing node declarations, where each element is of the form (ID
   TAG ATTRIBUTE NODE) where ID is the HTML id of the element, TAG is
   the tag-name of the element, ATTRIBUTE is the attribute to which
   the node named NODE is bound."

  (let ((root-node (plump:parse stream))
        (html-nodes (make-array 0 :adjustable t :fill-pointer t)))

    (values
     (walk-html-node root-node html-nodes)
     html-nodes)))


;;;; Process HTML Nodes

(defvar *parent-html-node* nil
  "The parent node of the HTML node being traversed.")

(defgeneric walk-html-node (node html-node-ids &key &allow-other-keys)
  (:documentation
   "Traverse the HTML node NODE. Node declarations appearing within
    the HTML node are extracted and parsed from which `NODE' objects
    are built and added to *GLOBAL-NODE-TABLE*. The HTML nodes in
    which the node declarations appear are appended to the array
    HTML-NODE-IDS. A new HTML node is returned with the node
    declarations removed from each attribute."))


;;; Process HTML elements

(defmethod walk-html-node ((element plump:element) html-nodes &key)
  "Extracts node declarations from each attribute of the element."

  (let* ((element (plump:clone-node element nil))
         (attributes (plump:attributes element))
         (html-id (gethash "id" attributes)))

    (dohash (key value attributes)
      (awhen (extract-metalink-node value)
        (build-node it *global-node-table*)

        (vector-push-extend (list html-id (plump:tag-name element) key it) html-nodes)
        (remhash key attributes)))

    (call-next-method element html-nodes :clone nil)))


;;; Process Text Nodes

(defmethod walk-html-node ((node plump:text-node) html-nodes &key)
  "Extracts node declarations from the text contained in the text
   node. If the node contains node declarations a new empty text node
   is created and returned, otherwise NODE is returned as is."

  (with-accessors ((text plump:text)) node
    (when (plump:element-p *parent-html-node*)

      (let* ((tag-name (plump:tag-name *parent-html-node*))
             (html-id (plump:attribute *parent-html-node* "id")))

        (acond
          ((extract-metalink-node text)
           (build-node it *global-node-table*)
           (vector-push-extend (list html-id tag-name "textContent" it) html-nodes)

           (plump:make-text-node *parent-html-node*))

          (t node))))))


;;; Process Child Nodes

(defmethod walk-html-node ((node plump:nesting-node) html-nodes &key (clone t))
  "Walks each child of NODE, by WALK-HTML-NODE and returns a new node
   which is a clone of NODE, if CLONE is true, with each child
   replaced by the new child node returned from WALK-HTML-NODE. If
   WALK-HTML-NODE returns NIL for a particular child, it is removed
   from the child nodes array. If CLONE is NIL, NODE is not cloned and
   its child array is modified directly."

  (let* ((node (if clone (plump:clone-node node nil) node))
         (*parent-html-node* node)
         (children (plump:children node)))

    (setf (plump:children node) (plump:make-child-array))

    (iter (for child in-vector children)
          (awhen (walk-html-node child html-nodes)
            (plump:append-child node it)))

    node))

(defmethod walk-html-node ((node plump:root) html-nodes &key)
  (call-next-method (make-instance 'plump:root :children (plump:children node)) html-nodes :clone nil))

(defmethod walk-html-node (node html-nodes &key)
  (declare (ignore html-nodes))
  node)


;;; Process Script Tags

(defmethod walk-html-node ((element plump:fulltext-element) html-nodes &key)
  "Traverses full text elements such as script and style tags. If the
   element is a script tag with the language attribute equal to
   'metalink' the text content of the script tag is parsed as metalink
   code, and NIL is returned. Otherwise ELEMENT is returned as is."

  (declare (ignore html-nodes))

  (let ((children (plump:children element)))
    (cond
      ((and (metalink-script? element)
            (not (emptyp children)))

       (aif (plump:attribute element "src")
            (process-source-file it)
            (process-script (aref children 0)))

       nil)

      (t element))))

(defun metalink-script? (element)
  "Returns true if ELEMENT is a script tag containing metalink node
   declarations."

  (when (string-equal (plump:tag-name element) "script")
    (aand (plump:attribute element "language")
          (string-equal it "metalink"))))

(defun process-script (text-node)
  "Parses the text content of a script tag as metalink code, and adds
   the node definitions to *GLOBAL-NODE-TABLE*. TEXT-NODE is the
   text-node child of the script tag."

  (let ((text (plump:text text-node)))
    (with-input-from-string (in text)
      (build-partial-graph (make-parser in) *global-node-table*))))

(defun process-source-file (path)
  "Processes the metalink source file at PATH."

  (with-open-file (in path)
    (build-partial-graph (make-parser in) *global-node-table*)))


;;; Parse Attributes and Text Content

(defun extract-metalink-node (value)
  "Extracts node declarations from the string VALUE, where value is
   either the value of an HTML attribute or the text content of an
   HTML text node. Returns the parsed node declaration if any, NIL if
   VALUE does not contain any node declarations."

  (flet ((parse-node (start end)
           (with-input-from-string (in value :start start :end end)
             (let ((parser (make-parser in)))
               (funcall parser)))))

    (let ((string-start 0)
          (strings (make-array 0 :adjustable t :fill-pointer t)))

      (do-scans
          (start end reg-starts reg-ends #"{{(.*?)}}"# value)

        (when (plusp (- start string-start))
          (vector-push-extend (subseq value string-start start) strings))

        (setf string-start end)

        (vector-push-extend (parse-node (aref reg-starts 0) (aref reg-ends 0)) strings))

      (cond
        ((length= 1 strings)
         (aref strings 0))

        ((not (emptyp strings))
         (reduce #2`(,(id-symbol "+") ,a1 ,a2) strings))))))


;;;; Compiling HTML nodes

(defmethod create-node ((node html-node))
  "Generates the node definitions for NODES which correspond directly
   to HTML elements. Either the set_value method is overridden to
   update the value of the HTML element's attribute (if the node
   functions as an observer) or an event listener is attached to the
   element which updates the value of the node (if the node functions
   as a dependency)."

  (with-slots (element-id tag-name attribute value-function) node
    (let ((value-fn value-function)
          (dependencies? (plusp (dependencies-count node))))
      (setf value-function nil)

      (let ((code (call-next-method))
            (path (node-path node)))

        (list
         code
         (make-onloaded-method
          (list
           (js-call '= (js-member path "html_element")
                    (js-call (js-member "document" "getElementById")
                             (js-string element-id)))
           (unless dependencies?
             (make-event-listener node))))

         (when dependencies?
           (js-call '=
                    (js-member path "compute")
                    (make-set-attributes node value-fn))))))))

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

(defun make-set-attributes (node value-function)
  (labels ((make-set-attribute (attribute)
             (destructuring-bind (key value-fn) attribute
               (let ((*get-input* (curry #'make-link-expression node))
                     (return-var (var-name)))
                 (multiple-value-bind (value-block value-expr)
                     (make-expression value-fn :return-variable return-var :tailp nil)
                   (multiple-value-call #'make-block
                     value-block
                     (use-expression
                      value-expr
                      (lambda (expr)
                        (values
                         (js-block (set-attribute-expression key expr))
                         nil))))))))

           (set-attribute-expression (attribute expr)
             (js-call '=
                      (js-members "self" "html_element" attribute)
                      expr))

           (make-block (block1 block2 expression)
             (js-block block1 block2 expression)))

    (js-lambda
     (list "values")
     (list*
      (js-var "self" "this")
      (mapcar #'make-set-attribute (rest value-function))))))


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
