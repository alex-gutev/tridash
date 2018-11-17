;;;; builder.lisp
;;;;
;;;; Tridash Programming Language.
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

;;;; HTML File Builder

(in-package :tridash.builder.html)

(in-readtable lol-syntax)


(defclass html-node (node)
  ((tag-name
    :initarg :tag-name
    :accessor tag-name
    :documentation
    "The name of the HTML tag.")

   (html-attribute
    :initarg :html-attribute
    :initform nil
    :accessor html-attribute
    :documentation
    "The HTML attribute of the element to which this node is bound.")

   (element-id
    :initarg :element-id
    :accessor element-id
    :documentation
    "Id of the corresponding HTML element."))

  (:documentation
   "An `HTML-NODE' is a node which corresponds directly to an HTML
    element."))

(defclass html-component-node (node)
  ((element-node
    :initarg :element-node
    :accessor element-node
    :documentation
    "The PLUMP HTML element node.")))



(defvar *global-module-table* nil)

(defvar *runtime-library-path* "tridash.js"
  "Path to the Tridash runtime library which will be referenced,
   using a script tag, in the output HTML file.")

(defvar *html-id-counter* 0
  "Counter for automatically generated HTML element id's. The value of
   this counter is appended to a prefix in order to generate a unique
   identifier.")


;;;; Building HTML files

(define-file-builder (html htm) (file modules options)
  (destructuring-bind (&key (node-name (gensym "HTML-COMP"))) options
    (-<>> (build-html-file file modules)
          (make-instance 'html-component-node :element-node)
          (add-node node-name <> (node-table modules)))))


(defgeneric build-html-file (file module-table)
  (:documentation
   "Extracts nodes from the HTML file FILE and adds the node
    definitions to the current module of MODULE-TABLE. Prints the
    processed HTML file to standard output."))

(defmethod build-html-file (file module-table)
  "Extracts nodes from the HTML file at path FILE."

  (with-open-file (stream file)
    (build-html-file stream module-table)))

(defmethod build-html-file ((stream stream) *global-module-table*)
  "Extracts nodes from the input stream STREAM."

  (preprocess-html stream))


;;;; Process HTML Files

(defun preprocess-html (stream)
  "Extract node declarations from an HTML file and add the `NODE'
   objects built to the current module in
   *GLOBAL-MODULE-TABLE*. Returns the root node."

  (walk-html-node (plump:parse stream)))


;;;; Process HTML Nodes

(defvar *parent-html-node* nil
  "The parent node of the HTML node being traversed.")

(defgeneric walk-html-node (node &key &allow-other-keys)
  (:documentation
   "Traverse the HTML node NODE. Node declarations appearing within
    the HTML node are extracted and parsed from which `NODE' objects
    are built and added to the current module in
    *GLOBAL-MODULE-TABLE*. A new HTML node is returned with the node
    declarations removed from each attribute."))


;;; Process HTML elements

(defmethod walk-html-node ((element plump:element) &key)
  "Extracts node declarations from each attribute of the element."

  (let* ((element (plump:clone-node element nil))
         (attributes (plump:attributes element)))

    (let ((html-id (html-element-id element))
          (tag (plump:tag-name element)))
      (dohash (key value attributes)
        (awhen (extract-tridash-node value)
          (make-html-element-node html-id tag *global-module-table*)

          (bind-html-node
           (make-html-attribute-node html-id tag key *global-module-table*)
           it
           *global-module-table*))))

    (call-next-method element :clone nil)))

(defun generate-id (&optional (prefix "__id"))
  "Generates a unique HTML identifier by concatenating PREFIX with the
   value of *HTML-ID-COUNTER*. The value of *HTML-ID-COUNTER* is
   incremented."

  (prog1 (mkstr prefix *html-id-counter*)
    (incf *html-id-counter*)))


;;; Creating HTML nodes

(defun make-html-element-node (html-id tag module-table)
  "Builds the node corresponding to the HTML element with id HTML-ID."

  (let ((node (build-node (id-symbol html-id) module-table)))
    (setf (gethash :no-coalesce (attributes node)) t)

    (unless (typep node 'html-node)
      (change-class node 'html-node
                    :tag-name tag
                    :element-id html-id))))

(defun make-html-attribute-node (html-id tag attribute module-table)
  "Builds the subnode corresponding to the attribute ATTRIBUTE of the
   HTML element HTML-ID."

  (multiple-value-bind (node table)
      (build-node (list +subnode-operator+ (id-symbol html-id) (id-symbol attribute)) module-table)

    (add-input node table)
    (setf (gethash :no-coalesce (attributes node)) t)

    (unless (typep node 'html-node)
      (change-class node 'html-node
                    :tag-name tag
                    :html-attribute attribute
                    :element-id html-id))

    node))

(defun bind-html-node (html-node decl module-table)
  "Establishes a two-way binding between the HTML node object
   HTML-NODE and the node declaration DECL."

  (let ((name (name html-node)))
    (handler-case
        (build-node
         `(,+list-operator+
           (,+bind-operator+ ,decl ,name)
           (,+bind-operator+ ,name ,decl))
         module-table)

      (target-node-error ()))))


;;; Process Text Nodes

(defmethod walk-html-node ((node plump:text-node) &key)
  "Extracts node declarations from the text contained in the text
   node. If the node contains node declarations a new empty text node
   is created and returned, otherwise NODE is returned as is."

  (with-accessors ((text plump:text)) node
    (when (plump:element-p *parent-html-node*)

      (let ((tag-name (plump:tag-name *parent-html-node*))
            (html-id (html-element-id *parent-html-node*)))

        (acond
          ((extract-tridash-node text)
           (make-html-element-node html-id tag-name *global-module-table*)
           (bind-html-node
            (make-html-attribute-node html-id tag-name "textContent" *global-module-table*)
            it
            *global-module-table*)

           (plump:make-text-node *parent-html-node*))

          (t node))))))

(defun html-element-id (element)
  "Returns the ID of the HTML element ELEMENT. If ELEMENT does not
   have an ID a unique ID is generated, using GENERATE-ID, and is set
   as the ID of ELEMENT."

  (ensure-gethash "id" (plump:attributes element) (generate-id)))


;;; Process Child Nodes

(defmethod walk-html-node ((node plump:nesting-node) &key (clone t))
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
          (awhen (walk-html-node child)
            (plump:append-child node it)))

    node))

(defmethod walk-html-node ((node plump:root) &key)
  (call-next-method (make-instance 'plump:root :children (plump:children node)) :clone nil))

(defmethod walk-html-node (node &key)
  node)


;;; Process Script Tags

(defmethod walk-html-node ((element plump:fulltext-element) &key)
  "Traverses full text elements such as script and style tags. If the
   element is a script tag with the language attribute equal to
   'tridash' the text content of the script tag is parsed as tridash
   code, and NIL is returned. Otherwise ELEMENT is returned as is."

  (let ((children (plump:children element)))
    (cond
      ((and (tridash-script? element)
            (not (emptyp children)))

       (aif (plump:attribute element "src")
            (process-source-file it)
            (process-script (aref children 0)))

       nil)

      (t element))))

(defun tridash-script? (element)
  "Returns true if ELEMENT is a script tag containing tridash node
   declarations."

  (when (string-equal (plump:tag-name element) "script")
    (aand (plump:attribute element "language")
          (string-equal it "tridash"))))

(defun process-script (text-node)
  "Parses the text content of a script tag as tridash code, and adds
   the node definitions to *GLOBAL-NODE-TABLE*. TEXT-NODE is the
   text-node child of the script tag."

  (let ((text (plump:text text-node)))
    (with-input-from-string (in text)
      (build-parsed-nodes (make-parser in) *global-module-table*))))

(defun process-source-file (path)
  "Processes the tridash source file at PATH."

  (with-open-file (in path)
    (build-parsed-nodes (make-parser in) *global-module-table*)))


;;; Parse Attributes and Text Content

(defun extract-tridash-node (value)
  "Extracts node declarations from the string VALUE, where value is
   either the value of an HTML attribute or the text content of an
   HTML text node. Returns the parsed node declaration if any, NIL if
   VALUE does not contain any node declarations."

  (flet ((parse-node (start end)
           (with-input-from-string (in value :start start :end end)
             (let ((parser (make-parser in)))
               (funcall parser (operator-nodes (node-table *global-module-table*)))))))

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
