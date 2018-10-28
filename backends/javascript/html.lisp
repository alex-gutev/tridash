;;;; html.lisp
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

;;;; Extract nodes from html files

(in-package :tridash.backend.js)

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


(defvar *global-module-table* nil)

(defvar *runtime-library-path* "tridash.js"
  "Path to the Tridash runtime library which will be referenced,
   using a script tag, in the output HTML file.")

(defvar *html-id-counter* 0
  "Counter for automatically generated HTML element id's. The value of
   this counter is appended to a prefix in order to generate a unique
   identifier.")


;;;; Building HTML files

(defgeneric build-html-file (file module-table)
  (:documentation
   "Extracts nodes from the HTML file FILE and adds the node
    definitions to the current module of MODULE-TABLE. Prints the
    processed HTML file to standard output."))

(defmethod build-html-file (file module-table)
  "Extracts nodes from the HTML file at path FILE."

  (with-open-file (stream file)
    (build-html-file stream module-table)))

(defmethod build-html-file ((stream stream) module-table)
  "Extracts nodes from the input stream STREAM."

  (let* ((*html-id-counter* 0)
         (root-node (nth-value 1 (build-graph-from-html stream module-table))))
    (link-runtime-library *runtime-library-path* root-node)
    (link-graph module-table root-node)
    (plump:serialize root-node)))


;;;; Building Graph

(defun build-graph-from-html (stream &optional (*global-module-table* (make-instance 'module-table)))
  "Extract node definitions from an HTML file, and build the
   graph. Returns two values: the global module table and the HTML
   root node."

  (let ((root-node (preprocess-html stream)))
    (finish-build-graph *global-module-table*)
    (values *global-module-table* root-node)))


;;;; Process HTML Files

(defun preprocess-html (stream)
  "Extract node declarations from an HTML file and add the `NODE'
   objects built to the current module in
   *GLOBAL-MODULE-TABLE*. Returns the root node."

  (walk-html-node (plump:parse stream)))


;;; Link Runtime Library

(defun link-runtime-library (path root-node)
  "Inserts a script tag which references the tridash runtime library
   at path PATH. ROOT-NODE is the root-node of the HTML DOM."

  (plump:make-element
   (find-head-tag root-node)
   "script"
   :attributes (alist-hash-table (acons "src" (mkstr path) nil) :test #'equalp)))


(defun find-head-tag (root-node)
  "Finds the head tag, or an appropriate tag into which a script tag
   can be inserted, in the HTML DOM with root ROOT-NODE."

  (or (find-tag "head" root-node) (find-tag "html" root-node) root-node))

(defun find-tag (tag root-node)
  "Finds a tag with name TAG in the HTML DOM with root ROOT-NODE."

  (first (plump:get-elements-by-tag-name root-node tag)))


;;; Link Generated Code

(defun link-graph (graph root-node &key path)
  "Compiles the node definitions in GRAPH to JavaScript code, which is
   inserted in a script tag. If the keyword argument :PATH is
   provided, the generated code is written to the file at PATH
   instead, and a script tag is inserted which references that file."

  (let ((head-tag (find-head-tag root-node)))
    (cond
      (path
       (output-code-to-file graph path)
       (plump:make-element
        head-tag "script"
        :attributes (alist-hash-table
                     (list
                      (cons "src" (mkstr path))
                      (cons "defer" nil))
                     :test #'equalp)))

      (t
       (plump:make-fulltext-element head-tag "script" :text (output-code-to-string graph))))))

(defun output-code-to-file (graph path)
  "Compiles the node definitions in GRAPH and outputs the code to the
   file at path PATH."

  (with-open-file (*standard-output* path :direction :output :if-exists :supersede)
    (compile-nodes :javascript graph)))

(defun output-code-to-string (graph)
  "Compiles the node definitions in GRAPH and returns a string of the
   generated code."

  (with-output-to-string (*standard-output*)
    (compile-nodes :javascript graph)))


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
  "Establishes a two-way binding the HTML node object HTML-NODE and
   the node declaration DECL."

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

(defmethod create-node ((node html-node) &optional (code (make-code-array)))
  "Generates the node definition for a NODE which corresponds to an
   HTML element."

  ;; Create base node definition first
  (call-next-method)

  (cond
    ((gethash :input (contexts node))
     (make-input-html-node node code))

    ((gethash :object (contexts node))
     (make-output-html-node node code))))

(defun make-input-html-node (node code)
  "Generates the node definition for a node which corresponds to an
   attribute of an HTML element. The HTML element is retrieved and an
   event listener, for changes to the attribute's value, is attached."

  (let ((path (node-path node)))
    (with-slots (element-id tag-name html-attribute) node
      (when html-attribute
        (vector-push-extend
         (make-onloaded-method
          (list
           (make-get-element path element-id)

           (js-call '= (js-member path "value")
                    (js-members path "html_element" html-attribute))

           (make-event-listener node html-attribute)))
         code)))))

(defun make-output-html-node (node code)
  "Generates the node definition for a node which corresponds to an
   HTML element. The update_value method is overridden to update the
   attributes of the element."

  (let ((path (node-path node))
        (context (gethash :object (contexts node))))
    (with-slots (value-function) context
      (vector-push-extend
       (make-onloaded-method
        (list (make-get-element path (element-id node))))
       code)

      (vector-push-extend
       (js-call
        '=
        (js-member path "update_value")
        (js-lambda
         (list "value")
         (mapcar (compose (curry #'make-set-attribute node "value") #'first) (rest value-function))
         )
        )
       code)
      )))

(defun make-get-element (path id)
  "Generates code which retrieves a reference to the HTML element with
   id ID, and stores it in the html_element field of the node, which
   is reference by the expression PATH."

  (js-call '= (js-member path "html_element")
           (js-call (js-member "document" "getElementById")
                    (js-string id))))

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

(defun make-set-attribute (node object attribute)
  "Generates code which sets the attribute the attribute ATTRIBUTE of
   the HTML element stored in the html_element field of NODE. OBJECT
   is an expression referencing a JS object in which the value of the
   attribute (to be set) is stored in the field ATTRIBUTE."

  (let ((path (node-path node)))
    (js-call
     '=
     (js-members path "html_element" attribute)
     (js-member object attribute))))

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

(defun make-event-listener (node attribute)
  "Generates code which attaches an event listener to the attribute
   ATTRIBUTE of the HTML element (stored in the html_element field of
   NODE), which updates the value of NODE."

  (let ((path (node-path node)))
    (with-slots (tag-name) node
      (awhen (gethash (list tag-name attribute) *html-events*)
        (js-call (js-members path "html_element" "addEventListener")
                 (js-string it)
                 (js-lambda
                  nil
                  (list
                   (js-call '=
                            (js-member path "value")
                            (js-member "this" attribute))
                   (js-call (js-member path "set_value")
                            (js-member path "value")))))))))
