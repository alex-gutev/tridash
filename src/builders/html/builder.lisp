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
   "A node referencing an HTML element or an attribute of an HTML
    element."))

(defclass html-component-node (node)
  ((element-node
    :initarg :element-node
    :accessor element-node
    :documentation
    "The PLUMP HTML element node."))

  (:documentation
   "A node storing an HTML element which is inserted in another HTML
    element as a component."))


(defvar *global-module-table* nil)

(defvar *html-id-counter* 0
  "Counter for automatically generated HTML element id's. The value of
   this counter is appended to a prefix in order to generate a unique
   identifier.")

(defvar *html-file-path* nil
  "Path to the HTML file currently being processed.")

;;;; Building HTML files

(define-file-builder (html htm) (file modules options)
  (with-hash-keys ((node-name "node-name")) options
    (multiple-value-bind (module name) (node-path->name node-name)
      (let ((module (get-module module modules))
            (comp-node (make-html-component-node name nil)))

        (add-node name comp-node module)
        (add-node +self-node+ comp-node module)

        (unwind-protect
             (progn
               (setf (element-node comp-node)
                     (build-html-file file modules))

               (foreach #'build-meta-node-graphs (map-values (modules modules))))

          (remove-node +self-node+ module))))))

(defun make-html-component-node (name root-node)
  "Creates the HTML component node of the file. NAME is the name of
   the node to create and ROOT-NODE is the root HTML node."

  (aprog1 (make-instance 'html-component-node :name name :element-node root-node)
    (setf (attribute :no-remove it) t)))

(defun build-html-file (*html-file-path* *global-module-table*)
  "Extracts nodes from the HTML file at path FILE."

  (with-open-file (stream *html-file-path*)
    (preprocess-html stream)))


;;;; Process HTML Files

(defun preprocess-html (stream)
  "Extract node declarations from an HTML file and add the `NODE'
   objects built to the current module in
   *GLOBAL-MODULE-TABLE*. Returns the root node."

  (walk-html-node (plump:parse stream)))


;;;; Process HTML Nodes

(defvar *parent-html-node* nil
  "The parent node of the HTML node being traversed.")

(defvar *siblings-p* nil
  "Flag: True if the node currently being walked has sibling nodes.")


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

    (let ((tag (plump:tag-name element)))
      (doseq ((key . value) attributes)
        (acond
          ((extract-tridash-node value)
           (let ((html-id (html-element-id element)))
             (make-html-element-node html-id tag *global-module-table*)

             (-> (make-html-attribute-node html-id tag key *global-module-table*)
                 (bind-html-node it *global-module-table*)))

           (erase attributes key))

          ((plump:attribute element "id")
           (make-html-element-node it tag *global-module-table*)))))

    (call-next-method element :clone nil)))

(defun generate-id (&optional (prefix "__id"))
  "Generates a unique HTML identifier by concatenating PREFIX with the
   value of *HTML-ID-COUNTER*. The value of *HTML-ID-COUNTER* is
   incremented."

  (prog1 (mkstr prefix *html-id-counter*)
    (incf *html-id-counter*)))


;;; Creating HTML nodes

(defun make-html-element-node (html-id tag module-table)
  "Builds the node referencing the HTML element with id HTML-ID."

  (-> (build-node (html-node-id html-id) module-table)
      (node->html-node :tag-name tag :element-id html-id)))

(defun html-attributes->html-nodes (node html-id tag-name)
  "If NODE is not an HTML-NODE and has an :OBJECT context, convert all
   the context's operand nodes to HTML attribute nodes, for the HTML
   element with id HTML-ID and tag-name TAG-NAME."

  (unless (typep node 'html-node)
    (when-let ((context (get :object (contexts node))))
      (iter
        (for (attribute link) in (rest (value-function context)))
        (make-attribute-reference (node-link-node link) html-id tag-name attribute)))))

(defun make-html-attribute-node (html-id tag attribute module-table)
  "Builds the subnode referencing the attribute ATTRIBUTE of the HTML
   element with id HTML-ID."

  (let ((node (-> (list +subnode-operator+ (html-node-id html-id) (id-symbol attribute))
                  (build-node module-table))))

    (make-attribute-reference node html-id tag attribute)
    node))

(defun make-attribute-reference (node html-id tag attribute)
  "Changes the type of NODE to an HTML-NODE, which references an
   attribute of a particular HTML element. HTML-ID is the id of the
   element, TAG is the tag-name and ATTRIBUTE is the attribute."

  (add-input node (home-module node))

  (node->html-node node
                   :tag-name tag
                   :html-attribute (string attribute)
                   :element-id html-id))

(defun html-node-id (id)
  "Returns the name of the node corresponding to the HTML element with
   id ID."

  (list +subnode-operator+ +self-node+ (id-symbol id)))

(defun node->html-node (node &rest args &key element-id tag-name &allow-other-keys)
  "If NODE is an ordinary node changes its type to `HTML-NODE'"

  (setf (attribute :no-coalesce node) t)

  (unless (typep node 'html-node)
    (html-attributes->html-nodes node element-id tag-name)
    (apply #'change-class node 'html-node args)))

(defun bind-html-node (html-node decl module-table)
  "Establishes a two-way binding between the HTML node object
   HTML-NODE and the node declaration DECL."

  (let ((name (name html-node)))
    (build-node `(,+bind-operator+ ,decl ,name) module-table)

    ;; Establish the binding in the other direction, handling all
    ;; `target-node-error' conditions since they are caused by the
    ;; binding in this direction (NAME -> DECL) rather than by DECL
    ;; itself.

    (handler-case
        (build-node
         `(,+bind-operator+ ,name ,decl)
         module-table)

      (target-node-error ()))))


(defmethod process-subnode ((object-node html-node) key)
  "Convert the subnode node into an HTML attribute node."

  (let* ((subnode (call-next-method)))
    (with-slots (tag-name element-id) object-node
      (make-attribute-reference subnode element-id tag-name key))

    subnode))

(defmethod process-subnode ((comp-node html-component-node) key)
  "Process subnode method for `HTML-COMPONENT-NODE's. Simply creates a
   node without giving it a member access functor."

  (-<> (name comp-node)
       (list +subnode-operator+ <> key)
       (ensure-node (home-module comp-node))))


;;; Process Text Nodes

(defmethod walk-html-node ((node plump:text-node) &key)
  "Extracts node declarations from the text contained in the text
   node. If the node contains node declarations a new empty text node
   is created and returned, otherwise NODE is returned as is."

  (with-accessors ((text plump:text)) node
    (labels ((make-node (node)
               "Creates the HTML node corresponding to the tridash
                node declaration NODE, the node is automatically added
                to *PARENT-HTML-NODE*. If NODE is a string a text-node
                with the string as its contents is created. Otherwise
                a span element is created."

               (typecase node
                 (string
                  (plump:make-text-node *parent-html-node* node))

                 (otherwise
                  (aprog1 (plump:make-element *parent-html-node* "span")
                    (make-element-node it node)))))

             (make-element-node (element node)
               "Creates a Tridash node corresponding to the HTML
                element ELEMENT and binds it to the node with
                declaration NODE."

               (let ((tag-name (plump:tag-name element))
                     (html-id (html-element-id element)))
                 (make-html-element-node html-id tag-name *global-module-table*)

                 (-> (make-html-attribute-node html-id tag-name "textContent" *global-module-table*)
                     (bind-html-node node *global-module-table*)))))

      (when (plump:element-p *parent-html-node*)
        (let ((nodes (extract-nodes text)))
          (cond
            ((and (not *siblings-p*) (= (length nodes) 1))
             (make-element-node *parent-html-node* (elt nodes 0))
             nil)

            ((> (length nodes) 1)
             (foreach #'make-node nodes))

            (t node)))))))

(defun html-element-id (element)
  "Returns the ID of the HTML element ELEMENT. If ELEMENT does not
   have an ID a unique ID is generated, using GENERATE-ID, and is set
   as the ID of ELEMENT."

  (ensure-get "id" (plump:attributes element) (generate-id)))


;;; Process Child Nodes

(defmethod walk-html-node ((node plump:nesting-node) &key (clone t))
  "Walks each child of NODE, by WALK-HTML-NODE and returns a new node
   which is a clone of NODE, if CLONE is true, with each child
   replaced by the new child node returned from WALK-HTML-NODE. If
   WALK-HTML-NODE returns NIL for a particular child, it is removed
   from the child nodes array. If CLONE is NIL, NODE is not cloned and
   its child array is modified directly."

  (let* ((node (if clone (plump:clone-node node nil) node))
         (children (plump:children node))
         (*parent-html-node* node)
         (*siblings-p* (> (length children) 1)))

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
            (process-script (elt children 0)))

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
   the node definitions to *GLOBAL-MODULE-TABLE*. TEXT-NODE is the
   text-node child of the script tag."

  (let ((text (plump:text text-node)))
    (with-input-from-string (in text)
      (build-parsed-nodes (make-parser in) *global-module-table*))))

(defun process-source-file (path)
  "Processes the tridash source file at PATH."

  (build-source-file (cl-fad:merge-pathnames-as-file *html-file-path* path) *global-module-table*))


;;; Parse Attributes and Text Content

(defun extract-tridash-node (value)
  "Extracts node declarations from the string VALUE, where value is
   either the value of an HTML attribute or the text content of an
   HTML text node. If VALUE contains inline Tridash node declarations,
   returns a node declaration which is the concatenation of node
   declarations and surrounding strings, otherwise returns NIL."

  (let ((strings (extract-nodes value)))
   (cond
     ((length= 1 strings)
      (elt strings 0))

     ((not (emptyp strings))
      (reduce #2`(,(id-symbol "+") ,a1 ,a2) strings)))))

(defun extract-nodes (string)
  "Extracts Tridash nodes from the string STRING. If STRING contains
   inline Tridash node declarations, returns a sequence of the literal
   string portions and Tridash node declarations (in the order they
   appear in STRING) otherwise returns NIL."

  (flet ((parse-node (start end)
           (with-input-from-string (in string :start start :end end)
             (let ((parser (make-parser in)))
               (funcall parser (operator-nodes (node-table *global-module-table*)))))))

    (let ((string-start 0)
          (strings (make-array 0 :adjustable t :fill-pointer t)))

      (do-scans
          (start end reg-starts reg-ends (create-scanner #"<%(.*?)%>"# :single-line-mode t) string)

        (when (plusp (- start string-start))
          (vector-push-extend (subseq string string-start start) strings))

        (setf string-start end)

        (vector-push-extend (parse-node (aref reg-starts 0) (aref reg-ends 0)) strings))

      (unless (or (zerop string-start) (= string-start (length string)))
        (vector-push-extend (subseq string string-start) strings))

      strings)))
