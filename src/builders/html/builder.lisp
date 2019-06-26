;;;; builder.lisp
;;;;
;;;; Tridash Programming Language.
;;;; Copyright (C) 2018-2019  Alexander Gutev
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
      (let ((module (ensure-module module modules))
            (comp-node (make-html-component-node name nil)))

        (add-node name comp-node module)
        (add-node +self-node+ comp-node module)

        (unwind-protect
             (progn
               (setf (element-node comp-node)
                     (build-html-file file modules))

               (foreach (compose #'build-meta-nodes #'meta-nodes) (map-values (modules modules))))

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

  (aprog1 (plump:parse stream)
    (plump:traverse it #'walk-html-node)))


;;;; Process HTML Nodes

(defgeneric walk-html-node (element)
  (:documentation
   "Extracts node declarations from each attribute of the element.")

  (:method ((element plump:element))
    (let ((attributes (plump:attributes element)))

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
             (make-html-element-node it tag *global-module-table*)))))))

  (:method ((element t))))

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
        (for (attribute link) in (object-expression-entries (value-function context)))
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


(defun html-element-id (element)
  "Returns the ID of the HTML element ELEMENT. If ELEMENT does not
   have an ID a unique ID is generated, using GENERATE-ID, and is set
   as the ID of ELEMENT."

  (ensure-get "id" (plump:attributes element) (generate-id)))


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
             (parse-build-nodes (make-parser in)))))

    (let ((string-start 0)
          (strings (make-array 0 :adjustable t :fill-pointer t)))

      (do-scans
          (start end reg-starts reg-ends (create-scanner #"<\?@(.*?)\?>"# :single-line-mode t) string)

        (when (plusp (- start string-start))
          (vector-push-extend (subseq string string-start start) strings))

        (setf string-start end)

        (vector-push-extend (parse-node (aref reg-starts 0) (aref reg-ends 0)) strings))

      (unless (or (zerop string-start) (= string-start (length string)))
        (vector-push-extend (subseq string string-start) strings))

      strings)))

(defun parse-build-nodes (parser &optional (modules *global-module-table*))
  "Builds the node declarations parsed using PARSER, and adds them to
   TABLE. Returns the last node declaration parsed."

  (let (last)
    (build-parsed-nodes
     (lambda (operators)
       (aand (funcall parser operators)
             (setf last it)))
     modules)
    last))
