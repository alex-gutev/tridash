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


;;; Link Runtime Library

(defun link-runtime-library (root-node)
  "Inserts a script tag which references the tridash runtime library
   at path *RUNTIME-LIBRARY-PATH*. ROOT-NODE is the root-node of the
   HTML DOM."

  (case *runtime-link-type*
    (static
     (plump:make-fulltext-element
      (find-head-tag root-node)
      "script"
      :text (slurp-runtime-library *runtime-library-path*)))

    (dynamic
     (plump:make-element
      (find-head-tag root-node)
      "script"
      :attributes (alist-hash-table (acons "src" (mkstr *runtime-library-path*) nil) :test #'cl:equalp)))))


(defun find-head-tag (root-node)
  "Finds the head tag, or an appropriate tag into which a script tag
   can be inserted, in the HTML DOM with root ROOT-NODE."

  (or (find-tag "head" root-node) (find-tag "html" root-node) root-node))

(defun find-tag (tag root-node)
  "Finds a tag with name TAG in the HTML DOM with root ROOT-NODE."

  (first (plump:get-elements-by-tag-name root-node tag)))

(defun slurp-runtime-library (path)
  "Returns the contents of the runtime library at path PATH."

  (read-file-into-string path))


;;; Link Generated Code

(defun create-html-file (code root-node)
  "Embeds the generated code, CODE, inside a script tag within the
   head node of the HTML document with root node ROOT-NODE. Serializes
   the ROOT-NODE to standard output."

  (let ((head-tag (find-head-tag root-node)))
    (link-runtime-library root-node)
    (plump:make-fulltext-element head-tag "script"
                                 :text (output-code-to-string code))
    (plump:serialize root-node)))

(defun output-code-to-file (code path)
  "Compiles the node definitions in GRAPH and outputs the code to the
   file at path PATH."

  (with-open-file (*standard-output* path :direction :output :if-exists :supersede)
    (output-code code)))

(defun output-code-to-string (code)
  "Returns a string containing the serialized JavaScript code from the
   AST nodes in CODE."

  (with-output-to-string (*standard-output*)
    (output-code code)))


;;;; Compiling HTML nodes

(define-condition html-component-binding-error (semantic-error)
  ((name :initarg :name
         :reader name
         :documentation
         "Name of the `HTML-COMPONENT-NODE'."))

  (:documentation
   "Error condition: A binding involving an html component node was
    established, which is not yet supported."))

(defmethod print-object ((e html-component-binding-error) stream)
  (format stream "Binding involving html component node ~a is not yet supported." (name e)))

(defmethod create-node ((node html-component-node))
  "Currently does nothing other than signal an error if there are any
   bindings which involve the component node."

  (unless (emptyp (contexts node))
    (error 'html-component-binding-error :name (name node))))

(defmethod create-node ((node html-node))
  "Generate the code which associates a node with an HTML element."

  (concatenate
   ;; Create base node definition first
   (call-next-method)

   (when (get :input (contexts node))
     (list
      (make-input-html-node node)))

   (when (get :object (contexts node))
     (list
      (make-output-html-node node)))))

(defun make-input-html-node (node)
  "Generate code which attaches an event listener to the attribute of
   the HTML element associated with node, and updates the nodes value
   accordingly."

  (with-slots (element-id tag-name html-attribute) node
    (when html-attribute
      (when-let ((listener (make-event-listener node "element" html-attribute)))
        (make-onloaded-method
         (list
          (js-var "element" (make-get-element element-id))
          listener))))))

(defun make-output-html-node (node)
  "Generate code which adds a callback function to a node that updates
   the attributes of the HTML element associated with it."

  (let ((value-index (node-index node))
        (context (get :object (contexts node))))

    (with-slots (value-function) context
      (make-onloaded-method
       (list
        (js-var "element" (make-get-element (element-id node)))

        (js-call
         (js-member "module" "watch_node")

         value-index

         (js-lambda
          (list "value")
          (-> (compose (curry #'make-set-attribute node "element" "value") #'first)
              (map (object-expression-entries value-function))))))))))

(defun make-get-element (id)
  "Generates code which retrieves a reference to the HTML element with
   id ID"

  (js-call (js-member "document" "getElementById")
           (js-string id)))

(defun make-onloaded-method (body)
  "Generates code which executes the code BODY, after the DOM has been
   constructed."

  (js-call (js-member "document" "addEventListener")
           (js-string "DOMContentLoaded")
           (js-lambda nil body)))

(defun make-set-attribute (node element object attribute)
  "Generates code which sets the attribute ATTRIBUTE of the HTML
   element, stored in the variable ELEMENT. OBJECT is an expression
   referencing a JS object in which the value of the attribute (to be
   set) is stored in the field ATTRIBUTE. If NODE has the
   HTML-ATTRIBUTE slot, the attribute <HTML-ATTRIBUTE>.<ATTRIBUTE> is
   set."

  (with-slots (html-attribute) node
    (unless (equal (string attribute) "style")
      (js-catch
       (list
        (js-call
         "="
         (apply #'js-members element (append (ensure-list html-attribute) (list attribute)))
         (resolve (js-member (resolve object) attribute))))

       "e"
       nil))))


(defparameter *html-events*
  (alist-hash-map
   '((("input" "value") "change" "input")
     (("input" "checked") "change" "input")
     (("textarea" "value") "change" "input"))
   :test #'cl:equalp)

  "Map containing the change event names of HTML tag attributes. Each
   key is a list of two elements: the tag name and the attribute name,
   with the corresponding value being a list of two elements: the
   ordinary event name and the urgent event name. The `HASH-MAP' uses
   the EQUALP test thus the tag names and attributes can be specified
   as case-insensitive strings.")

(defun make-event-listener (node element attribute)
  "Generates code which attaches an event listener to the attribute
   ATTRIBUTE of the HTML element ELEMENT, which updates the value of
   NODE."

  (flet ((get-event-name (tag attribute)
           (let ((events (get (list tag attribute) *html-events*)))
             (if (bool-value (attribute :urgent node))
                 (second events)
                 (first events)))))

    (let ((value-index (node-index node))
          (input-index (context-index node (context node :input))))

      (with-slots (tag-name) node
        (awhen (get-event-name tag-name attribute)
          (js-call (js-member element "addEventListener")
                   (js-string it)
                   (js-lambda
                    nil

                    (list
                     (js-call
                      (js-member "module" "set_value")

                      value-index
                      input-index

                      (js-member "this" attribute))))))))))
