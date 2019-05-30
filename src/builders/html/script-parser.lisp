;;;; script-parser.lisp
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

;;;; HTML File Builder

(in-package :tridash.builder.html)

(defclass html-script-stream (fundamental-character-input-stream)
  ()

  (:documentation
   "Character input stream which reads input from an HTML file that is
    currently being parsed by PLUMP."))

(defmethod stream-read-char ((stream html-script-stream))
  "Read a character from the HTML file currently being parsed by
   PLUMP."

  (or (plump:consume) :eof))

(defmethod stream-unread-char ((stream html-script-stream) character)
  "Replace the last character back in the input stream of the HTML
   file currently being parsed. Note: CHARACTER is ignored."

  (declare (ignore character))

  (plump:unread)
  nil)


(defstruct (inline-lexer (:include lexer))
  "Tridash lexer for tridash located inline in an HTML file. The lexer
   returns NIL (EOF) when the closing '?>' is read."

  (end nil))

(defmethod next-token ((lex inline-lexer) &key)
  "Reads the next token in the inline Tridash code. Returns NIL (EOF)
   if the next token begins with '?>'."

  (with-accessors ((end? inline-lexer-end)) lex
    (unless end?
      (multiple-value-bind (type lexeme) (call-next-method)
        (cond
          ((/= (mismatch "?>" lexeme) 0)
           (plump:unread-n (- (length lexeme) 2))
           (setf end? t)
           nil)

          (t
           (values type lexeme)))))))


;;;; Parsing Tridash code tags

(plump:define-tag-dispatcher (tridash plump:*tag-dispatchers*) (name)
  (starts-with #\? name))

(plump:define-tag-parser tridash (name)
  (with-open-stream (in (make-instance 'html-script-stream))
    (let* ((lex (make-inline-lexer :stream in))
           (last (parse-build-nodes (make-parser lex))))

      (cond
        ((= name "?@")
         (aprog1 (plump:make-element plump:*root* "span")
           (make-element-node it last)))

        (t
         (plump:make-text-node plump:*root*))))))

(defun make-element-node (element node)
  "Creates a Tridash node corresponding to the HTML element ELEMENT
   and binds it to the node with declaration NODE."

  (let ((tag-name (plump:tag-name element))
        (html-id (html-element-id element)))
    (make-html-element-node html-id tag-name *global-module-table*)

    (-> (make-html-attribute-node html-id tag-name "textContent" *global-module-table*)
        (bind-html-node node *global-module-table*))))
