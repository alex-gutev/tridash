;;;; tridash.asd
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

;;;; Frontend Error Conditions.

(in-package :tridash.frontend)

(define-condition semantic-error (error) ()
  (:documentation
   "Base condition class for semantic errors."))

(defmethod print-object ((err semantic-error) stream)
  (format stream "Semantic Error: ~a" (message err)))


(define-condition target-node-error (semantic-error)
  ((node :initarg :node
         :reader node
         :documentation
         "The node which appeared as the target."))

  (:documentation
   "Condition for when a node is the target node of a binding, however
    it cannot appear as the target."))

(defmethod message ((err target-node-error))
  "Prints the message associated with the error ERR."

  (format nil "~a cannot appear as the target of a binding" (node err)))


(define-condition ambiguous-context-error (semantic-error)
  ((node :initarg :node
         :reader node
         :documentation
         "The node with ambiguous context's"))

  (:documentation
   "Error condition for when a single path activates multiple contexts
    of a single node.")  )
