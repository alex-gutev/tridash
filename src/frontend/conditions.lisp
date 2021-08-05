;;;; conditions.lisp
;;;;
;;;; Tridash Programming Language.
;;;; Copyright (C) 2018-2021  Alexander Gutev <alex.gutev@mail.bg>
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

;;;; Error Conditions for semantic errors related to building the node
;;;; definitions out of the parsed declarations.

(in-package :tridash.frontend)


;;; Global State Variables

(defvar *global-module-table*)

(defvar *declaration-stack* nil
  "List of declarations currently being processed. The tail of the
   list contains the top-level declaration being processed. The head
   contains the inner most declaration, of the top-level, declaration
   currently being processed.")


;;; Base Error Condition Class

(define-condition semantic-error (error)
  ((declaration-stack
    :initform *declaration-stack*
    :reader declaration-stack
    :documentation
    "The declaration stack leading up to the declaration where the
     error occurred.")

   (module-table
    :initform *global-module-table*
    :reader module-table
    :documentation
    "The global module table."))

  (:report print-semantic-error)

  (:documentation
   "Base condition class for semantic errors."))

(defgeneric print-semantic-error (error stream)
  (:documentation
   "Print a description of a semantic error condition."))

(defmethod print-semantic-error :around ((err semantic-error) stream)
  (format stream "Semantic Error: ")
  (call-next-method))


;;; Node Referencing Errors

(define-condition non-existent-node-error (semantic-error)
  ((node :initarg :node
         :reader node
         :documentation
         "The name of the node being searched for.")

   (module :initarg :module
           :reader module
           :documentation
           "The module in which the node was looked up."))

  (:documentation
   "Error condition: the node NAME was not found in the module's node
    table."))

(defmethod print-semantic-error ((err non-existent-node-error) stream)
  (format stream "No node `~a` in module `~a`."
          (node err) (name (module err))))


;;; Module Errors

(define-condition non-existent-module-error (semantic-error)
  ((module-name :initarg :module-name
                :reader module-name
                :documentation "Name of the module."))

  (:documentation
   "Error condition: Referencing a non-existent module."))

(defmethod print-semantic-error ((err non-existent-module-error) stream)
  (format stream "No module named `~a`." (module-name err)))
