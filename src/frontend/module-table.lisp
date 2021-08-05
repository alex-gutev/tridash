;;;; module-table.lisp
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

;;;; Table containing all modules in a program

(in-package :tridash.frontend)

(defclass module-table ()
  ((modules
    :accessor modules
    :initarg :modules
    :initform (make-hash-map)
    :documentation
    "Module Map. Each key is a module identifier symbol and the
     corresponding value is the `MODULE'.")

   (current-module
    :accessor current-module
    :initarg :current-module
    :initform (make-instance 'module :name :init)
    :documentation
    "The current `MODULE', into which nodes are added.")

   (attribute-processors
    :accessor attribute-processors
    :initarg :attribute-processors
    :initform (make-hash-map :test #'cl:equalp)
    :documentation
    "Map of attribute processors. Each key is the attribute's string
     identifier and the corresponding value is a meta-node which
     should be invoked to process the attribute."))

  (:documentation
   "Module table object mapping module identifiers to `MODULE'
    objects."))


(defmethod initialize-instance :after ((module-table module-table) &key &allow-other-keys)
  "Adds a mapping for the `MODULE' initially bound to the
   `CURRENT-MODULE' slot of MODULE-TABLE with identifier
   :INIT. Creates the builtin module."

  (with-slots (modules current-module) module-table
    ;;TODO: Create builtin module

    (setf (get :init modules) current-module)))

(defun change-module (module &optional (modules *global-module-table*))
  "Changes the CURRENT-MODULE of MODULES (defaults to
   *GLOBAL-MODULE-TABLE*) to MODULE."

  (setf (current-module modules)
        (ensure-module module modules)))

(defun ensure-module (module &optional (modules *global-module-table*))
  "Ensure that MODULE names a module and return its `MODULE' object.

   If MODULE is not present in the MODULES a new empty `MODULE' is
   created."

  (ensure-get module (modules modules) (make-instance 'module :name module)))

(defun get-module (module &optional (modules *global-module-table*))
  "Return the `MODULE' with identifier MODULE in MODULES.

   If there is no module with identifier MODULE in MODULES, signals a
   `NON-EXISTENT-MODULE-ERROR' condition."

  (with-retry-restart
    (or (get module (modules modules))
        (error 'non-existent-module-error :module-name module))))
