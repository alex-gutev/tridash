;;;; modules.lisp
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

;;;; Frontend State

(in-package :tridash.frontend)

(defclass module-table ()
  ((modules
    :accessor modules
    :initarg :modules
    :initform (make-hash-table :test #'eq)
    :documentation
    "Hash-table mapping module names to the modules' `NODE-TABLES'.")

   (node-table
    :accessor node-table
    :initarg :node-table
    :initform (make-instance 'node-table)
    :documentation
    "The `NODE-TABLE' of the current module.")))

(defvar *global-module-table* (make-instance 'module-table))


(defmethod initialize-instance :after ((state module-table) &key &allow-other-keys)
  "Stores the `NODE-TABLE' initially stored in the `NODE-TABLE' slot
   of STATE in the entry for the :INIT module within the `MODULES'
   STATE."

  (with-slots (modules node-table) state
    (setf (gethash :init modules) node-table)))

(defun change-module (module &optional (frontend *global-module-table*))
  "Changes the global node table (stored in the NODE-TABLE slot of the
   FRONTEND argument) to the node table of the module MODULE."

  (setf (node-table frontend) (ensure-module module frontend)))

(defun ensure-module (module &optional (frontend *global-module-table*))
  "Ensures that MODULE names a module and returns its `NODE-TABLE'. If
   MODULE is not present in the MODULES table of FRONTEND a new empty
   `NODE-TABLE' is created."

  (ensure-gethash module (modules frontend) (make-instance 'node-table)))

(defun get-module (module &optional (modules *global-module-table*))
  "Returns the `NODE-TABLE' of MODULE. If there is module named MODULE
   in MODULES, signals a `NON-EXISTENT-MODULE' condition."

  (or (gethash module (modules modules))
      (error 'non-existent-module :module-name module)))
