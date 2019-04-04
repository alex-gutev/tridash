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

;;;; Functions for creating and switching modules

(in-package :tridash.frontend)

(defclass module-table ()
  ((modules
    :accessor modules
    :initarg :modules
    :initform (make-hash-map)
    :documentation
    "Module Map. Each key is a module identifier symbol and the
     corresponding value is the module's `NODE-TABLE'.")

   (node-table
    :accessor node-table
    :initarg :node-table
    :initform (make-instance 'node-table :name :init)
    :documentation
    "The `NODE-TABLE' of the current module.")))

(defvar *global-module-table*)


(defmethod initialize-instance :after ((module-table module-table) &key &allow-other-keys)
  "Stores the `NODE-TABLE' initially stored in the `NODE-TABLE' slot
   of STATE in the entry for the :INIT module within the `MODULES'
   STATE. Creates the builtin module."

  (with-slots (modules node-table) module-table
    (create-builtin-module module-table)
    (setf (get :init modules) node-table)))

(defun change-module (module &optional (frontend *global-module-table*))
  "Changes the global node table (stored in the NODE-TABLE slot of the
   FRONTEND argument) to the node table of the module MODULE."

  (setf (node-table frontend)
        (ensure-module module frontend)))

(defun ensure-module (module &optional (frontend *global-module-table*))
  "Ensures that MODULE names a module and returns its `NODE-TABLE'. If
   MODULE is not present in the MODULES table of FRONTEND a new empty
   `NODE-TABLE' is created."

  (ensure-get module (modules frontend) (make-instance 'node-table :name module)))

(defun get-module (module &optional (modules *global-module-table*))
  "Returns the `NODE-TABLE' of MODULE. If there is module named MODULE
   in MODULES, signals a `NON-EXISTENT-MODULE' condition."

  (with-retry-restart
    (or (get module (modules modules))
        (error 'non-existent-module :module-name module))))


;;;; Builtin Module Definition

;;; The 'builtin' module contains a few macro nodes such as the "case"
;;; meta-node. Eventually a macro-system will be developed and these
;;; will be specified in Tridash code.

(defun create-builtin-module (modules)
  "Creates the builtin module."

  (let ((builtin (ensure-module (id-symbol "builtin") modules)))
    (let ((case-node (add-external-meta-node (id-symbol "case") builtin)))
      (setf (attribute :macro-function case-node) #'case-macro-function)
      (export-node (id-symbol "case") builtin))))

(defun case-macro-function (operator operands table)
  "Case macro function. Transforms the case expression into a series
   of nested if expressions."

  (declare (ignore operator))

  (let ((if-node (list +in-module-operator+ (id-symbol "core") (id-symbol "if"))))
    (flet ((make-if (case expr)
             (match case
               ((list (eq +def-operator+) cond node)
                (list if-node cond node expr))

               (_ case))))

      (process-declaration
       (reduce #'make-if operands :from-end t :initial-value nil)
       table))))
