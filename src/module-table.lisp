;;;; module-table.lisp
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

;;;; Functions for creating and switching modules

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
    "The current `MODULE', into which nodes are added."))

  (:documentation
   "Module table object mapping module identifiers to `MODULE'
    objects."))

(defvar *global-module-table*)


(defmethod initialize-instance :after ((module-table module-table) &key &allow-other-keys)
  "Adds a mapping for the `MODULE' initially bound to the
   `CURRENT-MODULE' slot of MODULE-TABLE with identifier
   :INIT. Creates the builtin module."

  (with-slots (modules current-module) module-table
    (create-builtin-module module-table)
    (setf (get :init modules) current-module)))

(defun change-module (module &optional (modules *global-module-table*))
  "Changes the CURRENT-MODULE of MODULES (defaults to
   *GLOBAL-MODULE-TABLE*) to MODULE."

  (setf (current-module modules)
        (ensure-module module modules)))

(defun ensure-module (module &optional (modules *global-module-table*))
  "Ensures that MODULE names a module and returns its `MODULE'
   object. If MODULE is not present in the MODULES a new empty
   `MODULE' is created."

  (ensure-get module (modules modules) (make-instance 'module :name module)))

(defun get-module (module &optional (modules *global-module-table*))
  "Returns the `MODULE' with identifier MODULE in MODULES. If there is
   no module with identifier MODULE in MODULES, signals a
   `NON-EXISTENT-MODULE' condition."

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
    (let ((case-node (add-external-meta-node (id-symbol "case") builtin))
          (?->-node (add-external-meta-node (id-symbol "?->") builtin)))

      (setf (node-macro-function case-node) #'case-macro-function)
      (export-node (id-symbol "case") builtin)

      (setf (node-macro-function ?->-node) #'?->-macro-function)
      (export-node (id-symbol "?->") builtin))))

(defun case-macro-function (operator operands module)
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
       module))))

(defun ?->-macro-function (operator operands module)
  "Macro function of the ?-> operator."

  (match operands
    ((list src target)

     (process-declaration
      `(,+bind-operator+
        ((,+in-module-operator+ ,(id-symbol "core") ,(id-symbol "not"))
         (,+subnode-operator+ ,src ,(id-symbol "fail")))

        (,+bind-operator+
         (,+subnode-operator+ ,src ,(id-symbol "value"))
         ,target))
      module

      :top-level t))

    (_ (error 'invalid-arguments-error
              :expected '(node node)
              :operator operator
              :arguments operands))))