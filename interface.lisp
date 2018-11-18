;;;; interface.lisp
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

;;;; Generic Interfaces

(in-package :tridash.interface)


;;;; Frontend Interface

;;; Building Individual Files

(defvar *file-builders* (make-hash-table :test #'equalp)
  "Hash table of file processing functions. Each key is a file
   extension string and the corresponding value is a function for
   extracting node definitions from that file. The path to the file
   and the `NODE-TABLE' are passed to the function as arguments.")

(defun file-builder (extension)
  "Returns the file builder associated with the file extension
   EXTENSIONS. Raises an error if no such file builder exists."

  (or (gethash extension *file-builders*)
      (error "No file builder for extension: ~a" extension)))

(defun build-nodes-in-file (path module-table options)
  "Builds the nodes in file at path PATH, using the file builder
   associated with the file's extension, and adds the node definitions
   to MODULE-TABLE. Options is an PLIST of options passed to the file
   builder."

  (let ((ext (cdr (ppath:splitext path))))
    (unless (emptyp ext)
      (funcall (file-builder (subseq ext 1)) path module-table options))))

;;; Adding File Builders

(defun set-file-builder (extension function)
  "Sets the file builder, for files with extension EXTENSION, to
   FUNCTION. FUNCTION is an a function of three arguments: the path to
   the file, the module table in which the node definitions should be
   built and an options plist."

  (setf (gethash (string extension) *file-builders*)
        function))

(defun set-file-builders (extensions function)
  "Sets the file builders for each extension, in EXTENSIONS, to
   FUNCTION."

  (mapcar (rcurry #'set-file-builder function) extensions))


(defmacro define-file-builder (extensions (path-var module-table-var &optional (options-var (gensym))) &body body)
  "Defines a file builder for a particular extension or sets of
   extensions.

   EXTENSIONS is either a symbol, whose symbol-name is the file
   extension, or a list of symbols, in which case the file builder is
   added for each extension.

   PATH-VAR is a symbol naming the variable to which the file path
   will be bound.

   MODULE-TABLE-VAR is a symbol naming the variable to which the
   module-table, to which the node definitions should be added, is
   bound.

   OPTIONS-VAR is a symbol naming the variable to which the options
   plist is bound."

  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (set-file-builders ',(mapcar #'string (ensure-list extensions))
                        (lambda (,path-var ,module-table-var ,options-var)
                          (declare (ignorable ,options-var))
                          ,@body))))


;;;; Backend Interface

;;; Compiling Nodes

(defgeneric compile-nodes (backend module-table &key &allow-other-keys)
  (:documentation
   "Generates code for the nodes and meta-nodes contained in
    NODE-TABLE using the backend identified by the symbol BACKEND. The
    generated code is written to *standard-output*"))
