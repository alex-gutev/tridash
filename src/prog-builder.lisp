;;;; prog-builder.lisp
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

(in-package :tridash.frontend)


;;;; Compiler Interface

(defun build-program (&key files)
  "Builds the node definitions parsed from the files in FILES. Each
   element in FILES is either a path to a source file or a list of
   which the first element is a path and the remaining elements are
   options passed to the file builder. INPUTS and OUTPUTS are
   currently unused. Returns the module table."

  (let ((modules (make-instance 'module-table)))
    (foreach (rcurry #'build-source-file modules) files)

    (finish-build-graph modules)))

(defun build-source-file (file modules)
  "Builds the node definitions in the source file FILE. FILE is either
   the path to the source file or a list with the first element being
   the path to the file and the second element being an options
   hash-table."

  (destructuring-bind (path &optional (options (make-hash-map :test #'cl:equalp))) (ensure-list file)
    (change-module :init modules)
    (build-nodes-in-file path modules options)))
