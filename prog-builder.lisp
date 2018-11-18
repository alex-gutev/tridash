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

(defun build-program (&key inputs outputs output-file files)
  (let ((modules (make-instance 'module-table)))
    (iter
      (for file in files)

      (destructuring-bind (path &rest options) (ensure-list file)
        (change-module :init modules)
        (build-nodes-in-file path modules options)))

    (finish-build-graph modules)
    modules))
