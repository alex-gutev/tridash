;;;; main.lisp
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

;;;; Implementation of the compiler application itself

(in-package :tridash)


(defun main (argv)
  "Compiler application entry point."

  (if (< (length argv) 2)
      (format *error-output* "Usage: trc [build file]~%")
      (build-app (elt argv 1))))

(defun build-app (build-file)
  "Builds the application with the source and build options specified
   in the YAML file at path BUILD-FILE."

  (let ((prog-info (yaml:parse (pathname build-file))))
    (unless (and (hash-table-p prog-info) (= (hash-table-count prog-info) 1))
      (error "Build file should contain a map of 1 key-value pair."))

    (let* ((prog-info (first (hash-table-values prog-info)))
           (modules (build-sources (gethash "sources" prog-info)))
           (output-info (gethash "output" prog-info))
           (out-path (gethash "path" output-info)))

      (with-open-file (*standard-output* out-path :direction :output :if-exists :supersede)
        (let ((backend (make-keyword (string-upcase (gethash "backend" output-info)))))
          (compile-nodes backend modules output-info))))))

(defun build-sources (sources)
  "Builds the node definitions out of the sources list
   SOURCES. Returns the global module table."

  (build-program :files (source-file-list sources)))

(defun source-file-list (sources)
  "Converts the sources list SOURCES from the format specified in the
   source file to the format specified expected by the file builder
   interface."

  (iter
    (for source in sources)
    (collect
        (match source
          ((type hash-table)
           (list
            (gethash "path" source)
            source))

          (_ source)))))
