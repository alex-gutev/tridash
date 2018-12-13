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

(defvar *module-search-paths*
  '(#p"/usr/lib/tridash/modules/"
    #p"/usr/local/lib/tridash/modules/"
    #p"~/.tridash/modules/")

  "List of paths to search for module build files.")


(defun main (argv)
  "Compiler application entry point."

  (if (< (length argv) 2)
      (format *error-output* "Usage: trc [build file]~%")
      (build-app (pathname (elt argv 1)))))

(defun build-app (build-file)
  "Builds the application with the source and build options specified
   in the YAML file at path BUILD-FILE."

  (let ((prog-info (yaml:parse build-file)))
    (unless (and (hash-table-p prog-info) (= (hash-table-count prog-info) 1))
      (error "Build file should contain a map of 1 key-value pair."))

    (let* ((prog-info (first (hash-table-values prog-info)))
           (modules (build-sources build-file (gethash "sources" prog-info)))
           (output-info (gethash "output" prog-info))
           (out-path (cl-fad:merge-pathnames-as-file build-file (gethash "path" output-info))))

      (with-open-file (*standard-output* out-path :direction :output :if-exists :supersede)
        (let ((backend (make-keyword (string-upcase (gethash "backend" output-info)))))
          (compile-nodes backend modules output-info))))))

(defun build-sources (build-path sources)
  "Builds the node definitions out of the sources list
   SOURCES. Returns the global module table. BUILD-PATH is the
   `PATHNAME' to the build file."

  (handler-bind
      ((non-existent-module #'load-module-handler))
    (build-program :files (source-file-list build-path sources))))

(defun source-file-list (build-path sources)
  "Converts the sources list SOURCES from the format specified in the
   source file to the format specified expected by the file builder
   interface. BUILD-PATH is the `PATHNAME' to the build file."

  (iter
    (for source in sources)
    (collect
        (match source
          ((type hash-table)
           (list
            (cl-fad:merge-pathnames-as-file build-path (gethash "path" source))
            source))

          (_
           (cl-fad:merge-pathnames-as-file build-path source))))))


;;; Module Loading

(defun load-module-handler (c)
  "Handler function for the `NON-EXISTENT-MODULE-ERROR'
   condition. Searches for the module in the module search paths and
   if the module is found, builds its source files and invokes the
   RETRY restart. Otherwise returns normally."

  (with-accessors ((module-name module-name) (module-table module-table)) c
    (let ((current-module (node-table module-table)))
      (unwind-protect
           (when (load-module module-name module-table)
             (retry))

        (setf (node-table module-table) current-module)))))

(defun load-module (module module-table)
  "Searches for the module MODULE in the module search paths and if
   found builds its source files. Returns non-NIL if a module was
   found, returns NIL otherwise."

  (find-if (rcurry #'load-module-sources module module-table) *module-search-paths*))

(defun load-module-sources (path module module-table)
  "Searches for the module MODULE in the directory at PATH. If the
   module was found builds its source files and returns true."

  (let ((module-path (cl-fad:merge-pathnames-as-file path (concatenate 'string (string module) ".yml"))))
    (when (osicat:regular-file-exists-p module-path)
      (let ((module-info (yaml:parse module-path)))
        (unless (hash-table-p module-info)
          (error "Expected a map."))

        (let ((sources (source-file-list module-path (gethash "sources" module-info))))
          (mapc (rcurry #'build-source-file module-table) sources)))
      t)))
