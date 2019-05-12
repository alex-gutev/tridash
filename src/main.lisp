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

(defconstant +module-paths-var+ "TRIDASH_MODULE_PATHS"
  "Environment variable containing additional module search paths.")

(defconstant +paths-delimiter+ #\:
  "Search path delimiter.")

(defvar *module-search-paths*
  '(#p"/usr/local/share/tridash/modules/"
    #p"/usr/share/tridash/modules/"
    #p"~/.tridash/modules/")

  "List of paths to search for module build files.")


(defun parse-option (str)
  "Parses a key-value pair (KEY=VALUE)."

  (aif (position #\= str)
       (cons (subseq str 0 it) (subseq str (1+ it)))
       (error "Option must be of the form OPTION=VALUE")))

(opts:define-opts
  (:name :help
   :description "Print this help screen"
   :short #\h
   :long "help")

  (:name :build-config
   :description "Build using the options in the build configuration file instead of the command line options"
   :short #\b
   :long "build-config"
   :meta-var "PATH"
   :arg-parser #'identity)

  (:name :out-file
   :description "Output file name"
   :short #\o
   :long "output-file"
   :meta-var "PATH"
   :arg-parser #'identity)

  (:name :output-option
   :description "Code generation option (OPTION=VALUE)"
   :short #\p
   :long "output-option"
   :meta-var "K=V"
   :arg-parser #'parse-option)

  (:name :type
   :description "Compilation target (backend). If not provided, the target is guessed from the output file name."
   :short #\t
   :long "output-type"
   :meta-var "type"
   :arg-parser #'make-keyword))

(defmacro when-option ((options opt) &body body)
  `(awhen (getf ,options ,opt)
     ,@body))


;;;; Entry Point

(defun main ()
  "Compiler application entry point."

  (let ((*debugger-hook* #'debugger-hook))
    (multiple-value-bind (opts free-args) (opts:get-opts)
      (when (and (emptyp opts) (emptyp free-args))
        (print-help))

      (when-option (opts :help)
        (print-help))

      (when-option (opts :build-config)
        (build-from-config-file (pathname it))
        (opts:exit 0))

      (unless (get :out-file opts)
        (error "Error: No output file specified. Specify output file using -o option."))

      (let ((out-path (get :out-file opts)))
        (build-app (parse-sources free-args) out-path
                   :backend (or (get :type opts) (guess-out-type out-path))
                   :out-options (get-output-options opts))))))

(defun print-help ()
  "Prints the command line options help."

  (opts:describe
   :prefix "Tridash Compiler"
   :usage-of "tridashc"
   :args "[SOURCES]"
   :suffix
   "Additional options for processing each source file may be
specified. The special ':' argument indicates that the following
argument is a comma-separated list of options for processing the
source file in the preceding argument.

The options are in the form of key-value pairs 'key=value'. The
key/value should be enclosed in quotes if it contains either '=' or
','. Note two pairs of quotes may be needed due to the first pair
being interpreted by the shell.

Example: tridashc ui.trd : node-name=ui")

  (opts:exit 1))


(defun get-output-options (opts)
  "Returns a list of all :OUTPUT-OPTION options in the options PLIST
   OPTS."

  (let ((map (make-hash-map :test #'cl:equalp)))
    (loop
       for (key value) on opts by #'cddr
       do
         (when (= key :output-option)
           (setf (get (car value) map) (cdr value))))
    map))

(defun parse-sources (args)
  "Parses the sources list. Returns a list of the sources where each
   element is either the pathname to the source file or a list with
   the first element being the pathname and the second element being a
   map of options for processing the source file."

  (iter (generating (source . rest) on args)
        (collect
            (match (next rest)
              ((list* (equal ":") opts _)
               (aprog1 (list (pathname source) (parse-map opts))
                 (next source)
                 (next source)))

              (_
               (pprint rest)
               (pathname source))))))

(defun parse-map (opts)
  "Parses a map from the string OPTS."

  (labels ((parse-value (value)
             (match value
               ((ppcre "^[\"'](.*)[\"']$" value)
                (parse-quoted-string value))

               ((ppcre "^[\"'].*[^\"']$")
                (error "Missing closing quote ~a" value))

               (_ value)))

           (parse-quoted-string (str)
             (let ((value (make-array 0 :element-type 'character :adjustable t :fill-pointer t)))
               (doiter (it str)
                 (let ((c (at it)))
                   (case c
                     (#\\
                      (advance it)
                      (vector-push-extend (at it) value))

                     (otherwise
                      (vector-push-extend c value)))))
               value)))

    (let ((map (make-hash-map :test #'cl:equalp)))
      (symbol-macrolet
          ((regex '(:sequence
                    #1=(:register
                        (:alternation
                         (:sequence #\" (:greedy-repetition 0 nil (:alternation (:inverted-char-class #\" #\\) (:sequence #\\ :everything))) #\")
                         (:sequence #\' (:greedy-repetition 0 nil (:alternation (:inverted-char-class #\' #\\) (:sequence #\\ :everything))) #\')
                         (:greedy-repetition 0 nil (:inverted-char-class #\, #\=))))
                    #\=
                    #1#)))

        (do-register-groups ((#'parse-value key) (#'parse-value value))
            (regex opts)
          (setf (get key map) value)))
      map)))


(defconstant +out-type-extensions+
  (alist-hash-map
   '(("js" . :javascript)
     ("htm" . :javascript)
     ("html" . :javascript))
   :test #'cl:equalp))

(defun guess-out-type (out-path)
  "Determines the backend from the name of the output file. If the
   type cannot be determined, signals an error."

  (or (get (pathname-type out-path) +out-type-extensions+)
      (error "Cannot determine output type from \"~a\"" out-path)))


;;; Building from build config file

(defun build-from-config-file (build-file)
  "Builds the application with the source and build options specified
   in the YAML build config file at path BUILD-FILE."

  (let ((*module-search-paths* (search-paths))
        (prog-info (yaml:parse build-file)))

    (unless (and (hash-table-p prog-info) (= (length prog-info) 1))
      (error "Build file should contain a map of 1 key-value pair."))

    (let* ((prog-info (cdr (first prog-info)))
           (output-info (get "output" prog-info))
           (out-path (cl-fad:merge-pathnames-as-file build-file (get "path" output-info))))

      (build-app (source-file-list build-file (get "sources" prog-info))
                 out-path
                 :out-options output-info
                 :backend (make-keyword (string-upcase (get "backend" output-info)))))))

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
            (cl-fad:merge-pathnames-as-file build-path (get "path" source))
            source))

          (_
           (cl-fad:merge-pathnames-as-file build-path source))))))


;;; Building Application

(defun build-app (sources out-file &key backend out-options ((:search-paths *module-search-paths*) (search-paths)))
  "Builds an application with source files SOURCES generating the
   output file OUT-FILE. BACKEND is the backend to use and OUT-OPTIONS
   is a map of output file generation options. The SEARCH-PATHS
   argument is a list of the module search paths."

  (let* ((*global-module-table* (make-instance 'module-table))
         (node-table (build-sources sources)))

    (with-open-file (*standard-output* out-file :direction :output :if-exists :supersede)
      (compile-nodes backend node-table out-options))))

(defun search-paths ()
  "Returns a list of the module search paths. This contains the search
   paths in *MODULE-SEARCH-PATHS* and the search paths specified in
   the environment variable."

  (append
   (aand (osicat:environment-variable +module-paths-var+)
         (map #'cl-fad:pathname-as-directory (split-sequence +paths-delimiter+ it)))
   *module-search-paths*))

(defun add-data-dir (data-dir)
  "Add the directory DATA-DIR to the front of the module search paths
   list."

  (let ((dir (cl-fad:merge-pathnames-as-directory (pathname data-dir) "tridash/modules/")))
    (pushnew dir *module-search-paths*)))

(defun build-sources (sources)
  "Builds the node definitions from the source files in the list
   SOURCES. Returns the flattened module table."

  (handler-bind
      ((non-existent-module #'load-module-handler))
    (build-program :files sources)))


;;;; Module Loading

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

  (let ((module-path (cl-fad:merge-pathnames-as-file path (concatenate-to 'string (string module) ".yml"))))
    (when (osicat:regular-file-exists-p module-path)
      (let ((module-info (yaml:parse module-path)))
        (unless (hash-table-p module-info)
          (error "Expected a map."))

        (let ((sources (source-file-list module-path (get "sources" module-info))))
          (foreach (rcurry #'build-source-file module-table) sources)))
      t)))


;;;; Error Reporting

(defgeneric debugger-hook (condition prev-hook)
  (:documentation
   "Compiler application debugger hook. Currently simply displays the
    error and exits the application with exit code 1."))

(defmethod debugger-hook (condition prev-hook)
  (declare (ignore prev-hook))

  (let ((*print-pprint-dispatch* (pprint-table)))
    (format *debug-io* "~&~a~%" condition)

    (opts:exit 1)))

(defun pprint-table ()
  "Returns a PPRINT-DISPATCH table suitable for printing error
   messages without reader escape characters."

  (let ((old-table *print-pprint-dispatch*))
    (flet ((print-no-escape (stream object)
             "Prints OBJECT to STREAM with *PRINT-ESCAPE* bound to NIL."
             (write object :stream stream :escape nil :pprint-dispatch old-table)))

      (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
        (set-pprint-dispatch 'pathname #'print-no-escape)
        *print-pprint-dispatch*))))

(defun choose-restart (restarts &key (prompt "Choice") (stream *debug-io*))
  "Chooses a restart to execute out of the list RESTARTS. If RESTARTS
   contains only one restart, it is returned otherwise PROMPT is
   displayed along with the list of restarts, the user is queried for
   the restart to execute, and the chosen restart is returned."

  (let ((n (length restarts)))
    (cond
      ((> n 1)
       (iter
         (for restart in restarts)
         (for i initially 1 then (1+ i))
         (format stream "~&[~d] ~a~%" i restart))

       (iter
         (for i = (prompt-action prompt stream))
         (until (typep i `(integer 1 ,n)))
         (finally (return (nth (- i 1) restarts)))))

      ((= n 1)
       (first restarts)))))

(defun prompt-action (prompt stream)
  "Prints PROMPT to the stream and READ an object from STREAM."

  (format stream "~&~a: " prompt)
  (finish-output stream)

  (prog1 (read)
    (fresh-line)))
