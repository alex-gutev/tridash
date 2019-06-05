;;;; script.lisp
;;;;
;;;; Tridash test script

(defun debug-hook (condition prev)
  (declare (ignore prev))

  (format *debug-io* "~&~a~%" condition)
  (uiop:quit -1))


(setf *debugger-hook* #'debug-hook)

(ql:quickload :tridash/test)
(uiop:quit
 (if (prove:run :tridash/test :reporter :fiveam) 0 -1))
