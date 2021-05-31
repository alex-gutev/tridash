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
 (if (tridash/test:test-tridash) 0 -1))
