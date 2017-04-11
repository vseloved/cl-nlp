;;; (c) 2017 Vsevolod Dyomkin

(in-package #:nlp.util)
(named-readtables:in-readtable rutilsx-readtable)

(defun princ-progress (cur total)
  "Print '.' on every full percent of CUR's progress to TOTAL."
  (when (> (floor (* 100 (/ cur total)))
           (floor (* 100 (/ (1- cur) total))))
    (format *debug-io* ".")))

(defvar *progress-gensyms* nil)

(defun clear-progress-gensyms ()
  (void *progress-gensyms*))

(defmacro progress-bar (&key (count 100) total)
  "Show a progress bar of COUNT points.
   TOTAL is (an estimated) total number of calls that will be made to reach 100%."
  (once-only (total count)
    (let ((cc (gensym)))
      `(if (boundp ',cc)
           (when (zerop (rem (handler-bind ((warning 'muffle-warning))
                               (:+ ,cc))
                             (if ,total
                                 (:= ,count (ceiling ,total 100))
                                 ,count)))
             (format *debug-io* "."))
           (progn
             (push ',cc *progress-gensyms*)
             (defparameter ,cc 0))))))
