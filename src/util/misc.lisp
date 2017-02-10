;;; (c) 2013-2016 Vsevolod Dyomkin

(in-package #:nlp.util)
(named-readtables:in-readtable rutilsx-readtable)

(declaim (inline filler))


;;; Some aliasing

(rename-package "CL-PPCRE" "CL-PPCRE" '("PPCRE" "RE"))

(defparameter +utf-8+ (flex:make-external-format :utf-8))


;;; Conditions

(define-condition nlp-error (simple-error) ())

(define-condition not-implemented-error (simple-error) ())


;;; Various functions and macros

(defun filler (n &optional (fill-char #\Space))
  "Produce an N-element filler string of FILL-CHAR's."
  (if (plusp n)
      (make-string n :initial-element fill-char)
      ""))

(defun sorted-ht-keys (test ht)
  "Return hash-table keys of HT in sorted order accroding to TEST."
  (sort (ht-keys ht) test :key #`(get# % ht)))

(defun shorter? (list n)
  "Tests if LIST has at least N elements."
  (let ((tail list))
    (loop :repeat (1- n) :do (setf tail (cdr tail)))
    (null tail)))

(defun uniq (seq &key raw (test 'equal))
  "Return only unique elements from SEQ either as a new list
   or as hash-table if RAW is set.
   TEST should be a hash-table test."
  (let ((uniqs (make-hash-table :test test)))
    (etypecase seq
      (list (dolist (elt seq)
              (:= (? uniqs elt) t)))
      (vector (dovec (elt seq)
                (:= (? uniqs elt) t))))
    (if raw uniqs (ht-keys uniqs))))

(defun equal-when-present (obj specimen)
  "Checks is OBJ has all the slots with values provided by SPECIMEN hash-table."
  (maphash #`(unless (equal (slot-value obj %) %%)
               (return-from equal-when-present nil))
           specimen))

(defun princ-progress (cur total)
  "Print '.' on every full percent of CUR's progress to TOTAL."
  (when (> (floor (* 100 (/ cur total)))
           (floor (* 100 (/ (1- cur) total))))
     (format *debug-io* ".")))

(defun timestamp ()
  "Return current timestamp as string."
  (mv-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time))
    (fmt "~A~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D" year month day hour min sec)))

(defgeneric s! (obj)
  (:documentation
   "Get a string from an object")
  (:method (obj) (string obj)))


;;; Progress

(defvar *progress-gensyms* nil)

(defun clear-progress-gensyms ()
  (void *progress-gensyms*))

(defmacro progress-bar (&key (count 100) total)
  (once-only (total count)
    (let ((cc (gensym)))
      `(if (boundp ',cc)
           (when (zerop (rem (handler-bind ((warning 'muffle-warning))
                               (:+ ,cc))
                             (if ,total
                                 (:= ,count (ceiling ,total 100))
                                 ,count)))
             (princ "."))
           (progn
             (push ',cc *progress-gensyms*)
             (defparameter ,cc 0))))))
