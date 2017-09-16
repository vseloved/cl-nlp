;;; (c) 2013-2017 Vsevolod Dyomkin

(in-package #:nlp.util)
(named-readtables:in-readtable rutilsx-readtable)

(declaim (inline filler))


;;; Some aliasing

(rename-package "CL-PPCRE" "CL-PPCRE" '("PPCRE" "RE"))

(define-condition nlp-error (simple-error) ())
(define-condition not-implemented-error (simple-error) ())


(defun ending-word-p (word)
  "Check if string WORD is some kind of a period char or a paragraph mark."
  (or (every 'period-char-p word)
      (string= "¶" word)))

(defun filler (n &optional (fill-char #\Space))
  "Produce an N-element filler string of FILL-CHAR's."
  (if (and (numberp n) (plusp n))
      (make-string n :initial-element fill-char)
      ""))

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
              (:+ (get# elt uniqs 0))))
      (vector (dovec (elt seq)
                (:+ (get# elt uniqs 0)))))
    (if raw uniqs (ht-keys uniqs))))

(defun bound-equal (obj specimen)
  "Checks is OBJ has all the slots with values provided by SPECIMEN hash-table."
  (maphash ^(unless (equal (slot-value obj %) %%)
              (return-from bound-equal nil))
           specimen))

(defun timestamp ()
  "Return current timestamp as string."
  (mv-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time))
    (fmt "~A~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D" year month day hour min sec)))

(defgeneric ss (obj)
  (:documentation
   "Get a short string from an object")
  (:method (obj) (string obj)))
