;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.util)
(named-readtables:in-readtable rutils-readtable)


;;; Some aliasing

(rename-package "CL-PPCRE" "CL-PPCRE" '("PPCRE" "RE"))

(defparameter +utf-8+ (flex:make-external-format :utf-8))


;;; Conditions

(define-condition nlp-error (simple-error) ())

(define-condition not-implemented-error (simple-error) ())


;;; Various functions and macros

(defmacro define-lazy-singleton (name init &optional docstring)
  "Define a function NAME, that will return a singleton object,
   initialized lazily with INIT on first call.
   Also define a symbol macro <NAME> that will expand to (NAME)."
  (with-gensyms (singleton)
    `(let (,singleton)
       (defun ,name ()
         ,docstring
         (or ,singleton
             (setf ,singleton ,init)))
       (define-symbol-macro ,(mksym name :format "<~A>") (,name)))))

(declaim (inline filler))
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

(defun uniq (list &key raw (eq-test 'equal))
  "Return only unique elements from LIST either as a new list
   or as hash-table if RAW is set.
   EQ-TEST should be EQUAL or EQUALP."
  (let ((uniqs (make-hash-table :test eq-test)))
    (dolist (elt list)
      (set# elt uniqs t))
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
