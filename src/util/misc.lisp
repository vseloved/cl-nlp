;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.util)
(named-readtables:in-readtable rutils-readtable)


;;; Some package renaming

(rename-package "CL-PPCRE" "CL-PPCRE" '("PPCRE" "RE"))

;;; Conditions

(define-condition nlp-error (simple-error) ())

(define-condition not-implemented-error (simple-error) ())


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

(defun uniq (list &key raw case-insensitive)
  "Return only unique elements from LIST either as a new list
   or as hash-table if RAW is set. Can be CASE-INSENSITIVE."
  (let ((uniqs (make-hash-table :test (if case-insensitive 'equalp 'equal))))
    (dolist (elt list)
      (set# elt uniqs t))
    (if raw uniqs (ht-keys uniqs))))