(in-package #:nlp.util)
(named-readtables:in-readtable rutils-readtable)

;;; Some package renaming

(rename-package "CL-PPCRE" "CL-PPCRE" '("PPCRE" "RE"))


;;; Character utilities

(defparameter +newline+
  (fmt "~%")
  "A string with a single newline.")

(defparameter +white-chars+
  '(#\Space #\Tab #\Newline #\Return #\Linefeed)
  "Chars considered WHITESPACE.")

(defun white-char-p (char)
  "Test if CHAR is in +WHITE-CHARS+."
  (member char +white-chars+))

(defparameter +period-chars+
  '(#\. #\? #\!)
  "Chars considered legitimate sentence endings.")

(defun period-char-p (char)
  "Test if CHAR is in +PERIOD-CHARS+."
  (member char +period-chars+))

(declaim (inline filler))
(defun filler (n &optional (fill-char #\Space))
  "Produce an N-element filler string of FILL-CHAR's."
  (make-string n :initial-element fill-char))

(defun ending-word-p (word)
  "Check if string WORD is some kind of a period char or a paragraph mark."
  (or (every #'period-char-p word)
      (string= "¶" word)))


;;; Utilities for working with project files

(eval-always
  (defparameter +project-root+ (asdf:system-relative-pathname 'cl-nlp "")
    "Base dir of cl-nlp project."))

(defun data-file (filename)
  "File in data/ subdir of cl-nlp."
  (merge-pathnames (strcat "data/" filename)
                   +project-root+))

(defun list-from-file (file)
  "Load the contents of FILE into a list of strings for each trimmed line."
  (let (rez)
    (dolines (line file)
      (push (string-trim +white-chars+ line) rez))
    (reverse rez)))


;;; Misc

(defmacro define-lazy-singleton (name init &optional docstring)
  "Define a function NAME, that will return a singleton object,
   initialized lazily with INIT on first call.
   Also define a symbol macro <NAME> that will expand to (NAME)."
  (with-gensyms (singleton)
    `(let (,singleton)
       (defun ,name ()
         ,docstring
         (unless ,singleton
           (setf ,singleton ,init)))
       (define-symbol-macro ,(mksym name :format "<~A>") (,name)))))