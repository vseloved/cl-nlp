;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.util)
(named-readtables:in-readtable rutils-readtable)


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

(defun write-bin-file (path data)
  "Save octet sequence DATA to file at PATH,
   overwriting it if it already exists."
  (with-open-file (out path
                       :direction :output
                       :if-exists :supersede
                       :element-type 'flex:octet)
    (write-sequence data out))
  path)

(defun download-file (url dir)
  "Download file from URL and place it into DIR by the name
   inferred from the URL, overwriting any existing file with the same name.
   Returns file path and filename."
  (let* ((real-uri (puri:uri-path (nth-value 3 (drakma:http-request
                                                url :method :head))))
         (filename (sub real-uri (1+ (position #\/ real-uri :from-end t))))
         (path (merge-pathnames filename dir)))
    (write-bin-file path (drakma:http-request url :force-binary t))
    (values path
            filename)))

(defgeneric download (what &key url dir)
  (:documentation
   "Download WHAT from URL to DIR."))
