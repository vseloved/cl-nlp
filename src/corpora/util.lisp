;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutils-readtable)


(defun attr (name attributes)
  "Shortut XML attribute accessor."
  (sax::standard-attribute-value
   (find name attributes :test 'string=
         :key #'sax::standard-attribute-local-name)))


(defmacro do-entries ((stream zip &key (external-format :utf-8) raw) &body body)
  (with-gensyms (name entry)
    `(zip:do-zipfile-entries (,name ,entry ,zip)
       (unless (char= #\/ (elt ,name (1- (length ,name))))
         ,(if raw
              `(flex:with-input-from-sequence
                   (,stream (zip:zipfile-entry-contents ,entry))
                 ,@body)
              `(with-input-from-string
                   (,stream (flex:octets-to-string
                             (zip:zipfile-entry-contents ,entry)
                             :external-format ,external-format))
                 ,@body))))))

(defmacro with-zip ((stream src &key (external-format :utf-8) raw) &body body)
  "Process contents in BODY of all files in zip archive at SRC
   binding them to a a string input STREAM.
   The archive's EXTERNAL-FORMAT may be specified.
   If RAW is T, will not decode the file's binary stream to string."
  (with-gensyms (zip)
    `(zip:with-zipfile (,zip ,src)
       (do-entries (,stream ,zip :external-format ,external-format :raw raw)
         ,@body))))

(defmacro with-zipped-zip ((stream zipfile-entry
                                   &key (external-format :utf-8) raw)
                           &body body)
  "Like WITH-ZIP but for processing embedded archive passed as ZIPFILE-ENTRY."
  (with-gensyms (zipstream v end entry entries zip x n)
    `(flex:with-input-from-sequence (,zipstream
                                     (zip:zipfile-entry-contents ,zipfile-entry))
       (let ((,v (make-array (zip:zipfile-entry-size ,zipfile-entry)
                             :element-type '(unsigned-byte 8))))
         (read-sequence ,v ,zipstream)
         (if-it (search #(80 75 5 6) ,v :from-end t)
                (file-position ,zipstream it)
                (error "end of central directory header not found"))
         (let* ((,end (zip::make-end-header ,zipstream))
                (,n (zip::end/total-files ,end))
                (,entries (make-hash-table :test #'equal))
                (,zip (zip::make-zipfile :stream ,zipstream
                                         :entries ,entries
                                         :external-format ,external-format)))
           (file-position ,zipstream (zip::end/central-directory-offset ,end))
           (dotimes (,x ,n)
             (let ((,entry (zip::read-entry-object ,zipstream ,external-format)))
               (set# (zip:zipfile-entry-name ,entry) ,entries ,entry)))
           (do-entries (,stream ,zip :external-format ,external-format :raw ,raw)
             ,@body))))))