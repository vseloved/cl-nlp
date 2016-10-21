;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.contrib.wordnet)
(named-readtables:in-readtable rutils-readtable)


;;; download methods

(defmethod download ((what (eql :wordnet))
                     &key (url "http://sourceforge.net/projects/wnsql/files/wnsql-1.0.1/wordnet30-sqlite-1.0.1.zip/download")
                          (dir "data/"))
  (let* ((dir (merge-pathnames dir (asdf:system-definition-pathname 'cl-nlp)))
         (archive (download-file url dir)))
    (format t "Downloading Wordnet from:~A to:~A~%It may take some time...~%"
            url dir)
    (unwind-protect
        (zip:with-zipfile (zip archive)
          (zip:do-zipfile-entries (name entry zip)
            (when (string= "wordnet30.sqlite" name)
              (write-bin-file (merge-pathnames name dir)
                              (zip:zipfile-entry-contents entry)))))
      (delete-file archive))))

(defmethod download ((what (eql :wordnet-ic))
                     &key (url "http://nltk.org/nltk_data/packages/corpora/wordnet_ic.zip")
                          (dir "data/"))
  (let ((dir (merge-pathnames dir (asdf:system-definition-pathname 'cl-nlp))))
    (format t "Downloading Wordnet IC from:~A to:~A~%It may take some time...~%"
            url dir)
    (download-file url dir)))


;;; SQL utilities

(defvar *cache* (make-hash-table :test 'equal)
  "Wordnet entities cache.")

(defun esc (str)
  (etypecase str
    (character (fmt "'~C'" str))
    (string (loop :for char :across str
               :when (or (member char '(#\Newline #\Return #\\ #\'))
                         (member (char-code char) '(#x00 #x1a)))
               :collect #\\ :into rez
               :collect char :into rez
               :finally (return (strcat "'" (coerce rez 'string) "'"))))))

(defun select (from &optional args)
  (values (fmt "(SELECT ~A ~{~A~^ ~})"
               (etypecase from
                 (symbol
                  (fmt "* FROM ~A" (clsql:view-table (find-class from))))
                 (list
                  (ds-bind (fields tables) (split :from from)
                    (fmt "~{~A~^,~} FROM ~{~A~^,~}"
                         (mapcar #`(if (listp %)
                                       (fmt "~A(~A)" (car %) (cadr %))
                                       %)
                                 fields)
                         tables))))
               (mapcar #`(case %
                           (:group-by "GROUP BY")
                           (:order-by "ORDER BY")
                           (t %))
                       args))
          (when (symbolp from) from)))


(defmacro query (sql)
  "A thin wrapper over CLSQL:QUERY to work with SELECT
   and cache Wordnet entities."
  (with-gensyms (sqlstr class rez slots row keys cache-key k v)
    `(mv-bind (,sqlstr ,class) ,sql
       (mv-bind (,rez ,slots) (clsql:query (substr ,sqlstr 1 -1)
                                           :flatp t)
         (if ,class
             (let ((,keys (mapcar #'c2mop:slot-definition-name
                                  (slot-value (find-class ,class)
                                              'clsql-sys::key-slots))))
               (mapcar (lambda (,row)
                         (let (,cache-key)
                           (loop :for ,k :in ,slots
                                 :for ,v :in ,row :do
                              (when-it (find (mksym ,k) ,keys)
                                (push (cons it ,v) ,cache-key)))
                           (or (get# ,cache-key *cache*)
                               (set# ,cache-key *cache*
                                     (apply #'make-instance ,class
                                            (mapcan #`(list (mkeyw %) %%)
                                                    ,slots ,row))))))
                       ,rez))
             ,rez)))))

(defmacro query1 (sqlstr)
  "Run a query for a single result."
  `(car (query ,sqlstr)))
