;;; (c) 2013 Vsevolod Dyomkin

(in-package #:asdf)


(defsystem #:cl-nlp-contrib
  :version "0.0.2"
  :description "CL-NLP additional packages."
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :license "Apache 2.0"
  :depends-on (#:cl-nlp #:closer-mop #:clsql #:clsql-sqlite3 #:zip)
  :serial t
  :components
  ((:module #:contrib
            :serial t
            :components
            ((:file "packages")
             (:file "ms-ngrams")
             (:module #:wordnet
                      :serial t
                      :components
                      ((:file "package")
                       (:file "util")
                       (:file "wordnet")
                       (:file "db")
                       (:file "ic")
                       (:file "interface")
                       (:file "similarity")
                       (:file "sql-wordnet")
                       (:file "wn")))))))


(defmethod asdf:perform :after ((o asdf:load-op)
                                (c (eql (asdf:find-system 'clsql))))
  (dolist (path '("lib/x86_64/" "lib/i386/"))
    (funcall (find-symbol (symbol-name '#:push-library-path)
                          (find-package 'clsql))
             (asdf:system-relative-pathname 'cl-nlp path))))
