;;; (c) 2013-2017 Vsevolod Dyomkin

(defsystem #:cl-nlp-contrib
  :version (:read-file-line "version.txt" :at 1)
  :description "CL-NLP additional packages."
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :license "Apache 2.0"
  :depends-on (#:cl-nlp #:closer-mop
               #:clsql #:clsql-sqlite3
               #+dev #:should-test)
  :serial t
  :components
  ((:module #:contrib
            :components
            ((:file "packages")
             (:file "ms-ngrams" :depends-on ("packages"))
             (:module #:wordnet
                      :serial t
                      :depends-on ("packages")
                      :components
                      ((:file "util")
                       (:file "wordnet")
                       (:file "db")
                       (:file "ic")
                       (:file "interface")
                       (:file "similarity")
                       (:file "sql-wordnet")
                       (:file "wn")))
             #+nil
             (:module #:lexics
                      :depends-on (#:wordnet)
                      :components
                      ((:file "wordnet")))
             (:module #:corpora
                      :depends-on ("packages")
                      :components
                      ((:file "wikipedia")
                       (:file "ptb")
                       (:file "nps-chat")
                       (:file "semcor")
                       (:file "reuters")))))
   #+dev
   (:module #:test
            :components
            ((:module #:corpora
                      :components
                      ((:file "wikipedia-test")
                       (:file "ptb-test")
                       (:file "nps-chat-test")
                       (:file "reuters-test")))))))



(defmethod asdf:perform :after ((o asdf:load-op)
                                (c (eql (asdf:find-system 'clsql))))
  (dolist (path '("lib/x86_64/" "lib/i386/"))
    (funcall (find-symbol (symbol-name '#:push-library-path)
                          (find-package 'clsql))
             (asdf:system-relative-pathname 'cl-nlp path))))
