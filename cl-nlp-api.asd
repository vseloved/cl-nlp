;;; (c) 2017 Vsevolod Dyomkin

(defsystem #:cl-nlp-api
  :version (:read-file-line "version.txt" :at 2)
  :description "CL-NLP based microservices API for Ukrainian language."
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :license "Apache 2.0"
  :depends-on (#:rutilsx #:cl-nlp
                         #:woo #:local-time #:yason #:hunchentoot
                         #:babel #:flexi-streams #:verbose #:cl-who)
  :serial t
  :components
  ((:module #:api
    :serial t
    :components
    ((:file "packages")
     (:file "core")
     (:file "parsing")
     (:file "tokenization")
     (:file "lemmatization")
     (:file "embeddings")))))
