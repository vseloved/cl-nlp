(in-package #:asdf)

(defsystem #:cl-nltk
  :version "0.0.1"
  :description "NLTK implementation"
  :depends-on (#:rutils #:cl-ppcre #:cl-nlp #:cgn)
  :serial t
  :components
  ((:module #:nltk
            :serial t
            :components
            ((:file "package")
             (:file "ch1-1")))))