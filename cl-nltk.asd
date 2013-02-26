;;; (c) 2013 Vsevolod Dyomkin

(in-package #:asdf)

(defsystem #:cl-nltk
  :version "0.0.2"
  :description "Implementation of the examples from the NLTK book."
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :license "Apache 2.0"
  :depends-on (#:rutils #:cl-ppcre #:cl-nlp #:cgn)
  :serial t
  :components
  ((:module #:nltk
            :serial t
            :components
            ((:file "package")
             (:file "ch1-1")
             (:file "ch1-2")))))
