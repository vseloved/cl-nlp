;;; (c) 2013 Vsevolod Dyomkin

(in-package #:asdf)

(defsystem #:coursera-nlang
  :version "0.0.1"
  :description "Tasks for Coursera Natural Language Processing course from Columbia University by Michael Collins"
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :license "Apache 2.0"
  :depends-on (#:rutils #:cl-ppcre #:cl-nlp #:cl-json)
  :serial t
  :components
  ((:module #:coursera-nlang
            :serial t
            :components
            ((:file "package")
             (:file "task1")))))
