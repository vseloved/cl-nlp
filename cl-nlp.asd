;;; (c) 2013 Vsevolod Dyomkin

(asdf:defsystem #:cl-nlp
  :version "0.0.9"
  :description "NLP toolkit for Common Lisp"
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :license "Apache 2.0"
  :depends-on (#:rutilsx #:cl-fad #:cl-ppcre
               #:cxml #:drakma #:zip #:userial ;#:cgn
               #+dev #:should-test)
  :serial t
  :components
  ((:module #:src
            :serial t
            :components
            ((:file "packages")
             (:module "util"
                      :serial t
                      :components
                      ((:file "misc")
                       (:file "chars")
                       (:file "files")
                       (:file "words")
                       (:file "trees")
                       (:file "math")))
             (:module #:syntax
                      :components
                      ((:static-file "word-tags.txt")
                       (:static-file "phrase-tags.txt")
                       (:file "tags" :depends-on ("word-tags.txt"
                                                  "phrase-tags.txt"))))
             (:module #:core
                      :serial t
                      :components
                      ((:file "general")
                       (:file "stats")
                       (:file "normalization")
                       (:file "tokenization")
                       (:file "ngrams")
                       (:file "language-models")
                       (:file "indexing")
                       (:file "cond-freq-dist")))
             (:module #:corpora
                      :serial t
                      :components
                      ((:file "general")
                       (:file "util")
                       (:file "brown")
                       (:file "nps-chat")
                       (:file "reuters")
                       (:file "treebank")
                       (:file "ptb")
                       (:file "user")))
             (:module #:learning
                      :serial t
                      :components
                      ((:file "general")
                       (:file "features")
                       (:file "perceptron")))
             (:module #:generation
                      :serial t
                      :components
                      ((:file "markov-chain")))
             (:module #:tagging
                      :serial t
                      :components
                      ((:file "general")
                       (:file "dicts")
                       (:file "greedy-ap")))
             (:module #:parsing
                      :serial t
                      :components
                      ((:file "general")
                       (:file "tree-util")
                       (:file "grammars")))
             (:file "user")))
   #+dev
   (:module #:test
            :components
            ((:module #:util
                      :components
                      ((:file "trees-test")))
             (:module #:corpora
                      :components
                      ((:file "treebank-test")))
             (:module #:learning
                      :components
                      ((:file "general-test")))
             (:module #:tagging
                      :components
                      ((:file "greedy-ap-test")))
             (:module #:user
                      :components
                      ((:file "user-test")))))))
