;;; (c) 2013-2014 Vsevolod Dyomkin

(asdf:defsystem #:cl-nlp
  :version "0.0.13"
  :description "NLP toolkit for Common Lisp"
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :license "Apache 2.0"
  :depends-on (#:rutilsx #:cl-fad #:cl-ppcre
               #:cxml #:drakma #:flexi-streams
               #:zip #:archive #:chipz #:gzip-stream
               ;; #:cgn
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
                       (:static-file "deps.txt")
                       (:file "tags" :depends-on ("word-tags.txt"
                                                  "phrase-tags.txt"))
                       (:file "deps" :depends-on ("deps.txt"))))
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
                      :components
                      ((:file "general")
                       (:file "util" :depends-on ("general"))
                       (:file "brown" :depends-on ("util"))
                       (:file "xml" :depends-on ("util"))
                       (:file "treebank" :depends-on ("util"))
                       (:file "user" :depends-on ("brown" "xml" "treebank"))))
             (:module #:learning
                      :components
                      ((:file "general")
                       (:file "features" :depends-on ("general"))
                       (:file "perceptron" :depends-on ("features"))
                       (:file "decision-tree" :depends-on ("features"))
                       (:file "random-forest" :depends-on ("decision-tree"))))
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
                       (:file "grammars")
                       (:file "greedy-ap")))
             (:file "user")))
   #+dev
   (:module #:test
            :components
            ((:module #:util
                      :components
                      ((:file "trees-test")))
             (:module #:core
                      :components
                      ((:file "tokenization-test")))
             (:module #:corpora
                      :components
                      ((:file "treebank-test")))
             (:module #:learning
                      :components
                      ((:file "general-test")
                       (:file "decision-tree-test")))
             (:module #:tagging
                      :components
                      ((:file "greedy-ap-test")))
             (:module #:user
                      :components
                      ((:file "user-test")))
             (:file "ci")))))
