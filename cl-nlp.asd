;;; (c) 2013 Vsevolod Dyomkin

(asdf:defsystem #:cl-nlp
  :version "0.0.5"
  :description "NLP toolkit for Common Lisp."
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :license "Apache 2.0"
  :depends-on (#:rutils #:cl-fad #:cl-ppcre
               #:cxml #:drakma #:zip #:flexi-streams
               #+dev #:should-test)
  :serial t
  :components
  ((:module #:src
            :serial t
            :components
            ((:file "packages")
             (:module #:util
                      :serial t
                      :components
                      ((:file "generics")
                       (:file "misc")
                       (:file "files")
                       (:file "chars")
                       (:file "words")
                       (:file "trees")
                       (:file "math")))
             (:module #:core
                      :serial t
                      :components
                      ((:file "general")
                       (:file "stats")
                       (:file "tokenization")
                       (:file "ngrams")
                       (:file "language-models")
                       (:file "indexing")
                       (:file "cond-freq-dist")))
             (:module #:corpora
                      :serial t
                      :components
                      ((:file "util")
                       (:file "corpus")
                       (:file "brown")
                       (:file "nps-chat")
                       (:file "reuters")
                       (:file "treebank")
                       (:file "user")))
             (:module #:generation
                      :serial t
                      :components
                      ((:file "markov-chain")))
             (:module #:tagging
                      :serial t
                      :components
                      ((:file "general")
                       (:file "hmm")))
             (:module #:parsing
                      :serial t
                      :components
                      ((:file "general")
                       (:file "tree-util")
                       (:file "grammars")
                       (:file "cky")))
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
             (:module #:core
                      :components
                      ((:file "stats-test")))))))
