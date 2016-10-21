;;; (c) 2013-2016 Vsevolod Dyomkin

(asdf:defsystem #:cl-nlp
  :version (:read-file-line "version.txt" :at 0)
  :description "NLP toolkit for Common Lisp"
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :license "Apache 2.0"
  :depends-on (#:rutilsx #:cl-fad #:cl-ppcre
               #:cxml #:drakma #:flexi-streams
               #:zip #:archive #:chipz #:gzip-stream
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
                       (:file "math")
                       (:file "cond-freq-dist")))
             (:module #:core
                      :serial t
                      :components
                      ((:file "general")
                       (:file "lang")
                       (:file "stats")
                       (:file "normalization")
                       (:file "tokenization")
                       (:file "ngrams")
                       (:file "language-models")
                       (:file "indexing")))
             (:module #:lexics
                      :serial t
                      :components
                      ((:file "general")
                       (:file "porter")
                       (:file "dict-lemmatizer")))
             (:module #:embeddings
                      :serial t
                      :components
                      ((:file "general")
                       (:file "mem-vecs")))
             (:module #:syntax
                      :components
                      ((:file "tags")
                       (:file "deps")))
             (:module #:corpora
                      :components
                      ((:file "general")
                       (:file "brown" :depends-on ("general"))
                       (:file "xml" :depends-on ("general"))
                       (:file "treebank" :depends-on ("general"))
                       (:file "amrbank" :depends-on ("general"))
                       (:file "user" :depends-on ("brown" "xml" "treebank"
                                                  "amrbank"))))
             (:module #:learning
                      :components
                      ((:file "general")
                       (:file "features" :depends-on ("general"))
                       (:file "perceptron" :depends-on ("features"))
                       (:file "decision-tree" :depends-on ("features"))
                       (:file "random-forest" :depends-on ("decision-tree"))
                       (:file "boosted-trees" :depends-on ("decision-tree"))))
             (:module #:generation
                      :components
                      ((:file "markov-chain")))
             (:module #:tagging
                      :serial t
                      :components
                      ((:file "general")
                       (:file "dicts")
                       (:file "pos")))
             (:module #:parsing
                      :serial t
                      :components
                      ((:file "general")
                       (:file "tree-util")
                       (:file "grammars")
                       (:file "dependency")
                       (:file "stack-buffer")))
             (:file "user")))
   #+dev
   (:module #:test
            :components
            ((:module #:util
                      :components
                      ((:file "trees-test")))
             (:module #:lexics
                      :components
                      ((:file "porter-test")))
             (:module #:core
                      :components
                      ((:file "tokenization-test")))
             (:module #:corpora
                      :components
                      ((:file "brown-test")
                       (:file "treebank-test")
                       (:file "xml-test")))
             (:module #:learning
                      :components
                      ((:file "general-test")
                       (:file "decision-tree-test")))
             (:module #:tagging
                      :components
                      ((:file "pos-test")))
             (:module #:parsing
              #+nil :components
              #+nil ((:file "dep-test")
                     (:file "amr-test")))
             (:module #:user
                      :components
                      ((:file "user-test")))
             (:file "ci")))))
