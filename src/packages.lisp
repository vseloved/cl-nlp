;;; (c) 2013-2014 Vsevolod Dyomkin


(cl:defpackage #:nlp.util
  (:nicknames #:nutil)
  (:use #:common-lisp #:rutilsx
        #+dev #:should-test)
  (:export #:cl-nlp-error
           #:not-implemented-error

           #:*stopwords-en*

           #:+newline+
           #:+newline-chars+
           #:newline-char-p
           #:+white-chars+
           #:white-char-p
           #:+period-chars+
           #:period-char-p
           #:+quote-chars+
           #:quote-char-p
           #:+open-quote-chars+
           #:open-quote-char-p
           #:+close-quote-chars+
           #:close-quote-char-p

           #:ending-word-p

           #:filler
           #:uniq
           #:sorted-ht-keys
           #:shorter?
           #:equal-when-present
           #:timestamp

           #:corpus-file
           #:data-file
           #:model-file
           #:src-file
           #:test-file
           #:write-bin-file
           #:write-dict
           #:write-tsv
           #:download
           #:download-file
           #:zipped-file-data
           #:zip-as-text-file

           #:list-from-file
           #:dict-from-file

           #:argmax
           #:keymax
           #:bin-search
           #:dot

           #:define-lazy-singleton

           #:pprint-tree
           #:princ-progress

           #:+inf
           ))

(cl:defpackage #:nlp.core
  (:nicknames #:ncore)
  (:use #:common-lisp #:rutilsx #:nlp.util
        #+dev #:should-test)
  (:export #:ngrams
           #:ngrams-eq
           #:ngrams-order
           #:ngrams-count
           #:ngrams-total-freq
           #:ngrams-max-freq
           #:ngrams-min-freq
           #:ngrams-pairs
           #:vocab
           #:freq
           #:prob
           #:logprob
           #:cond-prob
           #:cond-logprob
           #:freqs
           #:probs
           #:logprobs
           #:cond-probs
           #:cond-logprobs
           #:top-ngram
           #:hapaxes
           #:table-ngrams
           #:ngrams-table

           #:language-model
           #:m-order
           #:m-ngrams
           #:make-lm
           #:perplexity
           #:plain-lm
           #:stupid-backoff-lm
           #:lm-backoff

           #:count-ngram-freqs
           #:index-ngrams
           #:index-context-freqs
           #:index-prefix-transition-freqs
           #:index-word-transition-freqs
           #:normalize-freqs

           #:tokenize
           #:tokenizer
           #:regex-word-tokenizer
           #:postprocessing-regex-word-tokenizer
           #:baseline-sentence-tokenizer
           #:<word-chnuker>
           #:<basic-word-tokenizer>
           #:<word-tokenizer>
           #:<sentence-splitter>

           #:token
           #:token-word
           #:token-beg
           #:token-end
           #:token-tag
           #:make-token
           #:sentence
           #:sent-tokens

           #:doublenewline-paragraph-splitter
           #:<paragraph-splitter>

           #:find-collocations

           #:normalize
           #:*number-regex*
           #:*url-regex*
           #:*email-regex*

           #:make-cfd
           ))

(cl:defpackage #:nlp.tags
  (:nicknames #:tag)
  (:use #:common-lisp #:rutil #:nutil)
  (:export #:*word-tags*
           #:*phrase-tags*
           ))

(cl:defpackage #:nlp.deps
  (:nicknames #:dep)
  (:use #:common-lisp #:rutil #:nutil)
  (:export #:*deps*
           ))

(cl:defpackage #:nlp.features
  (:nicknames #:f)
  (:use #:common-lisp #:rutil #:nutil))

(cl:defpackage #:nlp.corpora
  (:nicknames #:ncorp)
  (:use #:common-lisp #:rutil #:nutil #:nlp.core #:tag
        #+dev #:should-test)
  (:export #:corpus
           #:make-corpus
           #:corpus-desc
           #:corpus-texts
           #:corpus-groups

           #:text
           #:make-text
           #:text-name
           #:text-raw
           #:text-clean
           #:text-tokens
           #:text-sentences
           #:text-paragraphs
           #:treebank-text-trees

           #:read-corpus
           #:read-corpus-file
           #:map-corpus

           #:make-corpus-from-dir

           #:remove-dummy-tokens

           ;; Specific corpora
           #:+brown-corpus+
           #:+nps-chat-corpus+
           ))

(cl:defpackage #:nlp.learning
  (:nicknames #:nlearn)
  (:use #:common-lisp #:rutilsx #:nlp.util #:nlp.core
        #+dev #:should-test)
  (:export #:init-model
           #:classify
           #:train
           #:train1
           #:update1

           #:accuracy
           #:precision
           #:recall
           #:f1
           #:f_

           #:save-model
           #:load-model

           ;; Features
           #:ensure-f-init
           #:extract-gold
           #:extract-fs
           #:make-fs

           ;; Models
           #:categorical-model
           #:m-weights

           ;; Perceptron Models
           #:perceptron
           #:avg-perceptron
           #:greedy-ap
           ))

(cl:defpackage #:nlp.tagging
  (:nicknames #:ntag)
  (:use #:common-lisp #:rutilsx #:nutil #:ncore #:nlearn #:tag
        #+dev #:should-test)
  (:export #:tag
           #:tagger
           #:tgr-single-tag-words

           ;; HMM taggers
           #:hmm-tagger

           ;; Perceptron taggers
           #:greedy-ap-tagger
           ))

(cl:defpackage #:nlp.parsing
  (:nicknames #:nparse)
  (:use #:common-lisp #:rutil #:nutil #:ncore #:tag
        #+dev #:should-test)
  (:export #:parse

           #:conparser
           #:depparser

           #:dep
           #:dep-rel
           #:dep-govr
           #:dep-dept

           #:pretagged
           #:lexicalized
           #:markovized

           #:cfg
           #:pcfg

           #:cky-parser
           #:pretagged-cky-parser

           #:grammar-ts
           #:grammar-nts
           #:grammar-nts-idx
           #:grammar-root
           #:grammar-unary-rules
           #:grammar-binary-rules
           #:grammar-root-rules
           #:grammar-iurules
           #:grammar-ibrules
           ))

(cl:defpackage #:nlp.generation
  (:nicknames #:ngen)
  (:use #:common-lisp #:rutil #:nlp.util #:nlp.core
        #+dev #:should-test)
  (:export #:generate-text

           #:text-generator

           ;; Markov Chain Generators
           #:markov-chain-generator
           #:mark-v-shaney-generator
           #:<mark-v-shaney>
           #:markov-order
           ))

(cl:defpackage #:nlp-user
  (:nicknames #:nlp)
  (:use #:common-lisp #:rutilsx
        #:nlp.util #:nlp.corpora #:nlp.core #:nlp.generation #:tag
        #+dev #:should-test)
  (:export #:grep
           #:tabulate
           #:plot
           ))


(rutils:re-export-symbols '#:nutil    '#:nlp-user)
(rutils:re-export-symbols '#:ncorp    '#:nlp-user)
(rutils:re-export-symbols '#:ncore    '#:nlp-user)
(rutils:re-export-symbols '#:nlearn   '#:nlp-user)
(rutils:re-export-symbols '#:ngen     '#:nlp-user)
(rutils:re-export-symbols '#:ntag     '#:nlp-user)
(rutils:re-export-symbols '#:nparse   '#:nlp-user)
