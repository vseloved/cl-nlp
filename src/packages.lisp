;;; (c) 2013 Vsevolod Dyomkin


(cl:defpackage #:nlp.util
  (:nicknames #:nutil)
  (:use #:common-lisp #:rutil)
  (:export #:cl-nlp-error
           #:not-implemented-error

           #:generic-elt
           #:~
           #:pair
           #:l
           #:r
           #:vals
           #:keys
           #:pairs
           #:ht->pairs
           #:pairs->ht
           #:maptable
           #:donext

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

           #:+project-root+
           #:data-file
           #:list-from-file

           #:argmax
           #:bin-search

           #:define-lazy-singleton

           #:download-file
           #:download
           #:write-bin-file

           #:maptree
           #:mapleaves
           #:dotree
           #:doleaves
           #:pprint-tree

           #:equal-when-present
           #:write-tsv
           ))

(cl:defpackage #:nlp.core
  (:nicknames #:ncore)
  (:use #:common-lisp #:rutil #:nlp.util)
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
           #:model-order
           #:model-ngrams
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

           #:doublenewline-paragraph-splitter
           #:<paragraph-splitter>

           #:find-collocations

           #:normalize
           #:train

           #:make-cfd
           ))

(cl:defpackage #:nlp.corpora
  (:nicknames #:ncorpus)
  (:use #:common-lisp #:rutil #:nlp.util #:nlp.core)
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

           #:read-corpus
           #:read-corpus-file
           #:map-corpus

           #:make-corpus-from-dir

           #:+brown-corpus+
           #:+nps-chat-corpus+
           ))

(cl:defpackage #:nlp.tagging
  (:nicknames #:ntag)
  (:use #:common-lisp #:rutil #:nutil #:ncore)
  (:export #:+stop-tag+

           #:tag

           #:hmm
           #:make-hmm
           #:hmm-transition-lm
           #:hmm-emission-lm

           #:glm
           #:make-glm

           #:model-tags
           ))

(cl:defpackage #:nlp.parsing
  (:nicknames #:nparse)
  (:use #:common-lisp #:rutil #:nutil #:ncore)
  (:export #:chomsky-nf

           #:parse
           #:parse-n

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
  (:use #:common-lisp #:rutil #:nlp.util #:nlp.core)
  (:export #:generate-text

           #:text-generator

           #:markov-chain-generator
           #:mark-v-shaney-generator
           #:<mark-v-shaney>
           #:markov-order
           ))

(cl:defpackage #:nlp-user
  (:nicknames #:nlp)
  (:use #:common-lisp #:rutil
        #:nlp.util #:nlp.corpora #:nlp.core #:nlp.generation)
  (:export #:grep
           #:tabulate
           #:plot
           ))


(rutils:re-export-symbols '#:nutil    '#:nlp-user)
(rutils:re-export-symbols '#:ncorpus  '#:nlp-user)
(rutils:re-export-symbols '#:ncore    '#:nlp-user)
(rutils:re-export-symbols '#:ngen     '#:nlp-user)
(rutils:re-export-symbols '#:ntag     '#:nlp-user)
(rutils:re-export-symbols '#:nparse   '#:nlp-user)
