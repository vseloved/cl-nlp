;;; (c) 2013 Vsevolod Dyomkin

(cl:defpackage #:nlp.util
  (:nicknames #:nutil)
  (:use #:common-lisp #:rutil)
  (:export #:cl-nlp-error
           #:not-implemented-error

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

           #:*stopwords-en*
           #:ending-word-p

           #:filler

           #:+project-root+
           #:data-file
           #:list-from-file

           #:bin-search

           #:define-lazy-singleton
           #:sorted-ht-keys

           #:shorter?

           #:download-file
           #:download
           #:write-bin-file
           ))

(cl:defpackage #:nlp.corpora
  (:nicknames #:ncorpus)
  (:use #:common-lisp #:rutil #:nlp.util)
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

           #:read-corpus
           #:read-corpus-file
           #:map-corpus

           #:+brown-corpus+
           #:+nps-chat-corpus+
           #:+reuters-corpus+
           ))

(cl:defpackage #:nlp.test-util
  (:nicknames #:ntest)
  (:use #:common-lisp #:rutil #:nlp.util #:nlp.corpora)
  (:export ))

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

           #:index-ngrams
           #:index-context-freqs
           #:index-prefix-transition-freqs
           #:index-word-transition-freqs
           #:normalize-freqs

           #:tokenize
           ;; #:stream-tokenize
           #:tokenizer
           #:regex-word-tokenizer
           #:postprocessing-regex-word-tokenizer
           #:baseline-sentence-tokenizer
           #:<word-chnuker>
           #:<basic-word-tokenizer>
           #:<word-tokenizer>
           #:<sentence-tokenizer>

           #:token
           #:token-word
           #:token-beg
           #:token-end
           #:token-tag
           #:make-token

           #:doublenewline-paragraph-splitter
           #:<paragraph-splitter>

           #:find-collocations
           ))

;; (cl:defpackage #:nlp.phonetics
;;   (:nicknames #:npho)
;;   (:use #:common-lisp #:rutil)
;;   (:export #:phonetic-transform
;;            ))

(cl:defpackage #:nlp.syntax
  (:nicknames #:nsyn)
  (:use #:common-lisp #:rutil #:nutil #:ncore)
  (:export #:tag

           #:model-tags
           #:+stop-tag+

           #:hmm-tagger
           #:make-hmm
           #:hmm-transition-lm
           #:hmm-emission-lm

           #:parse
           #:parse-n

           #:cfg
           #:pcfg

           #:gr-ts
           #:gr-nts
           #:gr-nts-idx
           #:gr-root
           #:gr-rules
           #:gr-irules
           #:gr-root-rules

           #:maptree
           #:mapleaves
           #:dotree
           #:doleaves
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
  (:export #:grep))


(rutils:re-export-symbols '#:nutil    '#:nlp-user)
(rutils:re-export-symbols '#:ncorpus    '#:nlp-user)
(rutils:re-export-symbols '#:ncore    '#:nlp-user)
(rutils:re-export-symbols '#:ngen     '#:nlp-user)
(rutils:re-export-symbols '#:nsyn     '#:nlp-user)