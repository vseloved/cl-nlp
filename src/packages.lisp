;;; (c) 2013-2016 Vsevolod Dyomkin


(cl:defpackage #:nlp.util
  (:nicknames #:nutil)
  (:use #:common-lisp #:rutilsx
        #+dev #:should-test)
  (:export #:cl-nlp-error
           #:not-implemented-error

           #:+utf-8+

           #:*stopwords-en*

           #:+newline+
           #:+newline-chars+
           #:newline-char-p
           #:+white-chars+
           #:white-char-p
           #:+period-chars+
           #:period-char-p
           #:+punct-chars+
           #:punct-char-p
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
           #:lang-file
           #:src-file
           #:test-file

           #:dolines*
           #:dofiles
           #:with-tmp-file
           #:write-bin-file
           #:write-tsv

           #:write-dict
           #:list-from-file
           #:dict-from-file

           #:download
           #:download-file

           #:zipped-file-data
           #:zip-add-text-file
           #:do-zip-entries
           #:with-zip
           #:with-zipped-zip

           #:argmax
           #:keymax
           #:~=
           #:sum
           #:frobenius-norm
           #:bin-search
           #:dot

           #:pprint-tree
           #:princ-progress

           #:+inf

           #:s!

           #:make-cfd
           ))

(cl:defpackage #:nlp.core
  (:nicknames #:ncore)
  (:use #:common-lisp #:rutilsx #:nlp.util
        #+dev #:should-test)
  (:export #:+iso-639-1+
           #:*lang-profiles*
           #:iso-lang
           #:lang-iso
           #:in-lang
           #:def-lang-var

           #:id
           #:word
           #:pos
           #:lemma
           #:beg
           #:end

           #:token
           #:token-id
           #:token-word
           #:token-lemma
           #:token-beg
           #:token-end
           #:token-pos
           #:make-token

           #:sent
           #:make-sent
           #:sent-tokens
           
           #:ngrams
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
           #:baseline-sent-tokenizer
           #:full-text-tokenizer
           #:doublenewline-parag-splitter
           #:parags->text
           #:<word-chunker>
           #:<basic-word-tokenizer>
           #:<word-tokenizer>
           #:<sent-splitter>
           #:<full-text-tokenizer>
           #:<parag-splitter>

           #:find-collocations

           #:normalize
           #:denormalize
           #:*number-regex*
           #:*url-regex*
           #:*email-regex*
           ))

(cl:defpackage #:nlp.corpora
  (:nicknames #:ncorp)
  (:use #:common-lisp #:rutilsx #:nutil #:nlp.core
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
           #:text-tokenized
           #:text-trees

           #:read-corpus
           #:read-corpus-file
           #:map-corpus

           #:walk-corpus-dir

           #:sax-progress
           #:sax-progress-count
           #:sax-progress-report-rate
           #:sax-progress-tracking-tag
           #:xml-corpus-sax
           #:xml-progress-mixin
           #:xml-attr

           #:make-corpus-from-dir

           #:+brown-corpus+
           ))

(cl:defpackage #:nlp.lexics
  (:nicknames #:nlex)
  (:use #:common-lisp #:rutilsx #:nlp.util #:nlp.core
        #+dev #:should-test)
  (:export #:dict
           #:<dict>
           
           #:lookup
           #:pos-tags

           #:stemmer
           #:stem

           #:porter-stemmer
           #:<porter-stemmer>

           #:lemmatizer
           #:lemmatizer-dict
           #:lemmatize
           #:morph

           #:mem-dict
           #:load-mem-dict))

(cl:defpackage #:nlp.embeddings
  (:nicknames #:nemb)
  (:use #:common-lisp #:rutilsx #:nlp.util #:nlp.core
        #+dev #:should-test)
  (:export #:2vec
           #:vecs
           #:load-vecs
           #:<glove>))

(cl:defpackage #:nlp.learning
  (:nicknames #:nlearn)
  (:use #:common-lisp #:rutilsx #:nlp.util #:nlp.core
        #+dev #:should-test)
  (:export #:init-model
           #:score
           #:rank
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
           #:extract-gold
           #:ensure-fs-init
           #:extract-fs
           #:make-fs

           ;; Models
           #:categorical-model
           #:m-weights

           ;; Perceptron models
           #:perceptron
           #:avg-perceptron
           #:training-perceptron

           ;; Decision tree models
           ))

(cl:defpackage #:nlp.tagging
  (:nicknames #:ntag)
  (:use #:common-lisp #:rutilsx #:nutil #:ncore #:nlearn
        #+dev #:should-test)
  (:export #:tag
           #:tagger
           #:tagger-dict
           #:tagger-single-pos-words
           #:greedy-ap-dict-postagger
           #:<pos-tagger>
           ))

(cl:defpackage #:nlp.parsing
  (:nicknames #:nparse)
  (:use #:common-lisp #:rutilsx #:nutil #:ncore #:nlearn
        #+dev #:should-test)
  (:shadow #:merge)
  (:export #:parse
           #:parser

           #:oracle
           #:ranking-oracle
           #:better-oracle
           #:best-oracle

           #:dep
           #:dep-rel
           #:dep-govr
           #:dep-dept
           #:make-dep

           #:parsed-sent
           #:sent-tree
           #:sent-deps
           #:sent-amr

           #:grammar-ts
           #:grammar-nts
           #:grammar-nts-idx
           #:grammar-root
           #:grammar-unary-rules
           #:grammar-binary-rules
           #:grammar-root-rules
           #:grammar-iurules
           #:grammar-ibrules

           #:pprint-tags
           #:pprint-tree
           #:pprint-deps
           #:pprint-deps+tags
           #:pprint-syntax

           #:stack-buffer-parser
           #:parser-actions
           #:def-sb-action
           
           #:depparser
           ;; #:greedy-sb-depparser

           ;; #:amrparser
           ;; #:greedy-sb-amrparser
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
           #:markov-order
           #:<mark-v-shaney>
           ))

(cl:defpackage #:nlp-user
  (:nicknames #:nlp)
  (:use #:common-lisp #:rutilsx
        #:nlp.util #:nlp.corpora #:nlp.core #:nlp.lexics
        #:nlp.generation #:nlp.learning #:nlp.tagging #:nlp.parsing
        #+dev #:should-test)
  (:export ;; chars
           #:newline-char-p
           #:+white-chars+
           #:white-char-p
           #:+period-chars+
           #:period-char-p
           #:+punct-chars+
           #:punct-char-p
           #:+quote-chars+
           #:quote-char-p
           #:+open-quote-chars+
           #:open-quote-char-p
           #:+close-quote-chars+
           #:close-quote-char-p

           ;; langs
           #:iso-lang
           #:lang-iso
           #:in-lang
           #:def-lang-var
           
           ;; tokens
           #:tokenize
           #:parags->text
           #:<word-chunker>
           #:<basic-word-tokenizer>
           #:<word-tokenizer>
           #:<sent-splitter>
           #:<full-text-tokenizer>
           #:<parag-splitter>
           
           ;; lexics
           #:known?
           #:pos-tags
           #:stem
           #:lemmatize
           #:morph
           #:<porter-stemmer>
           #:<wikt-lemmatizer>

           ;; tagging
           #:tag
           #:<pos-tagger>
           ;; #:<ner-tagger>

           ;; parsing
           #:parse
           ;; #:<const-parser>
           #:<dep-parser>
           ;; #:<amr-parser>
           
           ;; util
           #:grep
           #:tabulate
           #:plot

           ;; conditions
           #:cl-nlp-error
           #:not-implemented-error
           ))

;; (rutils:re-export-symbols '#:nutil    '#:nlp-user)
;; (rutils:re-export-symbols '#:ncorp    '#:nlp-user)
;; (rutils:re-export-symbols '#:ncore    '#:nlp-user)
;; (rutils:re-export-symbols '#:nlex     '#:nlp-user)
;; (rutils:re-export-symbols '#:nlearn   '#:nlp-user)
;; (rutils:re-export-symbols '#:ngen     '#:nlp-user)
;; (rutils:re-export-symbols '#:ntag     '#:nlp-user)
;; (rutils:re-export-symbols '#:nparse   '#:nlp-user)


;; special namespaces

(cl:defpackage #:nlp.tags
  (:nicknames #:tag)
  (:use #:common-lisp #:rutilsx #:nutil #:ncore)
  (:export #:<word-tags>
           #:<phrase-tags>
           #:export-tag
           ))

(cl:defpackage #:nlp.deps
  (:nicknames #:dep)
  (:use #:common-lisp #:rutilsx #:nutil #:ncore)
  (:export #:<dep-tags>
           #:+root+
           ))

;; (cl:defpackage #:nlp.amr
;;   (:nicknames #:amr)
;;   (:use #:common-lisp #:rutil #:nutil #:ncore)
;;   (:export #:<amr-tags>
;;            ))
