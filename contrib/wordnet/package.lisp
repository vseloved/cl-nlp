;;; (c) 2013 Vsevolod Dyomkin

(cl:defpackage #:nlp.contrib.wordnet
  (:nicknames #:wordnet)
  (:use #:common-lisp #:rutil #:nlp.util #:nlp)
  (:export #:wordnet
           #:wordnet-uri
           #:sql-wordnet3
           #:<wordnet>

           #:with-wordnet
           #:connect-wordnet

           #:lemma
           #:lemma-word
           #:lemma-id
           #:lemmas

           #:synset
           #:synset-id
           #:synset-pos
           #:synset-def
           #:synset-name
           #:synsets

           #:sense
           #:sense-id
           #:senses

           #:sample
           #:sample-id
           #:samples

           #:link-type
           #:link-types
           #:link-id
           #:link-word1
           #:link-word2
           #:link-synset1
           #:link-synset2
           #:lexlink
           #:semlink

           #:words
           #:examples
           #:related

           #:lowest-common-hypernyms
           #:min-depth
           #:hypernym-paths
           #:path-similarity
           #:lch-similarity
           #:wup-similarity
           #:res-similarity
           #:jcn-similarity
           #:lin-similarity))

(cl:defpackage #:nlp.contrib.wn
  (:nicknames #:wn)
  (:use #:common-lisp #:rutil)
  (:export #:lemma
           #:lemmas
           #:synset
           #:synsets
           #:sense
           #:senses
           #:samples
           #:words
           #:examples
           #:related
           #:lch
           #:min-depth
           #:hypernym-paths
           #:path-similarity
           #:lch-similarity
           #:wup-similarity
           #:res-similarity
           #:jcn-similarity
           #:lin-similarity))
