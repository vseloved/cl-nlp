;;; (c) 2013 Vsevolod Dyomkin

(cl:defpackage #:nlp.contrib.ms-ngrams
  (:use #:common-lisp #:rutil #:nlp
        #+dev #:should-test)
  (:export #:ms-ngrams
           #:ms-ngrams-url
           #:ms-ngrams-user-token
           #:ms-ngrams-catalog))

(cl:defpackage #:nlp.contrib.corpora
  (:use #:common-lisp #:rutil #:rutilsx #:nlp
        #+dev #:should-test)
  (:export #:ptb-tagged-text
           #:reuters-text
           #:semcor-token))
