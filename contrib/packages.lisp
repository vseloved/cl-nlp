;;; (c) 2013 Vsevolod Dyomkin

(cl:defpackage #:nlp.contrib.ngrams
  (:use #:common-lisp #:rutil #:nlp)
  (:export #:ms-ngrams
           #:ms-ngrams-url
           #:ms-ngrams-user-token
           #:ms-ngrams-catalog))
