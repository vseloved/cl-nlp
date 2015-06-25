;;; (c) 2015 Vsevolod Dyomkin

(in-package #:nlp.contrib.corpora)
(named-readtables:in-readtable rutils-readtable)

(deftest read-reuters-rcv1 ()
  (should be string=
          "Emerging evidence that Mexico's economy was back on the recovery track sent Mexican markets into a buzz of excitement Tuesday, with stocks closing at record highs and interest rates at 19-month lows."
          (slice #1=(text-clean (read-corpus-file
                                 :reuters-rcv1
                                 (corpus-file "samples/reuters-rcv1-2286newsML.xml")))
                 0 (1+ (position #\. #1#)))))
