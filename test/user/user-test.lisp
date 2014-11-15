;;; (c) 2013-2014 Vsevolod Dyomkin

(in-package #:nlp-user)
(named-readtables:in-readtable rutils-readtable)


(deftest tabulate ()
  (should print-to *standard-output*
"      A1  A   B1  B2
  ACD   1  2  133   0
    B   0  0    3  44
" (tabulate #{:acd #{:a1 1 :a 2 :b1 133} :b #{:b1 3 :b2 44}}))
  (should print-to *standard-output*
"      A1  A   B1   B2
  ACD   1  3  136  136
    B   0  0    3   47
" (tabulate #{:acd #{:a1 1 :a 2 :b1 133} :b #{:b1 3 :b2 44}}
            :cumulative t))
  (should print-to *standard-output*
"      A1
  ACD   1
    B   0
" (tabulate #{:acd #{:a1 1 :a 2 :b1 133} :b #{:b1 3 :b2 44}}
            :cols '(:a1)))
  (should print-to *standard-output*
"      1   2    3
  ACD  1  22  333
    B  0   0    3
" (tabulate #{:acd #{1 1 2 22 3 333} :b #{4 44 3 3}}
            :cols '(2 3 1) :order-by '<)))
