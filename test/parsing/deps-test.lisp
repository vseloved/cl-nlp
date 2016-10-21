;;; (c) 2014-2015 Vsevolod Dyomkin

(in-package #:nlp.parsing)
(named-readtables:in-readtable rutils-readtable)


(deftest read-stanford-dep ()
  (should be equalp
         (make-dep :rel 'dep:nsubjpass
                   :govr (make-token :id 8 :word "submitted")
                   :dept (make-token :id 7 :word "Bills"))
         (read-stanford-dep "nsubjpass(submitted-8, Bills-7)"))
  (should be eql dep:+ROOT+
         (dep-govr (read-stanford-dep "root ( ROOT-0 , test-4 )"))))

(deftest read-dep-stanford ()
  (should be equalp
         (make-dep :rel 'dep:mod
                   :govr (make-token :id 1 :word "Pricing"
                                     :lemma "pricing" :tag 'tag:NN)
                   :dept (make-token :id 2))
         (read-dep :conll "1    Pricing        pricing        NN     _    2    NMOD"))
  (should be equalp
          (make-dep :rel 'dep:mod
                    :govr (make-token :id 1 :word "Pricing" :tag 'tag:NN)
                    :dept (make-token :id 2))
          (read-dep :conll "1    Pricing        _        NN     _    2    NMOD")))
