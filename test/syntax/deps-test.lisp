;;; (c) 2014 Vsevolod Dyomkin

(in-package #:nlp.deps)
(named-readtables:in-readtable rutils-readtable)


(deftest read-stanford-dep ()
  (shoud be equalp
         (make-dep :rel 'deps:nsubjpass
                   :govr (make-token :idx 8 :word "submitted")
                   :dept (make-token :idx 7 :word "Bills"))
         (read-stanford-dep "nsubjpass(submitted-8, Bills-7)"))
  (shoud be eql deps:+ROOT+
         (dep-govr (read-stanford-dep "root ( ROOT-0 , test-4 )"))))

(deftest read-dep-stanford ()
  (shoud be equalp
         (make-dep :rel 'desp:nmod
                   :govr (make-token :idx 1 :word "Pricing"
                                     :lemma "pricing" :tag 'tags:NN)
                   :dept (make-token :idx 2))
         (read-dep :conll "1    Pricing        pricing        NN     _    2    NMOD")))
