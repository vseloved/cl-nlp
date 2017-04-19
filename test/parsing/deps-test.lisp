;;; (c) 2014-2015 Vsevolod Dyomkin

(in-package #:nlp.parsing)
(named-readtables:in-readtable rutils-readtable)


(deftest read-dep-stanford ()
  (should be equalp
         (make-dep :rel 'dep:nsubjpass
                   :head (make-tok :id 8 :word "submitted")
                   :child (make-tok :id 7 :word "Bills"))
         (read-dep :stanford "nsubjpass(submitted-8, Bills-7)"))
  (should be eql dep:+root+
         (dep-head (read-dep :stanford "root ( test-4 , test-4 )"))))

(deftest read-dep-conll ()
  (should be equalp
         (make-dep :rel 'dep::NMOD
                   :child (make-tok :id 1 :word "Pricing"
                                   :lemma "pricing" :pos 'tag:NN)
                   :head (make-tok :id 2))
         (read-dep :conll "1	Pricing	pricing	NN	_	_	2	NMOD"))
  (should be equalp
          (make-dep :rel 'dep::NMOD
                    :child (make-tok :id 1 :word "Pricing" :pos 'tag:NN)
                    :head (make-tok :id 2))
          (read-dep :conll "1	Pricing	_	NN	_	_	2	NMOD")))
