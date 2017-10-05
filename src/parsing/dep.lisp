;;; (c) 2014-2017 Vsevolod Dyomkin

(in-package #:nlp.parsing)
(named-readtables:in-readtable rutils-readtable)


(defvar *cn-numbatch*
  (nemb:init-vecs (make 'nemb:lazy-mem-vecs :order 300)
                  :text "~/ext4/numberbatch-en-17_04.txt" :prolog t)
  "English ConceptNet Numberbatch vectors.")



;;; parser

(defclass depparser ()
  ()
  (:documentation
   "Dependency parser that returns a list of deps for a sentence."))

(defclass arc-eager-deporacle (oracle)
  ()
  (:documentation
   "Ar-eager dynamic oracle for dependency parsing."))

(defclass greedy-sb-depparser (mgl:fnn depparser stack-buffer-parser)
  ((transitions :initarg :transitions :acessor parser-transitions
                :initform '(dep:left
                            dep:right
                            dep:shift))
   (trans-model :initarg :trans-model :accessor parser-trans-model)
   (label-model :initarg :label-model :accessor parser-label-model)
   (oracle :initarg :oracle :accessor parser-oracle
           :initform (make 'arc-eager-deporacle)))
  (:documentation
   "A shift-reduce greedy dependency parser."))

(defmethod parse ((parser greedy-sb-depparser) (sent sent))
  (with (((stack buffer ctx) @ parser)
         (heads (make-array n :element-type 'integer :initial-element 0))
         rez)
    (:= stack '(0)
        buffer (range 0 (1+ (length sent)))
        (? ctx :toks) @sent.toks)
    (loop :while (or stack buffer) :do
      (call (trans-fn (select-transition parser heads)) parser heads))
    (doindex (i tok @sent.toks)
      (let ((head (if (zerop i) dep:+root+
                      (? @sent.toks (? heads (1- i))))))
        (push (make-dep :rel (if (zerop i) 'dep:root
                                 ;;'dep:dep)
                                 (classify parser (list head tok rez)))
                        :head head 
                        :tail tok)
              rez)))
    rez))

(defmethod select-transition ((parser greedy-sb-depparser) interm)
  )

(defmethod train ((model greedy-sb-depparser) sents &key verbose)
  (let ((trans-fnn (mgl:build-fnn (:class 'greedy-sb-depparser)
                                  (in (mgl:->input :size (* *vecs-size* 3)))
                                  (hid-act (mgl:->activation in :size 10000))
                                  (hid (mgl:->relu hid-act))
                                  (out-act (mgl:->activation hid :size 3)
                                           (out (mgl:->softmax-xe-loss out-act)))))
        (opt (make 'mgl:segmented-gd-optimizer
                   :segmenter (constantly (make 'mgl:sgd-optimizer
                                                :learning-rate 1
                                                :momentum 0.9
                                                :batch-size 100))))
        (prev-size 0))
    (:= (mgl:max-n-stripes trans-fnn) 50)
    (mgl:map-segments (lambda (clump)
                        (mgl:gaussian-random! (mgl:nodes clump) :stddev
                                              ;; Xavier initializer
                                              (sqrt (/ 6 (+ prev-size
                                                            (length (mgl:nodes %))))))
                        (:= prev-size (length (mgl:nodes %))))
                      trans-fnn)
    (mgl:monitor-optimization-periodically
     opt '((:fn report-optimization-progress :period 10000)
           (:fn mgl:reset-optimization-monitors :period 1000)))
    (mgl:minimize opt
                  (make 'mgl:bp-learner
                        :bpn trans-fnn
                        :monitors (mgl:make-cost-monitors
                                   trans-fnn :attributes `(:event "train")))
                  :dataset (make-sampler 10000))
    (:= @model.trans-model trans-fnn
        @model.label-model label-fnn)))
  

;;; fs

(defmethod extract-fs ((model greedy-sb-depparser) &rest args)
  (with (((stack buffer ctx) @ parser)
         ((wb0 &optional wb1 wb2) ...)
         (b0w (tok-word wb0))
         (b0t (tok-pos wb0))
         (s0w (tok-word ws0))
         (s0t (tok-pos ws0)))
      (cons (make-fs "bias")
            (make-fs "s0w + n0w" s0w n0w)
            (make-fs "s0w + n0" s0w n0w n0t)
            (make-fs "s0t + n0" s0t n0w n0t)
            (make-fs "s0 + n0w" s0w s0t n0w)
            (make-fs "s0 + n0t" s0w s0t n0t)
            (make-fs "s0 + n0" s0w s0t n0w n0t)
            (make-fs "s0t + n0t" s0t n0t)
            (make-fs "n0t + n1t" n0t n1t)
            (mapindex #`(make-fs "ttt" i (first %) (second %) (third %))
                      (list (list n0t n1t n2t)
                            (list s0t s1t s2t)
                            (list s0t n0t n1t)
                            (list s0t s1t n0t)
                            (list s0t s0f1t n0t)
                            (list s0t s0b1t n0t)
                            (list s0t n0t n0b1t)
                            (list s0t s0b1t s0b2t)
                            (list s0t s0f1t s0f2t)
                            (list n0t n0b1t n0b2t)
                            (list n0t n0f1t n0f2t)))

            (mapindex ^(make-fs "word" @%%.word
                                "tag" @%%.tag
                                "tok" % @%%.word @%%.tag)
                      (list wn0 wn1 wn2 ws0 ws1 ws2
                            wn0b1 wn0b2 ws0b1 ws0b2 ws0f1 ws0f2)))

(defmethod extract-gold ((model greedy-sb-depparser) data)
  )


;;; transitions

(deftransition dep:shift ()
  (push (pop buffer) stack))

(macrolet ((add-arc (head tail label)
             (once-only (head tail)
               `(:= (? interm ,tail) (pair ,head ,label)
                    (? diff (pair ,head ,tail)) t))))
               
(deftransition dep:left (label)
  (add-arc (first buffer) (pop stack) label))
  
(deftransition dep:right (label)
  (add-arc (pop stack) (first stack) label))

) ; end of macrolet

(defmethod select-transition ((parser greedy-sb-depparser) interm)
  (let ((trs (list-transitions parser interm)))
    (values (argmax ^(score parser %.fn %.fs) trs
                    :test '>= :min (- 1e10))
            trs)))

(defmethod judge-transitions ((oracle arc-eager-deporacle)
                              (parser greedy-sb-depparser)
                              interm gold &key &allow-other-keys)
  (with (((stack buffer ctx) @ parser))
    (cond
      ((null stack)))))

(defmethod list-transitions ((parser greedy-sb-depparser) interm)
  (with (((stack buffer ctx) @ parser)
         (fs (extract-fs parser interm))
         (toks (? ctx :toks)))
    (mapcar ^(make-trans :fn % :fs fs)
            (append (when buffer '(dep:shift))
                    (when (and buffer stack) '(dep:left))
                    (when (rest stack) '(dep:right))))))

