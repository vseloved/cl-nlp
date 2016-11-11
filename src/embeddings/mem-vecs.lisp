;;; (c) 2016 Vsevolod Dyomkin

(in-package #:nlp.embeddings)
(named-readtables:in-readtable rutilsx-readtable)


(defclass mem-vecs (vecs)
  ((dict :initarg :dict :accessor vecs-dict)
   (default :initarg :default :accessor vecs-default))
  (:documentation
   "Word vectors stored in memory."))

(defmethod slot-unbound (class (instance mem-vecs) (slot-name (eql 'default)))
  (:= @instance.default (make-array @instance.order :element-type 'single-float)))

(defclass lazy-mem-vecs (mem-vecs)
  ((understream :initarg :understream :accessor vecs-understream))
  (:documentation
   "Word vectors stored in memory, but loaded only on-demand."))


(defmethod 2vec ((vecs mem-vecs) word &key normalize)
  (get# word @vecs.dict))

(defun read-word (stream &optional return)
  (loop :for char := (read-char stream nil)
        :until (or (null char)
                   (member char '(#\Space #\Tab)))
        :collect char :into word
        :finally (when return
                   (return (coerce (reverse word) 'string)))))
  
(defmethod 2vec ((vecs lazy-mem-vecs) word &key normalize)
  (let ((rez (get# word @vecs.dict)))
    (cond ((numberp rez)
           (file-position @vecs.understream rez)
           (read-word @vecs.understream)
           (set# word @vecs.dict (read-vec @vecs.order @vecs.understream)))
          ((null rez)
           @vecs.default)
          (t rez))))

(defgeneric init-vecs (vecs file)
  (:documentation
   "Initialize word VECS from FILE.")
  (:method ((vecs mem-vecs) file)
    (let ((dict #h(equal)))
      (with-open-file (in file)
        (loop :for word := (read-word in t) :while word :do
          (:= (? dict word) (read-vec @vecs.order in))))
      (:= @vecs.dict dict)
      vecs))
  (:method ((vecs lazy-mem-vecs) file)
    (let ((dict #h(equal))
          (off 0)
          (in (open file :external-format :utf8)))
      (:= @vecs.understream in)
      (loop :for line := (read-line in nil) :while line :do
        (let ((word (slice line 0 (position-if ^(member % '(#\Space #\Tab))
                                               line))))
          (:= (? dict word) off)
          (:= off (file-position in))))
      (:= @vecs.dict dict)
      vecs)))

(defun read-vec (order stream)
  "Read word vectors of ORDER from stream"
  (let ((rez (make-array order :element-type 'single-float)))
    (dotimes (i order)
      (:= (aref rez i) (coerce (read stream) 'single-float)))
    rez))
