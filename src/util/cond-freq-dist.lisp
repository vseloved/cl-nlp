;;; (c) 2013-2016 Vsevolod Dyomkin

(in-package #:nlp.util)
(named-readtables:in-readtable rutils-readtable)


(defgeneric make-cfd (raw &key eq-test &allow-other-keys)
  (:documentation
   "Create a conditional frequency distribution from raw data."))


(defmethod make-cfd ((raw hash-table) &key (eq-test 'eql) &allow-other-keys)
  (let ((rez (make-hash-table :test eq-test)))
    (dotable (k _ raw)
      (set# k rez (make 'table-ngrams :order 1
                        :table (count-ngram-freqs (get# k raw)))))
    rez))

(defmethod write-tsv (cfd &key keys cols cumulative (order-by (constantly nil)))
  (let* ((filename (fmt "/tmp/~A" (gensym)))
         (conds (or keys (keys cfd)))
         (samples (or cols (vocab (first (vals cfd)) :order-by order-by)))
         (totals (make-hash-table :test (eq-test cfd))))
    (with-out-file (out filename)
      (format out "No~tLabel~t~{~A~t~}~%" conds)
      (doindex (i sample samples)
        (format out "~A~t~A~t~{~A~t~}~%" i sample
                (mapcar #`(let ((val (? cfd % sample)))
                            (if cumulative
                                (incf (get# % totals 0) val)
                                val))
                        conds))))
    (values filename
            (length samples)
            (length conds))))
