;;; (c) 2015-2016 Vsevolod Dyomkin

(in-package #:nlp.learning)
(named-readtables:in-readtable rutilsx-readtable)


(defclass decision-tree ()
  ((decision-fn :accessor tree-decision-fn :initarg :decision-fn)
   (repr :accessor tree-repr :initarg :repr)
   (classes :accessor tree-classes :initarg :classes)
   (max-depth :accessor tree-max-depth :initform nil :initarg :max-depth)
   (min-size :accessor tree-min-size :initform 0 :initarg :min-size)))

(defclass c4.5-tree (decision-tree)
  ())

(defclass cart-tree (decision-tree)
  ())


;;; main methods

(defmethod classify ((model decision-tree) fs &key classes)
  (declare (ignore classes))
  (funcall (tree-decision-fn model) fs))

(defmethod train ((model c4.5-tree) data
                  &key classes verbose idx-count
                       (idxs (range 0 (length (lt (first data))))))
  (let ((tree (tree-train 'info-gain data
                          :binary nil
                          :min-size (tree-min-size model)
                          :max-depth (tree-max-depth model)
                          :idxs idxs :idx-count idx-count
                          :verbose verbose)))
    (:= (tree-decision-fn model)  (eval `(lambda (%) ,tree))
        (tree-repr model) tree
        (tree-classes model) classes)
    model))

(defmethod train ((model cart-tree) data
                  &key classes verbose idx-count
                       (idxs (range 0 (length (lt (first data))))))
  (let ((tree (tree-train ^(- 1 (gini-split-idx %)) data
                          :binary t
                          :min-size (tree-min-size model)
                          :max-depth (tree-max-depth model)
                          :idxs idxs :idx-count idx-count
                          :verbose verbose)))
    (:= (tree-decision-fn model)  (eval `(lambda (%) ,tree))
        (tree-repr model) tree
        (tree-classes model) classes)
    model))

(defun tree-train (criterion data
                   &key (min-size 0) (depth 0) max-depth
                        (idxs (range 0 (length (lt (first data)))))
                        idx-count  ;; for RF classifier to sample dimensions
                        binary
                        verbose)
  "Train a decision tree from DATA using a CRITERION function."
  (princ ".")
  (cond
    ;; no data
    ((endp data) nil)
    ;; single class
    ((single (uniq (mapcar 'rt data)))
     `(pair ',(rt (first data)) 1))
    ;; no indices, min-size or max-depth reached
    ((or (endp idxs)
         (when max-depth (>= depth max-depth))
         (<= (length data) min-size))
     (with ((dist (maptab ^(length %%)
                          (partition-by 'rt data)))
            (total (reduce '+ (ht-vals dist)))
            (key max (keymax dist)))
         `(when (eql % ',key)
            (pair ',key ,(float (/ max total))))))
    ;; general case
    (t (with ((idx binary? split-point test
                   (split-idx criterion data
                              (if idx-count
                                  (sample idxs idx-count :with-replacement? nil)
                                  idxs)
                              :binary binary :verbose verbose)))
         (if binary?
             `(if (%= ,test ,idx ',split-point)
                  ,@(mapcar (lambda (side)
                              (tree-train criterion side
                                          :depth (1+ depth)
                                          :max-depth max-depth
                                          :idxs idxs
                                          :idx-count idx-count
                                          :min-size min-size
                                          :binary binary
                                          :verbose verbose))
                            (split-at split-point data idx :test test :key 'lt)))
             ;; for categorical data once we use the dimension (idx)
             ;; we won't return to it
             (let ((idxs (remove idx idxs)))
               `(case (? % ,idx)
                  ,@(sort (mapcar (lambda (cat-vals)
                                    `(,(lt cat-vals)
                                      ,(tree-train criterion (rt cat-vals)
                                                   :depth (1+ depth)
                                                   :max-depth max-depth
                                                   :idxs idxs
                                                   :idx-count idx-count
                                                   :min-size min-size
                                                   :verbose verbose)))
                                  (ht->pairs (partition-by idx data)))
                          ;; putting T case (if present) last
                          ^(not (eql % t)) :key 'first))))))))

(defun split-idx (criterion data idxs &key binary verbose)
  "Determine the dimension of IDXS that fits the DATA best
   according to CRITERION.
   BINARY enforces eql/<= test (for 1-vs-many and numerical values).
   Also return as other values:
   - is there a binary split?
   - split point
   - split test (<= or eql)
   - gain score"
  (let ((best-gain 0)
        best-idx
        best-test
        split-point
        binary?)
    (dolist (idx idxs)
      (let ((vals (mapcar ^(? (lt %) idx)
                          data))
            numeric?)
        (mv-bind (point gain)
            (cond
              ((typep (first vals) '(or float ratio))
               (:= numeric? t)
               (flet ((@idx (item)
                        (? (lt item) idx)))
                 (let ((left ())
                       (right (safe-sort data '< :key #'@idx)))
                   (argmax (lambda (split-point)
                             (appendf left
                                      (loop :for tail :on right
                                            :for item := (first tail)
                                            :while (<= (@idx item) split-point)
                                            :collect item
                                            :finally (:= right tail)))
                             (funcall criterion (list left right)))
                           (let* ((vals (uniq vals :raw t))
                                  (len (ht-count vals))
                                  (vals (sort (ht-keys vals) '<))
                                  (i 0)
                                  (step (/ len 100)))
                             (if (> len 100)
                                 (loop :for val :in vals
                                    :when (> (incf i) step)
                                    :do (:= i 0) :and :collect val)
                                 vals))))))
              (binary
               (argmax ^(call criterion (split-at % data idx :key 'lt))
                       (uniq vals)))
              (t
               (values nil  ; there's no split-point for non-binary splits
                       (call criterion data :idx idx))))
          (when verbose (format *debug-io* "~&idx=~A gain=~A~%" idx (float gain)))
          (when (>= gain best-gain)
            (:= best-idx idx
                best-gain gain
                binary? (or binary numeric?)
                split-point point
                best-test (if numeric? '<= 'eql))))))
    (when verbose (format *debug-io* "~&best-idx=~A gain=~A split-point=~A test=~A~%"
                          best-idx (float best-gain) split-point best-test))
    (values best-idx
            binary?
            split-point
            best-test
            best-gain)))


;;; split criteria

(defun info-gain (samples &key idx (key 'rt))
  "Info gain criterion for SAMPLES either in dimension IDX or by KEY."
  (if idx
      (- (entropy samples :key key)
         (entropy samples :idx idx :key key))
      (if (some 'null samples)
          0
          (- (entropy (reduce 'append samples) :key key)
             (entropy samples :key key :already-split? t)))))

(defun weighted-info-gain (samples split &key (key 'rt))
  "Weighted info gain criterion for SAMPLES either in dimension IDX or by KEY."
  (/ (info-gain samples :key key)
     (split-info split)))

(defun gini-idx (samples)
  "Gini impurity index of SAMPLES."
  (let ((len (length samples)))
    (- 1 (sum ^(expt (/ % len) 2)
              (mapcar 'length (ht-vals (partition-by 'rt samples)))))))

(defun gini-split-idx (samples)
  "Gini split index of SAMPLES."
  (float
   (ds-bind (data1 data2) samples
     (with ((len1 (length data1))
            (len2 (length data2))
            (len (+ len1 len2)))
       (+ (* (/ len1 len) (gini-idx data1))
           (* (/ len2 len) (gini-idx data2)))))))


;;; entropy calculations

(defun entropy (samples &key idx key already-split?)
  "Entropy calculated for SAMPLES based:
   - either on a given dimension IDX
   - or for ALREADY-SPLIT? data
   - or based on KEY selection"
  (float
   (cond
     ((atom samples) (if (member samples '(0 1)) 0
                         (- (+ (* samples (log samples 2))
                                (* (- 1 samples) (log (- 1 samples) 2))))))
     (idx (let ((size (length samples)))
            (sum #`(let ((len (length (rt %))))
                     (* (/ len size)
                        (entropy (/ (count t (rt %) :key key)
                                    len))))
                 (ht->pairs (partition-by idx samples)))))
     (already-split?
      (let ((size (reduce '+ (mapcar 'length samples))))
        (sum ^(* (/ (length %) size)
                 (entropy % :key key))
             samples)))
     (key (entropy (/ (count t samples :key key)
                      (length samples))))
     (t (- (sum ^(* % (log % 2))
                (remove-if 'zerop samples)))))))

(defun split-info (samples &optional idx)
  "Entropy of SAMPLES best split in dimension IDX."
  (let ((size (if idx
                  (length samples)
                  (sum #'length samples))))
    (entropy (mapcar ^(/ (length %) size)
                     (if idx
                         (ht-vals (partition-by idx samples))
                         samples)))))

(defun split-at (point data idx &key (key 'identity) (test 'eql))
  "Split DATA at POINT in dimension IDX."
  (let (left right)
    (dolist (sample data)
      (if (funcall test (? (funcall key sample) idx) point)
          (push sample left)
          (push sample right)))
    (list (reverse left)
          (reverse right))))


;;; utils

(defmacro %= (test idx val)
  "Generates a comparator function TEST to compare a given dimension IDX to VAL."
  `(,test (? % ',idx) ,val))

(defun partition-by (idx-or-fn list)
  "Partition a LIST in 2 either using a test function or a dimension
   given in IDX-OR-FN."
  (let ((fn (if (or (functionp idx-or-fn)
                    (symbolp idx-or-fn))
                idx-or-fn
                ^(? (lt %) idx-or-fn)))
        (rez #h(equal)))
    (dolist (item list)
      (push item (get# (funcall fn item) rez)))
    rez))

(defun sample (data n &key (with-replacement? t))
  "Sample N elements from DATA (by default, WITH-REPLACEMENT?)."
  (if with-replacement?
      (let ((len (length data)))
        (if (>= n len)
            data
            (loop :repeat n :collect (nth (random len) data))))
      (take n (nshuffle (copy-list data)))))

(defun all-permutations (list)
  "Generate all permutations of a LIST."
  (cond ((null list) nil)
        ((null (rest list)) (list list))
        (t (loop :for element :in list
              :append (mapcar #`(cons element %)
                              (all-permutations (remove element list)))))))
