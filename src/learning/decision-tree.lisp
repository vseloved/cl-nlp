;;; (c) 2015 Vsevolod Dyomkin

(in-package #:nlp.learning)
(named-readtables:in-readtable rutils-readtable)

(defclass decision-tree ()
  ((decision-fn :accessor tree-decision-fn :initarg :decision-fn)
   (repr :accessor tree-repr :initarg :repr)
   (classes :accessor tree-classes :initarg :classes)))

(defclass c4.5-tree (decision-tree)
  ())

(defclass cart-tree (decision-tree)
  ())

;;; main methods

(defmethod classify ((model decision-tree) fs)
  (funcall (tree-decision-fn model) fs))

(defmethod train ((model c4.5-tree) data
                  &key classes verbose (min-size 0) idx-count)
  (let ((tree (tree-train 'info-gain data :min-size min-size :idx-count idx-count
                          :verbose verbose)))
    (:= (tree-decision-fn model) (eval `(lambda (%) ,tree))
        (tree-repr model) tree
        (tree-classes model) classes)
    model))

(defmethod train ((model cart-tree) data
                  &key classes verbose (min-size 0) idx-count)
  (let ((tree (tree-train #`(- 1 (gini-split-idx %)) data
                          :binary t :min-size min-size :idx-count idx-count
                          :verbose verbose)))
    (:= (tree-decision-fn model) (eval `(lambda (%) ,tree))
        (tree-repr model) tree
        (tree-classes model) classes)
    model))

(defun tree-train (criterion data
                   &key (min-size 0)
                        (idxs (range 0 (length (lt (first data)))))
                        idx-count
                        binary
                        verbose)
  (cond
    ;; no data
    ((endp data) nil)
    ;; single class
    ((or (single data)
         (member (mapcar 'rt data) '(identity null) :test #`(every %% %)))
     (rt (first data)))
    ;; no indices or min-size reached
    ((or (endp idxs)
         (<= (length data) min-size))
     `(eql % ,(keymax (maptab #`(length %%) (partition-by #'rt data)))))
    ;; general case
    (t (mv-bind (idx split-point test)
           (best-idx criterion data
                     (if idx-count
                         (sample idxs idx-count :with-replacement? nil)
                         idxs)
                     :binary binary :verbose verbose)
         (if (or binary split-point)
             (ds-bind (left right) (split-at split-point data idx
                                             :test test :key 'lt)
               `(if (,test (elt % ,idx) ,split-point)
                    ,(tree-train criterion left :idxs idxs :idx-count idx-count
                                 :min-size min-size :binary binary
                                 :verbose verbose)
                    ,(tree-train criterion right :idxs idxs :idx-count idx-count
                                 :min-size min-size :binary binary
                                 :verbose verbose)))
             (let ((idxs (remove idx idxs)))
               `(case (elt % ,idx)
                  ,@(sort (mapcar (lambda (cat-vals)
                                    `(,(lt cat-vals)
                                       ,(tree-train criterion (rt cat-vals)
                                                    :idxs idxs :idx-count idx-count
                                                    :min-size min-size
                                                    :verbose verbose)))
                                  (ht->pairs (partition-by idx data)))
                          ;; putting T case if present last
                          #`(not (eql % t)) :key 'first))))))))

(defun best-idx (criterion data idxs &key binary verbose)
  (let ((best-gain 0)
        best-idx
        best-test
        split-point)
    (dolist (idx idxs)
      (let ((vals (mapcar #`(elt (lt %) idx) data))
            numeric?)
        (mv-bind (point gain)
            (cond
              ((numberp (first vals))
               (:= numeric? t)
               (argmax #`(funcall criterion (split-at % data idx :test '<= :key 'lt))
                       (sort (uniq vals) '<)))
              (binary
               (argmax #`(funcall criterion (split-at % data idx :key 'lt))
                       (uniq vals)))
              (t (values nil
                         (funcall criterion data :idx idx))))
          (when verbose (format *debug-io* "~&idx=~A gain=~A~%" idx (float gain)))
          (when (>= gain best-gain)
            (:= best-idx idx
                best-gain gain
                split-point point
                best-test (if numeric? '<= 'eql))))))
    (when verbose (format *debug-io* "~&best-idx=~A gain=~A split-point=~A test=~A~%"
                          best-idx (float best-gain) split-point best-test))
    (values best-idx
            split-point
            best-test
            best-gain)))


;;; split criteria

(defun entropy (samples &key idx key already-split?)
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
       (sum #`(* (/ (length %) size)
                 (entropy % :key key))
            samples)))
    (key (entropy (/ (count t samples :key key)
                     (length samples))))
    (t (- (sum #`(* % (log % 2))
               (remove-if 'zerop samples))))))

(defun split-info (samples &optional idx)
  (let ((size (if idx
                  (length samples)
                  (sum #'length samples))))
    (entropy (mapcar #`(/ (length %) size)
                     (if idx
                         (ht-vals (partition-by idx samples))
                         samples)))))

(defun split-at (point data idx &key (key 'identity) (test 'eql))
  (let (left right)
    (dolist (sample data)
      (if (funcall test (elt (funcall key sample) idx) point)
          (push sample left)
          (push sample right)))
    (list (reverse left)
          (reverse right))))

(defun info-gain (samples &key idx (key 'rt))
  (if idx
      (- (entropy samples :key key)
         (entropy samples :idx idx :key key))
      (if (some 'null samples)
          0
          (- (entropy (reduce 'append samples) :key key)
             (entropy samples :key key :already-split? t)))))

(defun weighted-info-gain (info-gain split &key (key 'rt))
  (/ info-gain (split-info split)))

(defun gini-idx (data)
  (let ((len (length data)))
    (- 1 (sum #`(expt (/ % len) 2)
              (mapcar 'length (ht-vals (partition-by #'rt data)))))))

(defun gini-split-idx (samples)
  (ds-bind (data1 data2) samples
    (let* ((len1 (length data1))
           (len2 (length data2))
           (len (+ len1 len2)))
      (+ (* (/ len1 len) (gini-idx data1))
         (* (/ len2 len) (gini-idx data2))))))


;;; utils

(declaim (inline sum))
(defun sum (fn seq &key key)
  (reduce '+ (mapcar (if key #`(funcall fn (funcall key %)) fn)
                     seq)))

(defun partition-by (idx-or-fn list)
  (let ((fn (if (functionp idx-or-fn)
                idx-or-fn
                #`(elt (lt %) idx-or-fn)))
        (rez #h(equal)))
    (dolist (item list)
      (push item (get# (funcall fn item) rez)))
    rez))

(defun sample (data n &key (with-replacement? t))
  (if with-replacement?
      (let ((len (length data)))
        (if (>= n len)
            data
            (loop :repeat n :collect (nth (random len) data))))
      (take n (nshuffle (copy-list data)))))

(defun all-permutations (list)
  (cond ((null list) nil)
        ((null (rest list)) (list list))
        (t (loop :for element :in list
              :append (mapcar #`(cons element %)
                              (all-permutations (remove element list)))))))
