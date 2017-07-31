;;; (c) 2015-2017 Vsevolod Dyomkin

(in-package #:nlp.learning)
(named-readtables:in-readtable rutilsx-readtable)


(defclass decision-tree ()
  ((decision-fn :accessor tree-decision-fn :initarg :decision-fn)
   (decision-fn-dbg :accessor tree-decision-fn-dbg :initarg :decision-fn-dbg)
   (repr :accessor tree-repr :initarg :repr)
   (classes :accessor tree-classes :initarg :classes)
   (max-depth :accessor tree-max-depth :initform nil :initarg :max-depth)
   (min-size :accessor tree-min-size :initform 0 :initarg :min-size)))

(defclass c4.5-tree (decision-tree)
  ())

(defclass cart-tree (decision-tree)
  ())  


;;; main methods

(defparameter *dtree-max-depth* 10)

(defmethod rank ((model decision-tree) fs &key classes)
  (with ((rez (call (if *dtree-debug*
                        (tree-decision-fn-dbg model)
                        (tree-decision-fn model))
                    fs))
         (has-alts (listp (second rez))))
    (unless has-alts
      (:= rez (list rez)))
    (when classes
      (:= rez (keep-if ^(member (first %) classes)
                       rez)))
    (pairs->ht rez)))

(defmethod train ((model c4.5-tree) data
                  &key idx-count (idxs (range 0 (length @data#0.fs)))
                       classes verbose fast)
  (let ((tree (tree-train 'info-gain data
                          :binary nil
                          :min-size (tree-min-size model)
                          :max-depth (tree-max-depth model)
                          :idxs idxs :idx-count idx-count
                          :verbose verbose
                          :fast fast)))
    (:= @model.repr tree
        @model.classes classes)
    (:= @model.decision-fn (eval `(lambda (%) ,tree))
        @model.decision-fn-dbg (let ((*dtree-debug* t))
                                 (eval `(lambda (%) ,tree))))
    model))

(defmethod train ((model cart-tree) data
                  &key idx-count (idxs (range 0 (length @data#0.fs)))
                       classes verbose fast)
  (with ((tree (tree-train ^(- 1 (gini-split-idx %)) data
                           :binary t
                           :min-size (tree-min-size model)
                           :max-depth (tree-max-depth model)
                           :idxs idxs :idx-count idx-count
                           :verbose verbose
                           :fast fast))
         (depth (cart-depth tree)))
    (:= @model.repr tree
        @model.classes classes)
    (:= @model.decision-fn (eval (if (> depth *dtree-max-depth*)
                                     (cart-fn tree)
                                     `(lambda (%) ,tree)))
        @model.decision-fn-dbg (if (> depth *dtree-max-depth*)
                                   (cart-fn tree :debug t)
                                   (let ((*dtree-debug* t))
                                     (eval `(lambda (%) ,tree)))))
    model))

(defmethod save-model ((model cart-tree) path)
  (gzip-stream:with-open-gzip-file (out path :direction :output
                                             :if-does-not-exist :create
                                             :if-exists :supersede)
    (call-next-method model out)))

(defmacro node-repr (node &rest clauses)
  `(cond ((eql t ,node) "__T__")
         ((eql nil ,node) "__F__")
         ,@clauses
         (t (fmt "~S" ,node))))

(defmethod save-model ((model cart-tree) (out stream))
  (with (((fs ops vals) (cart->vecs @model.repr)))
    (format out "~{~A~^ ~}~%" (map 'list ^(or % -1)
                                   fs))
    (format out "~{~A~^ ~}~%" (map 'list ^(case %
                                            (eql 0)
                                            (<= 1)
                                            (t 2))
                                   ops))
    (format out "~{~A~^~%~}~%"
            (map 'list ^(node-repr %
                          ((listp %) (fmt "~A ~A"
                                          (node-repr (? % 0))
                                          (? % 1))))
                 vals))))

(defmethod load-model ((model cart-tree) path &key)
  (gzip-stream:with-open-gzip-file (in path)
    (call-next-method model in)))

(defmethod load-model ((model cart-tree) (in stream) &key)
  (with ((fs (map 'vector 'parse-integer
                  (split #\Space (read-line in))))
         (ops (map 'vector ^(case %
                              (0 'eql)
                              (1 '<=))
                   (mapcar 'parse-integer (split #\Space (read-line in)))))
         (vals (make-array (length fs)))
         (*read-eval* nil))
    (dotimes (i (length fs))
      (let ((val (read-line in)))
        (:= (? vals i)
            (if (? ops i)
                (switch (val :test 'equal)
                  ("__T__" t)
                  ("__F__" nil)
                  (otherwise
                   (if (member (char val 0)
                               '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\-))
                       (read-from-string val)
                       val)))
                (unless (string= "0" val)
                  (with (((k v) (split #\Space val)))
                    (pair (cond ((string= k "__F__") nil)
                                ((string= k "__T__") t)
                                (t (read-from-string k)))
                          (read-from-string v))))))))
    (let ((tree (vecs->cart fs ops vals)))
      (:= @model.repr tree
          @model.decision-fn (%cart-fn fs ops vals)
          @model.decision-fn-dbg (%cart-fn fs ops vals :debug t)))
    model))
                   

;;; debug

(defparameter *dtree-debug* nil)

(defun print-dtree-debug-info (idx cur op val rez)
  "Format current decision of the tree indormation:
     IDX - current feature, CUR - its value, OP - test operation,
     VAL - test value, REZ - test result."
  (format *debug-io* "~{~C~}f(~A)=~A ~A ~A => ~A~%"
          (loop :repeat (if (numberp *dtree-debug*)
                            (:+ *dtree-debug*)
                            (:= *dtree-debug* 0))
                :collect #\Tab)
          idx cur op val rez))


;;; cart transforms

(defun cart-depth (cart)
  "Depth of a decision TREE."
  (cond ((atom cart) 0)
        ((atom (second cart)) 1)
        (t (1+ (reduce 'max (mapcar 'cart-depth (cddr cart)))))))

(defun cart-fn (cart &key debug)
  "Generate a decision function for a CART decision tree."
  (with (((fs ops vals) (cart->vecs cart)))
    (%cart-fn fs ops vals :debug debug)))

(defun %cart-fn (fs ops vals &key debug)
  "Generate a decision function for a CART decision tree."
  (lambda (%)
    (let ((i 0))
      (loop (if-it (? ops i)
                   (with ((val (? vals i))
                          (idx (? fs i))
                          (cur (? % idx))
                          (rez (call it cur val)))
                     (when debug (print-dtree-debug-info idx cur it val rez))
                     (:= i (+ (* i 2)
                              (if rez 1 2))))
                   (progn
                     (when debug (:= *dtree-debug* t))
                     (return (? vals i))))))))

(defun cart->vecs (cart)
  "Produce 3 arrays (features, operations, values) from a CART tree."
  (with ((depth (cart-depth cart))
         (q (make-queue))
         (fs (make-array (1- (expt 2 depth)) :initial-element nil))
         (ops (make-array (1- (expt 2 depth)) :initial-element nil))
         (vals (make-array (1- (expt 2 depth)) :initial-element 0)))
    (push-queue (pair cart 0) q)
    (loop :for (tree i) := (pop-queue q) :while tree :do
      (when (listp tree)
        (case (first tree)
          (if
           (with (((_ op f val) (second tree)))
             (:= (? ops i) op
                 (? fs i) f
                 (? vals i) val))
           (push-queue (pair (third tree) (+ (* 2 i) 1)) q)
           (push-queue (pair (fourth tree) (+ (* 2 i) 2)) q))
          (pair
           (:= (? vals i) (rest tree))))))
    (list fs
          ops
          vals)))

(defun vecs->cart (fs ops vals &optional (i 0))
  "Produce a CART tree from 3 arrays (features, operations, values)."
  (let ((f (? fs i))
        (o (? ops i))
        (v (? vals i)))
    (if o
        `(if (%= ,o ,f ,v)
             ,(vecs->cart fs ops vals (+ (* i 2) 1))
             ,(vecs->cart fs ops vals (+ (* i 2) 2)))
        `(pair ,@v))))


;;; training

(defun normed-dist (data)
  (with ((dist (maptab ^(length %%)
                       (partition-by 'ex-gold data)))
         (total (reduce '+ (ht-vals dist)))
         (key max (keymax dist)))
    `(pair ,key ,(float (/ max total)))))

(defun tree-train (criterion data
                   &key (min-size 0) (depth 1) max-depth
                        (idxs (range 0 (length @data#0.fs)))
                        idx-count  ; for RF classifier to sample dimensions
                        binary
                        (fast t)
                        verbose)
  "Train a decision tree from DATA using a CRITERION function."
  (format *debug-io* ".")
  (cond
    ;; no data
    ((endp data) nil)
    ;; single class
    ((single (uniq (mapcar 'ex-gold data)))
     `(pair ,(? data 0 'gold) 1.0))
    ;; no indices, min-size or max-depth reached
    ((or (endp idxs)
         (when max-depth (>= depth max-depth))
         (<= (length data) min-size))
     (normed-dist data))
    ;; general case
    (t (with ((idx binary? split-point test
                   (if fast
                       (fast-bin-split-idx criterion data
                                           (if idx-count
                                               (sample idxs idx-count
                                                       :with-replacement? nil)
                                               idxs)
                                           :verbose verbose)
                       (split-idx criterion data
                                  (if idx-count
                                      (sample idxs idx-count
                                              :with-replacement? nil)
                                      idxs)
                                  :binary binary :verbose verbose))))
         (cond ((null idx)
                (normed-dist data))
               (binary?
                `(if (%= ,test ,idx ,split-point)
                     ,@(mapcar (lambda (side)
                                 (tree-train criterion side
                                             :depth (1+ depth)
                                             :max-depth max-depth
                                             :idxs idxs
                                             :idx-count idx-count
                                             :min-size min-size
                                             :fast fast
                                             :binary binary
                                             :verbose verbose))
                               (split-at split-point data idx :test test))))
               ;; for categorical data once we use the dimension (idx)
               ;; we won't return to it
               (t
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
                                                      :fast fast
                                                      :verbose verbose)))
                                     (ht->pairs (partition-by idx data)))
                        ;; putting T case (if present) last
                        ^(not (eql % t)) :key 'first)))))))))

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
        gains
        split-point
        binary?)
    (dolist (idx idxs)
      (flet ((@idx (ex) (? @ex.fs idx)))
        (with ((vals (mapcar #'@idx data))
               (numeric? nil)
               (point gain
                      (cond
                        ((typep (first vals) '(or float ratio))
                         (:= numeric? t)
                         (let ((left ())
                               (right (safe-sort data '< :key #'@idx)))
                           (argmax (lambda (split-point)
                                     (appendf left
                                              (loop :for tail :on right
                                                    :for item := (? tail 0)
                                                    :while (<= (@idx item)
                                                               split-point)
                                                    :collect item
                                                    :finally (:= right tail)))
                                     (call criterion (list left right)))
                                   (with ((vals (uniq vals :raw t))
                                          (len (ht-count vals))
                                          (vals (sort (keys vals) '<))
                                          (i 0)
                                          (step (/ len 100)))
                                     (if (> len 100)
                                         (loop :for val :in vals
                                               :when (> (:+ i) step)
                                                 :do (:= i 0)
                                                 :and :collect val)
                                         vals)))))
                        (binary
                         (argmax ^(call criterion (split-at % data idx))
                                 (uniq vals)))
                        (t
                         (values nil ; there's no split-point for non-binary splits
                                 (call criterion data :idx idx ))))))
          (when verbose (format *debug-io* "~&idx=~A gain=~A~%" idx (float gain)))
          (push gain gains)
          (when (> gain best-gain)
            (:= best-idx idx
                best-gain gain
                binary? (or binary numeric?)
                split-point point
                best-test (if numeric? '<= 'eql))))))
    (unless (> (ht-count (uniq gains :raw t)) 1)
      (void best-idx))
    (when verbose
      (with (((left right) (split-at split-point data best-idx :test best-test)))
        (format *debug-io* "~&best-idx=~A gain=~A split-point=~A test=~A split=~$/~$~%"
                best-idx (float best-gain) split-point best-test
                (float (/ (length left) (length data)))
                (float (/ (length right) (length data))))))
    (values best-idx
            binary?
            split-point
            best-test
            best-gain)))

(defun fast-bin-split-idx (criterion data idxs &key verbose)
  "Determine the dimension of IDXS that fits the DATA best
   according to CRITERION.
   Also return as other values:
   - is there a binary split?
   - split point
   - split test (<= or eql)
   - gain score"
  (let ((best-gain 0)
        best-idx
        best-test
        gains
        split-point)
    (dolist (idx idxs)
      (flet ((@idx (ex) (? @ex.fs idx)))
        (with ((numeric? (typep (@idx (first data)) '(or float ratio)))
               (point gain (if numeric?
                               (with ((sorted (safe-sort (coerce data 'vector)
                                                         '< :key #'@idx))
                                      (beg 0)                                 
                                      (end (length sorted))
                                      (m (floor (- end beg) 2))
                                      (mg (call criterion
                                                (list (slice sorted 0 m)
                                                      (slice sorted m)))))
                                 (dotimes (i (floor (log (length sorted) 2)))
                                   (with ((l (floor (- end beg) 4))
                                          (r (+ l m))
                                          ((lg rg) (mapcar ^(call criterion %)
                                                           `((,(slice sorted 0 l)
                                                              ,(slice sorted l))
                                                             (,(slice sorted 0 r)
                                                              ,(slice sorted r))))))
                                     (cond ((< mg (min lg rg))
                                            (return))
                                           ((< lg rg)
                                            (:= end m))
                                           (t
                                            (:= beg m)))))
                                 (values (? sorted m 'fs idx)
                                         mg))
                               (argmax ^(call criterion (split-at % data idx))
                                       (uniq data :test 'eql)))))
          (when verbose
            (format *debug-io* "~&idx=~A gain=~A~%" idx (float gain)))
          (push gain gains)
          (when (> gain best-gain)
            (:= best-idx idx
                best-gain gain
                split-point point
                best-test (if numeric? '<= 'eql))))))
    (with (((&optional left right) (when best-idx
                                     (split-at split-point data best-idx
                                               :test best-test))))
      (unless (and left right)
        (void best-idx))
      (when verbose
        (format *debug-io* "~&best-idx=~A gain=~A split-point=~A test=~A split=~$/~$~%"
                best-idx (float best-gain) split-point best-test
                (when best-idx (float (/ (length left) (length data))))
                (when best-idx (float (/ (length right) (length data)))))))
    (values best-idx
            t
            split-point
            best-test
            best-gain)))

(defun split-at (point data idx &key (key 'ex-fs) (test 'eql))
  "Split DATA at POINT in dimension IDX."
  (let (left right)
    (dolist (ex data)
      (if (call test (? (call key ex) idx) point)
          (push ex left)
          (push ex right)))
    (list (reverse left)
          (reverse right))))


;;; split criteria

(defun info-gain (exs &key idx (key 'ex-gold))
  "Info gain criterion for examples EXS either in dimension IDX or by KEY."
  (if idx
      (- (entropy exs :key key)
         (entropy exs :idx idx :key key))
      (if (some 'null exs)
          0
          (- (entropy (reduce 'append exs) :key key)
             (entropy exs :key key :already-split? t)))))

(defun weighted-info-gain (exs split &key (key 'ex-gold))
  "Weighted info gain criterion for examples EXS either
   in dimension IDX or by KEY."
  (/ (info-gain exs :key key)
     (split-info split)))

(defun gini-idx (exs)
  "Gini impurity index of examples EXS."
  (let ((len (length exs)))
    (- 1 (sum ^(expt (/ % len) 2)
              (mapcar 'length (vals (partition-by 'ex-gold exs)))))))

(defun gini-split-idx (exs)
  "Gini split index of examples EXS."
  (float
   (with (((data1 data2) exs)
          (len1 (length data1))
          (len2 (length data2))
          (len (+ len1 len2)))
     (+ (* (/ len1 len) (gini-idx data1))
        (* (/ len2 len) (gini-idx data2))))))


;;; entropy calculations

(defun entropy (exs &key idx (key 'ex-gold) already-split?)
  "Entropy calculated for examples EXS based:
   - either on a given dimension IDX
   - or for ALREADY-SPLIT? data
   - or based on KEY selection"
  (float
   (cond
     ((atom exs) (if (member exs '(0 1)) 0
                     (- (+ (* exs (log exs 2))
                           (* (- 1 exs) (log (- 1 exs) 2))))))
     (idx (let ((size (length exs)))
            (sum ^(let ((len (length (rt %))))
                     (* (/ len size)
                        (entropy (/ (count t (rt %) :key key)
                                    len))))
                 (pairs (partition-by idx exs)))))
     (already-split?
      (let ((size (sum 'length exs)))
        (sum ^(* (/ (length %) size)
                 (entropy % :key key))
             exs)))
     (key (entropy (/ (count t exs :key key)
                      (length exs))))
     (t (- (sum ^(* % (log % 2))
                (remove-if 'zerop exs)))))))

(defun split-info (exs &optional idx)
  "Entropy of examples EXS best split in dimension IDX."
  (let ((size (if idx
                  (length exs)
                  (sum 'length exs))))
    (entropy (mapcar ^(/ (length %) size)
                     (if idx
                         (vals (partition-by idx exs))
                         exs))
             :key nil)))


;;; utils

(defmacro %= (test idx val)
  "Generates a comparator function TEST to compare a given dimension IDX to VAL."
  (if *dtree-debug*
      (with-gensyms (cur rez)
        `(with ((,cur (? % ',idx))
                (,rez (,test ,cur ,val)))
           (print-dtree-debug-info ',idx ,cur ',test ,val ,rez)
           ,rez))
      `(,test (? % ',idx) ,val)))

(defun partition-by (idx-or-fn seq)
  "Partition a SEQ in 2 either using a test function or a dimension
   given in IDX-OR-FN."
  (let ((fn (if (or (functionp idx-or-fn)
                    (symbolp idx-or-fn))
                idx-or-fn
                ^(? @%.fs idx-or-fn)))
        (rez #h(equal)))
    (etypecase seq
      (list (dolist (item seq)
              (push item (? rez (call fn item)))))
      (vector (dovec (item seq)
                (push item (? rez (call fn item))))))
    rez))

(defun all-permutations (list)
  "Generate all permutations of a LIST."
  (cond ((null list) nil)
        ((null (rest list)) (list list))
        (t (loop :for element :in list
              :append (mapcar ^(cons element %)
                              (all-permutations (remove element list)))))))


;;; simple queue utils

(defstruct queue
  head tail)

(defun push-queue (item q)
  (push item @q.head))

(defun pop-queue (q)
  (let (non-empty)
    (if @q.tail
        (:= non-empty t)
        (loop :for item := (pop @q.head) :while item :do
          (:= non-empty t)
          (push item @q.tail)))
    (values (pop @q.tail)
            non-empty)))
