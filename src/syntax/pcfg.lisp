;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.syntax)
(named-readtables:in-readtable rutils-readtable)


(defclass pcfg (cfg)
  ((root-rules :initarg :root-rules :reader gr-root-rules
    :documentation
    "A special set of dyadic rules of the form ((ROOT NT) . Q)
     which specify the probability of a sentence being one of NTS
     (like S or SBAR). It may be used, if we want to have fake root node.")
   (irules :reader gr-irules
    :documentation
    "Special slot for optimization purposes, which holds rules with
     numbers instead of non-terminals. Non-terminals are indexed in NTS-IDX.")
   (nts-idx :reader gr-nts-idx
    :documentation
    "Special slot for optimization purposes, which holds a bidirectional
     mapping between NTS and their indices.
     Its car maps from NTS to numbers, and cdr in the opposite direction."))
  (:documentation
   "Probabilistic context-free grammar adds QS rule probability paramters
    to an ordinary CFG."))

(defmethod slot-unbound (class (obj pcfg) (slot (eql 'nts-idx)))
  (let ((nts-idx (cons (make-hash-table) (make-hash-table))))
    (doindex (i nt (gr-nts obj))
      (set# nt (car nts-idx) i)
      (set# i  (cdr nts-idx) nt))
    (setf (slot-value obj 'nts-idx) nts-idx)))

(defmethod slot-unbound (class (obj pcfg) (slot (eql 'irules)))
  (let ((nts-idx (car (gr-nts-idx obj)))
        (irules (make-hash-table :test 'equal)))
    (dotable (rule q (gr-rules obj))
      (set# (mapcar #`(if (symbolp %) (get# % nts-idx) %)
                    rule)
            irules q))
    (setf (slot-value obj 'irules) irules)))


;;; CKY parsing

(defvar *n* nil
  "Length of the currently parsed sentence.")
(defvar *nt-count* nil
  "Number of non-terminals in the current grammar.")

;; (declaim (inline @))
(defun @ (m i j k)
  "Access element of M for indices I, J and non-terminal K."
  (get# (+ (* i *n* *nt-count*)
           (* j *nt-count*)
           k)
        m))

(defsetf @ (m i j k) (v)
  `(set# (+ (* ,i *n* *nt-count*)
            (* ,j *nt-count*)
            ,k)
         ,m ,v))

;; (defun @ (m i j k)
;;   "Access element of M for indices I, J and non-terminal K."
;;   (get# (list i j k) m))

;; (defsetf @ (m i j k) (v)
;;   `(set# (list ,i ,j ,k) ,m ,v))

(macrolet
    ((CKY (&body body)
       `(with-accessors ((rules gr-irules) (nts-idx gr-nts-idx)) grammar
          (let* ((nts->idx (car nts-idx))
                 (pi0 (make-hash-table :test 'equal))
                 (bps (make-hash-table :test 'equal))
                 (min most-negative-single-float))
            ;; init pi and backpointes matrix
            (doindex (i w sentence)
              (let ((i (1+ i)))
                (dotable (nt k nts->idx)
                  (when-it (get# (list k w) rules)
                    (setf (@ pi0 i i k) (log it)
                          (@ bps i i k) (cons k i))))))
            ;; (print-ht pi0)
            (do ((pos 1 (1+ pos)))
                ((>= pos *n*))
              (do ((i 1 (1+ i)))
                  ((> i (- *n* pos)))
                (let ((j (+ i pos)))
                  (dotable (_ k nts->idx)
                    (let (max arg)
                      (do ((s i (1+ s)))
                          ((>= s j))
                        (dotable (rule q rules)
                          (when (and (tryadic rule)
                                     (= k (first rule)))
                            ,@body)))
                      (when (if (listp max) max (> max min))
                        (setf (@ pi0 i j k) max
                              (@ bps i j k) arg)))))))
            ;; (terpri)
            ;; (print-ht pi0)
            ;; (print-ht bps)
            ;; (maphash #`(print (cons %
            ;;                         (mapcar #`(get# % (cdr nts-idx))
            ;;                                 (mklist (car %%)))))
            ;;          bps)
            (values pi0
                    bps)))))

(defmethod parse ((grammar pcfg) (sentence list))
  "Return a parse tree of SENTENCE for PCFG.

   Parsing is performed in 2 steps:

   - the main method returns as values the matrices PI0 (pi matrix)
     and BPS (backpointers matrix)
   - the :around method restores the parse tree from these matrices

   This brings additional flexibility for the return values of the method:
   for instance, we can try to return several trees or have several roots.
   The default method returns a single tree for the ROOT slot of PCFG."
  (CKY (let* ((cur (cons rule s))
              (l (@ pi0 i s (second rule)))
              (r (@ pi0 (1+ s) j (third rule)))
              (score (if (and l r)
                         (+ (log q) l r)
                         min)))
         (when (> score (or max min))
           (setf max score
                 arg cur)))))

(defmethod parse-n ((grammar pcfg) (sentence list) n)
  "Return a list of N best parse tree of SENTENCE for PCFG.

   Parsing is performed in 2 steps:

   - the main method returns as values the matrices PI0 (pi matrix)
     and BPS (backpointers matrix)
   - the :around method restores the parse tree from these matrices

   This brings additional flexibility for the return values of the method:
   for instance, we can try to return several trees or have several roots.
   The default method returns a single tree for the ROOT slot of PCFG."
  (CKY (let* ((cur (cons rule s))
              (l (atomize (@ pi0 i s (second rule))))
              (r (atomize (@ pi0 (1+ s) j (third rule))))
              (score (when (and l r)
                       (+ (log q) l r)))
              prev-s prev-r)
         (when score
           (loop :for rest-s :on max
                 :for rest-r :on arg :do
              (if (> (car rest-s) score)
                  (setf prev-s (cons (car rest-s) prev-s)
                        prev-r (cons (car rest-r) prev-r))
                  (return
                    (setf max (append (reverse prev-s)
                                      (cons score rest-s))
                          arg (append (reverse prev-r)
                                      (cons cur rest-r)))))
              :finally
              (when (< (length max) n)
                (setf max (append max (list score))
                      arg (append arg (list cur)))))
           (setf max (take n max)
                 arg (take n arg))))))

) ; end of macrolet


(macrolet ((with-raw-results (&body body)
             `(with-accessors ((nts-idx gr-nts-idx) (rules gr-irules)
                               (root gr-root) (root-rules gr-root-rules))
                  grammar
                (flet ((nt->idx (nt)
                         (get# nt (car nts-idx)))
                       (idx->nts (tree)
                         (maptree #`(if (numberp %) (get# % (cdr nts-idx)) %)
                                  tree)))
                  (let* ((*n* (length sentence))
                         (nts->idx (car nts-idx))
                         (*nt-count* (ht-count nts->idx))
                         (min most-negative-single-float)
                         (iroot (nt->idx root)))
                    (mv-bind (pi0 bps) (call-next-method)
                      ,@body))))))

(defmethod parse :around ((grammar pcfg) (sentence list))
  (with-raw-results
    ;; for the case of fake root determine the best real root
    (when root-rules
      (ds-bind (rule . prob)
          (reduce #`(if (> (cdr %) (cdr %%)) % %%)
                  (mapcar #`(cons (car %)
                                  (if-it (@ pi0 1 *n* (nt->idx (cadar %)))
                                         (* it (cdr %))
                                         min))
                          root-rules))
        (setf (@ pi0 1 *n* iroot) prob
              (@ bps 1 *n* iroot) (cons (mapcar #'nt->idx rule)
                                        *n*))))
    (values (idx->nts (decode-parse-tree sentence bps 1 *n* iroot))
            (exp (or (@ pi0 1 *n* iroot) min))
            pi0
            bps)))

(defmethod parse-n :around ((grammar pcfg) (sentence list) n)
  (with-raw-results
    ;; for the case of fake root determine the best real root
    (when root-rules
      (loop :for (rule . prob) :in root-rules
         :collect (cons (mapcar #'nt->idx rule) *n*) :into rules
         :collect prob :into probs
         :finally (setf (@ pi0 1 *n* iroot) probs
                        (@ bps 1 *n* iroot) rules)))
    (let ((best (n-best-parse-trees n sentence bps pi0 iroot)))
      (values (mapcar #`(idx->nts (car %)) best)
              (mapcar #`(exp (cdr %)) best)
              pi0
              bps))))

) ; end of macrolet


;;; Tree deccoding

(defun decode-parse-tree (sentence bps i j iroot)
  "Restore parse tree of the SENTENCE
   with non-treminal IROOT from backpointers BPS with bounds I and J."
  (ds-bind (rule . s) (@ bps i j iroot)
    (if (= i j)
        (list rule (nth (1- i) sentence))
        (list* (first rule)
               (decode-parse-tree sentence bps i s (second rule))
               (when-it (third rule)
                 (list (decode-parse-tree sentence bps (1+ s) j it)))))))

(defun n-best-parse-trees (n sentence bps pi0 iroot)
  "Return N best parse trees with their scores,
   starting from IROOT for SENTENCE with the Viterbi matrices PI0 and BPS."
  (let ((visited (make-hash-table))
        rez)
    (loop :while (plusp n) :do (decf n) :collect
       (funcall (decode-best-parse-tree sentence bps pi0 visited
                                        1 *n* iroot)))))

(defun decode-best-parse-tree (sentence bps pi0 visited i j iroot)
  "Return the best parse tree with its score for Viterbi matrices BPS and PI0
   holding not single maximums but lists of best n variants,
   with bounding indices I and J and root node index IROOT,
   considering the matrix of already visited variants VISITED
   (which were used in decoding the better parse trees)."
  (labels ((decode-alt-tree (i j iroot alt-idx)
             (ds-bind (rule . s) (nth alt-idx (@ bps i j iroot))
               (if (= i j)
                   (list rule (nth i sentence))
                   (list* (first rule)
                          (decode-alt-tree i s (second rule) 0)
                          (when-it (third rule)
                            (list (decode-alt-tree (1+ s) j it 0))))))))
    (let* ((unvisited (or (@ visited i j iroot) 0))
           (cur-bps (@ bps i j iroot))
           (cur-pi0 (@ pi0 i j iroot))
           (min most-negative-single-float)
           (best (if (< unvisited (length cur-bps))
                     (list (nth unvisited cur-pi0)
                           #`(decode-alt-tree i j iroot unvisited)
                           (list i j iroot))
                     (list min))))
      (loop :for bp :in (take unvisited cur-bps)
            :for score :in (take unvisited cur-pi0) :do
         (ds-bind ((h l &optional r) . s) bp
           ;; look at alternative paths at current node
           (dolist (idx (cons (list i s l)
                              (when r (list (list (1+ s) j r)))))
             (let* ((cur-scores (apply #'@ pi0 idx))
                    (next (1+ (apply #'@ visited idx)))
                    (alt-score (when (< next (length cur-scores))
                                 (+ score (- (nth next cur-scores)
                                             (car cur-scores))))))
               (when (and alt-score (> alt-score (car best)))
                 (setf best
                       (list alt-score
                             #`(if (= i (car idx))
                                   ;; we explore left alternatives
                                   (list iroot
                                         (decode-alt-tree i s l next)
                                         (when r (decode-alt-tree (1+ s) j r 0)))
                                   ;; we explore right alternatives
                                   (list iroot
                                         (decode-alt-tree i s l 0)
                                         (decode-alt-tree (1+ s) j r next)))
                             idx))))))
           ;; look below already visited nodes
           (dolist (idx (cons (list i s h)
                              (when (< s *n*)
                                (list (list (1+ s) j h)))))
             (ds-bind (tree-decoder . alt-score)
                 (apply #'decode-best-parse-tree sentence bps pi0 visited idx)
               (when (> alt-score (car best))
                 (setf best (list alt-score
                                  #`(list iroot (funcall tree-decoder))
                                  idx))))))
      (setf (apply #'@ visited best) (1+ (or (apply #'@ visited best) 0)))
      (cons (second best) (first best)))))
