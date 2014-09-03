;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.parsing)
(named-readtables:in-readtable rutils-readtable)


(defclass cky-parser ()
  ()
  (:documentation
   "Parser which uses the CKY algorithm."))


;;; Managing parsing context

(defvar *pi* nil
  "PI matrix.")
(defvar *bp* nil
  "Backpointers matrix.")
(defvar *n* nil
  "Length of the currently parsed sentence.")
(defvar *k* nil
  "Number of non-terminals in the current grammar.")

(declaim (inline @))
(defun @ (m i j k &optional default)
  "Access element of M for indices I, J and non-terminal K."
  (get# (+ (* i *n* *k*)
           (* j *k*)
           k)
        m
        default))

(defsetf @ (m i j k) (v)
  `(set# (+ (* ,i *n* *k*)
            (* ,j *k*)
            ,k)
         ,m ,v))

(macrolet ((parse-with-ctx ()
             `(with-accessors ((nts-idx grammar-nts-idx) (root grammar-root)
                               (urules grammar-iurules) (brules grammar-ibrules)
                               (root-rules grammar-root-rules))
                  grammar
                (flet ((nt->idx (nt)
                         (get# nt (car nts-idx)))
                       (idx->nts (tree)
                         (maptree #`(if (numberp %) (get# % (cdr nts-idx)) %)
                                  tree)))
                  (let* ((*pi* #{equal})
                         (*bp* #{equal})
                         (*n* (length sentence))
                         (nts->idx (car nts-idx))
                         (*k* (ht-count nts->idx))
                         (iroot (nt->idx root))
                         (min most-negative-single-float))
                    ;; init Pi & Backpointers matrices
                    (doindex (i w sentence)
                      (let ((i (1+ i)))
                        (dotable (nt k nts->idx)
                          (when-it (get# (list k w) urules)
                            (:= (@ *pi* i i k) (log it)
                                (@ *bp* i i k) (cons k i))))))
                    (call-next-method))))))

(defmethod parse :around ((parser cky-parser) (grammar pcfg) (sentence list))
  (parse-with-ctx))

(defmethod parse-n :around ((parser cky-parser) (grammar pcfg) (sentence list) n)
  (parse-with-ctx))

) ; end of macrolet


;;; The core algorithm

(macrolet
    ((cky (&key acc-clause result-clause)
       `(do ((pos 1 (1+ pos)))
            ((>= pos *n*) ,result-clause)
          (do ((i 1 (1+ i)))
              ((> i (- *n* pos)))
            (let ((j (+ i pos)))
              (dotable (_ k nts->idx)
                (let (max arg)
                  (do ((s i (1+ s)))
                      ((>= s j))
                    (dotable (rule q brules)
                      (when (= k (first rule))
                        ,acc-clause)))
                  (when (if (listp max) max (> max min))
                    (:= (@ *pi* i j k) max
                        (@ *bp* i j k) arg)))))))))

(defmethod parse ((parser cky-parser) (grammar pcfg) (sentence list))
  "Return a parse tree of SENTENCE for PCFG.

   Parsing is performed in 2 steps:

   - the main method returns as values the matrices Pi and Backpointers matrices
   - the :around method restores the parse tree from these matrices

   This brings additional flexibility for the return values of the method:
   for instance, we can try to return several trees or have several roots.
   The default method returns a single tree for the ROOT slot of PCFG."
  (cky :acc-clause
       (let* ((cur (cons rule s))
              (l (@ *pi* i s (second rule)))
              (r (@ *pi* (1+ s) j (third rule)))
              (score (if (and l r)
                         (+ (log q) l r)
                         min)))
         (when (> score (or max min))
           (setf max score
                 arg cur)))
       :result-clause
       (progn
         ;; for the case of fake root determine the best real root
         (when root-rules
           (ds-bind (rule . prob)
               (reduce #`(if (> (cdr %) (cdr %%)) % %%)
                       (mapcar #`(cons (car %)
                                       (if-it (@ *pi* 1 *n* (nt->idx (cadar %)))
                                              (* it (cdr %))
                                              min))
                               root-rules))
             (setf (@ *pi* 1 *n* iroot) prob
                   (@ *bp* 1 *n* iroot) (cons (mapcar #'nt->idx rule)
                                              *n*))))
         (values (idx->nts (decode-parse-tree sentence 1 *n* iroot))
                 (exp (or (@ *pi* 1 *n* iroot) min))
                 *pi*
                 *bp*))))

(defmethod parse-n ((parser cky-parser) (grammar pcfg) (sentence list) n)
  "Return a list of N best parse tree of SENTENCE for PCFG.

   Parsing is performed in 2 steps:

   - the main method returns as values the Pi and Backpointes matrices
   - the :around method restores the parse tree from these matrices

   This brings additional flexibility for the return values of the method:
   for instance, we can try to return several trees or have several roots.
   The default method returns a single tree for the ROOT slot of PCFG."
  (cky :acc-clause
       (let* ((cur (cons rule s))
              (l (atomize (@ *pi* i s (second rule))))
              (r (atomize (@ *pi* (1+ s) j (third rule))))
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
                 arg (take n arg))))
       :result-clause
       (progn
         ;; for the case of fake root determine the best real root
         (when root-rules
           (loop :for (rule . prob) :in root-rules
              :collect (cons (mapcar #'nt->idx rule) *n*) :into rules
              :collect prob :into probs
              :finally (setf (@ *pi* 1 *n* iroot) probs
                             (@ *bp* 1 *n* iroot) rules)))
         (let ((best (n-best-parse-trees n sentence iroot)))
           (values (mapcar #`(idx->nts (car %)) best)
                   (mapcar #`(exp (cdr %)) best)
                   *pi*
                   *bp*)))))

) ; end of macrolet


;;; Tree decoding

(defun decode-parse-tree (sentence i j iroot)
  "Restore parse tree of the SENTENCE
   with non-treminal IROOT from Backpointers matix with bounds I and J."
  (ds-bind (rule . s) (@ *bp* i j iroot)
    (if (= i j)
        (list rule (nth (1- i) sentence))
        (list* (first rule)
               (decode-parse-tree sentence i s (second rule))
               (when-it (third rule)
                 (list (decode-parse-tree sentence (1+ s) j it)))))))

(defun n-best-parse-trees (n sentence iroot)
  "Return N best parse trees with their scores,
   starting from IROOT for SENTENCE with the Viterbi Pi and Backpointers
   matrices."
  (let ((visited (make-hash-table))
        (n *n*)
        rez)
    (loop :while (plusp n) :do (decf n) :collect
       (funcall (decode-best-parse-tree sentence visited 1 n iroot)))))

(defun decode-best-parse-tree (sentence visited i j iroot)
  "Return the best parse tree with its score for Viterbi Pi and Backpointers
   matrices holding not single maximums but lists of best n variants,
   with bounding indices I and J and root node index IROOT,
   considering the matrix of already visited variants VISITED
   (which were used in decoding the better parse trees)."
  (labels ((decode-alt-tree (i j iroot alt-idx)
             (ds-bind (rule . s) (nth alt-idx (@ *bp* i j iroot))
               (if (= i j)
                   (list rule (nth i sentence))
                   (list* (first rule)
                          (decode-alt-tree i s (second rule) 0)
                          (when-it (third rule)
                            (list (decode-alt-tree (1+ s) j it 0))))))))
    (let* ((unvisited (or (@ visited i j iroot) 0))
           (cur-bp (@ *bp* i j iroot))
           (cur-pi (@ *pi* i j iroot))
           (min most-negative-single-float)
           (best (if (< unvisited (length cur-bp))
                     (list (nth unvisited cur-pi)
                           #`(decode-alt-tree i j iroot unvisited)
                           (list i j iroot))
                     (list min))))
      (loop :for bp :in (take unvisited cur-bp)
            :for score :in (take unvisited cur-pi) :do
         (ds-bind ((h l &optional r) . s) bp
           ;; look at alternative paths at current node
           (loop :for (ii jj kk) :in (cons (list i s l)
                                           (when r (list (list (1+ s) j r)))) :do
              (let* ((cur-scores (@ *pi* ii jj kk))
                     (next (1+ (@ visited ii jj kk)))
                     (alt-score (when (< next (length cur-scores))
                                 (+ score (- (nth next cur-scores)
                                             (car cur-scores))))))
               (when (and alt-score (> alt-score (car best)))
                 (:= best
                     (list alt-score
                           #`(if (= i ii)
                                 ;; explore left alternatives
                                 (list iroot
                                       (decode-alt-tree i s l next)
                                       (when r (decode-alt-tree (1+ s) j r 0)))
                                 ;; explore right alternatives
                                 (list iroot
                                       (decode-alt-tree i s l 0)
                                       (decode-alt-tree (1+ s) j r next)))
                           (list ii jj kk))))))
           ;; look below already visited nodes
           (dolist (idx (cons (list i s h)
                              (when (< s *n*)
                                (list (list (1+ s) j h)))))
             (ds-bind (tree-decoder . alt-score)
                 (apply #'decode-best-parse-tree sentence visited idx)
               (when (> alt-score (car best))
                 (:= best (list alt-score
                                #`(list iroot (funcall tree-decoder))
                                idx)))))))
      (ds-bind (i j k) best
        (:+ (@ visited best) 0))
      (cons (second best) (first best)))))


;;; Parsing with pretagged PCFG

(defclass pretagged-cky-parser (cky-parser)
  ()
  (:documentation
   "CKY parser that works with already tagged TOKENs,
    so it doesn't deal with unary rules at all."))

(macrolet ((init-for-pretagged-cky ()
             `(doindex (i token sentence)
                (let ((k (get# (token-tag token) nts->idx)))
                  (:= (@ *pi* i i k) 0.0
                      (@ *bp* i i k) (cons k i))))))

(defmethod parse :before ((parser pretagged-cky-parser) (grammar pcfg)
                          (sentence list))
  (init-for-pretagged-cky))

(defmethod parse-n :before ((parser pretagged-cky-parser) (grammar pcfg)
                            (sentence list) n)
  (init-for-pretagged-cky))

) ; end of macrolet
