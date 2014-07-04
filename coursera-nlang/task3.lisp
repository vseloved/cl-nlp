;;; (c) 2013 Vsevolod Dyomkin

(cl:in-package #:cnlang)
(named-readtables:in-readtable rutils-readtable)


(defvar *qs*)
(defvar *tr*)


;; (defun read-corpus (en-file es-file)
;;   (with-open-file (en en-file)
;;     (with-open-file (es es-file)
;;       (loop :for en-line := (read-line en nil nil) :while en-line
;;             :for es-line := (read-line es nil nil) :while es-line
;;             :collect (cons (split #\Space en-line)
;;                            (split #\Space es-line))))))

(defun get-params (corpus)
  (let (ls ms)
    (loop :for (es . fs) :in corpus
       :collect es :into ess
       :collect fs :into fss
       :do (pushnew (length es) ls)
           (pushnew (length fs) ms)
       :finally (return (values (uniq (flatten ess))
                                (uniq (flatten fss))
                                ls
                                ms)))))

(defun t1 (train-corpus eval-corpus outfile)
  (mv-bind (tr ess fss) (init-tr train-corpus)
    (loop :repeat 5 :do
       (print "+++")
       (ibm1-em train-corpus tr ess fss))
    (write-alignments-ibm1 outfile eval-corpus tr)
    tr))

(defun t2 (train-corpus eval-corpus ibm1-tr outfile)
  (let ((tr ibm1-tr)
        (qs #{}))
    (mv-bind (ess fss ls ms) (get-params train-corpus)
      (loop :repeat 5 :do
         (print "+++")
         (ibm2-em train-corpus tr qs ess fss ls ms))
      (setf *tr* tr
            *qs* qs)
      (write-alignments-ibm2 outfile eval-corpus tr qs)
      (values tr
              qs))))

(defmacro tr (f e)
  `(if-it (get# ,e tr)
          (get# ,f it)
          0))

(defmacro idx (i j l &optional m)
  (if m
      `(+ (* 1000000000 ,j) (* 1000000 ,i) (* 1000 ,l) ,m)
      `(- (+ (* 1000000 ,j) (* 1000 ,i) ,l))))

(defsetf tr (f e) (val)
  `(set# ,f (get# ,e tr) (float ,val)))

(defmacro q (j i l m)
  `(get# (idx ,j ,i ,l ,m) qs (random 100)))

(defsetf q (j i l m) (val)
  `(set# (idx ,j ,i ,l ,m) qs (float ,val)))

(defun init-tr (corpus)
  (let ((e/f (make-hash-table :test 'equal))
        (e-counts (make-hash-table :test 'equal))
        (tr (make-hash-table :test 'equal))
        ess fss)
    ;; init counts
    (loop :for (es . fs) :in corpus :do
       (dolist (e es)
         (set# e e/f (cons fs (get# e e/f)))))
    (dotable (e fs e/f)
      (set# e e-counts (length (set# e e/f (uniq (flatten fs))))))
    (setf ess (ht-keys e/f))
    (setf fss (uniq (flatten (ht-vals e/f))))
    ;; init tr
    (dotable (e fs e/f)
      (let ((cur (make-hash-table :test 'equal))
            (1/count (float (/ 1 (get# e e-counts)))))
        (dolist (f fs)
          (incf (get# f cur 0) 1/count))
        (set# e tr cur)))
    ;; init for nil word
    (let* ((fs (uniq (flatten (ht-vals e/f))))
           (1/count (float (/ 1 (length fs))))
           (cur (make-hash-table :test 'equal)))
      (dolist (f fs)
        (set# f cur 1/count))
      (set# nil tr cur))
    (values tr
            ess
            fss)))

(defun ibm1-em (corpus tr ess fss)
  (let ((c (make-hash-table :test 'equal)))
    (loop :for (es . fs) :in corpus :do
       (dolist (f fs)
         (let ((trs (reduce #'+ (mapcar #`(tr f %) (cons nil es)))))
           (dolist (e (cons nil es))
             (let ((delta (/ (tr f e) trs)))
               (incf (get# (cons e f) c 0) delta)
               (incf (get# e c 0) delta))))))
    (dolist (e (cons nil ess))
      (dolist (f fss)
        (when-it (get# (cons e f) c)
          (setf (tr f e)
                (/ it (get# e c))))))
    tr))

(defun ibm2-em (corpus tr qs ess fss ls ms)
  (let ((c (make-hash-table :test 'equal)))
    (loop :for (es . fs) :in corpus :do
       (doindex (i f fs)
         (let* ((l (length es))
                (m (length fs))
                (trs (reduce #'+ (mapcar #`(* (tr f (car %))
                                              (q (cadr %) i l m))
                                         (zip (cons nil es)
                                              (range 0 (1+ (length es))))))))
           (doindex (j e (cons nil es))
             (let ((delta (/ (* (tr f e) (q j i l m))
                             trs)))
               (incf (get# (cons e f) c 0) delta)
               (incf (get# e c 0) delta)
               (incf (get# (idx j i l m) c 0) delta)
               (incf (get# (idx i l m) c 0) delta))))))
    (dolist (l ls)
      (dolist (m ms)
        (dotimes (j (1+ l))
          (dotimes (i m)
            (when-it (get# (idx j i l m) c)
              (setf (q j i l m)
                    (/ it (get# (idx i l m) c))))))))
    (dolist (e ess)
      (dolist (f fss)
        (when-it (get# (cons e f) c)
          (setf (tr f e)
                (/ it (get# e c))))))
    (values tr
            qs)))

(defun write-alignments-ibm1 (outfile corpus tr)
  (with-out-file (out outfile)
    (doindex (k pair corpus)
      (ds-bind (es . fs) pair
        (doindex (i f fs)
          (let ((e (argmax #`(tr f %) (cons nil es))))
            (unless (null e)
              (format out "~A ~A ~A~%"
                      (1+ k)
                      (1+ (position e es :test 'string=))
                      ;; (1+ (cadar (sort (filter #`(string= e (car %))
                      ;;                          (zip es (range 0 (length es))))
                      ;;                  '< :key #`(abs (- (cadr %) i)))))
                      (1+ i)))))))))

(defun write-alignments-ibm2 (outfile corpus tr qs)
  (with-out-file (out outfile)
    (doindex (k pair corpus)
      (ds-bind (es . fs) pair
        (doindex (i f fs)
          (let* ((l (length es))
                 (m (length fs))
                 (e (argmax #`(* (tr f (car %))
                                 (q (cadr %) i l m))
                            (zip (cons nil es)
                                 (range 0 (1+ (length es)))))))
            (unless (zerop (cadr e))
              (format out "~A ~A ~A~%"
                      (1+ k)
                      (1+ (cadar (sort (filter #`(string= (car e) (car %))
                                               (zip es (range 0 (length es))))
                                       '< :key #`(abs (- (cadr %) i)))))
;                      (cadr e)
                      (1+ i)))))))))

(defun argmax (fn vals &key key)
  (let ((max 0)
        arg)
    (dolist (val vals)
      (let ((cur (or (funcall fn (if key (funcall key val) val)) 0)))
        (when (> cur max)
          (setf max cur
                arg val))))
    (values arg
            max)))

(defun uniq (list &key raw case-insensitive)
  "Return only unique elements from LIST either as a new list
   or as hash-table if RAW is set. Can be CASE-INSENSITIVE."
  (let ((uniqs (make-hash-table :test (if case-insensitive 'equalp 'equal))))
    (dolist (elt list)
      (set# elt uniqs t))
    (if raw uniqs (ht-keys uniqs))))