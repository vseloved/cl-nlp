;;; (c) 2014-2017 Vsevolod Dyomkin

(in-package #:nlp.learning)
(named-readtables:in-readtable rutilsx-readtable)


(defstruct (ex (:print-object (lambda (ex stream)
                                (format stream ">>~A:~A" @ex.gold @ex.fs))))
  fs gold raw)

(defclass categorical-model ()
  ((weights :initarg :weights :initform #h() :accessor m-weights))
  (:documentation
   "A categorical model has some way of distinguishing different categories
    based on the WEIGHTS mapping categories to some models."))

(defclass ensemble-model ()
  ()
  (:documentation
   "An ensemble model aggregates the results of many weak classifiers."))


(defgeneric score (model fs class)
  (:documentation
   "Score a selected CLASS with a MODEL given current FS.")
  (:method (model fs class)
    (? (rank model fs) class)))

(defgeneric rank (model fs &key classes)
  (:documentation
   "Score all or selected CLASSES of a MODEL in a table
    for a given feature set FS.")
  (:method :around (model fs &key classes)
    (let ((rez (call-next-method)))
      (when classes
        (:= rez (keep-if ^(member (first %) classes)
                         rez)))
      rez))
  (:method :around (model (fs ex) &key classes)
    (call-next-method model @fs.fs :classes classes)))


(defgeneric classify (model fs &key classes)
  (:documentation
   "Classify a feature set FS with a MODEL.
    A selected set of CLASSES can be provided.")
  (:method (model fs &key classes)
    (keymax (rank model fs :classes classes))))

(defvar *training* nil)

(defgeneric train (model data &key)
  (:documentation
   "Train some MODEL with the provided DATA.")
  (:method :around (model data &key)
    (let ((*training* t))
      (call-next-method)))
  (:method :after ((model categorical-model) data &key)
    (rem# nil (m-weights model))))

(defgeneric train1 (model gold guess gold-fs &optional guess-fs)
  (:documentation
   "Perform one step of MODEL training with GOLD and GUESS variants
    and correspondng GOLD-FS and optionally GUESS-FS features."))

(defgeneric cost (model data &key)
  (:documentation
   "Calculate the cost of the current DATA set for the given MODEL."))

(defgeneric save-model (model path)
  (:documentation
   "Save MODEL data into PATH (which will be overwritten).")
  (:method :around ((model categorical-model) path)
    (with-open-file (out path :direction :output :element-type 'flex:octet
                         :if-does-not-exist :create)
      (:= out (flex:make-flexi-stream (gzip-stream:make-gzip-output-stream out)
                                      :external-format :utf8))
      (call-next-method model out)
      path))
  (:method ((model categorical-model) out)
    (let ((total (sum 'ht-count (vals @model.weights)))
          (i 0))
      (format out "~A~%" (ht-count @model.weights))
      (dotable (c fw @model.weights)
        (format out "~S ~A~%" c (ht-count fw))
        (dotable (f w fw)
          (format out "~S ~A " (string f) (float w))
          (princ-progress (:+ i) total))
        (terpri out)))))

(defgeneric load-model (model path &key)
  (:documentation
   "Load MODEL data from PATH (overwriting existing model).
    Keyword arg CLASSES-PACKAGE determines the package where class names
    will be interned (default: keyword).")
  (:method :around (model path &key class-package)
    (format *debug-io* "~&Loading model from file: ~A - "
            (typecase path
              (flex:flexi-stream (or (ignore-errors
                                      (? (flex:flexi-stream-stream path) 'file))
                                     path))
              (stream (or (ignore-errors (? path 'file))
                          path))
              (t path)))
    (prog1 (call-next-method)
      (format *debug-io* "done.~%")))
  (:method :around ((model categorical-model) path &key class-package)
    (with-open-file (in path :element-type 'flex:octet)
      (:= in (flex:make-flexi-stream (gzip-stream:make-gzip-input-stream in)
                                     :external-format :utf8))
      (call-next-method model in :class-package (find-package class-package))))
  (:method ((model categorical-model) in &key class-package)
    (let ((total (read in))
          (i 0))
      (loop :repeat total :do
        (with ((class (let ((*package* (or class-package *package*)))
                        (read in)))
               (count (read in))
               (weights (set# class (m-weights model) #h(equal))))
          (loop :repeat count :do
            (setf (? weights (read in)) (read in)))
          (princ-progress (:+ i) total))))
    (rem# nil (m-weights model))
    model))


;;; quality measurement

(defgeneric accuracy (model gold-fs &key verbose)
  (:documentation
   "Measure MODEL's performance on GOLD-FS gold resultx features.")
  (:method (model gold-fs &key verbose)
    (let ((matched 0)
          (total 0)
          (len (length gold-fs)))
      (map nil ^(let ((guess (classify model @%.fs)))
                  (if (equal @%.gold guess)
                      (:+ matched)
                      (when verbose
                        (format *debug-io*
                                "~%guess: ~A~%gold:  ~A~@[~%raw: ~A~]~%fs: ~A~%"
                                guess @%.gold @%.raw @%.fs)))
                  (:+ total)
                  (unless verbose (princ-progress total len)))
           gold-fs)
      (* 100.0 (/ matched total)))))

(defgeneric f_ (model gold-fs class &key n verbose)
  (:documentation
   "Measure MODEL's f1 (or fN) on GOLD-FS gold result features.")
  (:method (model gold-fs class &key (n 1) verbose)
    (let ((tp 0)
          (fp 0)
          (fn 0)
          (total 0)
          (len (length gold-fs))
          (n2 (expt n 2)))
      (when (zerop len)
        (warn "No data given to f1.")
        (return-from f_ (values 0.0
                                0.0
                                0.0)))
      (map nil ^(let ((guess (classify model @%.fs)))
                  (if (and (equal guess class)
                           (equal guess @%.gold))
                      (:+ tp)
                      (progn
                        (cond ((eql class guess)
                               (:+ fp))
                              ((eql class @%.gold)
                               (:+ fn)))
                        (when verbose
                          (format *debug-io*
                                  "~%guess: ~A~%gold:  ~A~@[~%raw: ~A~]~%fs: ~A~%"
                                  guess @%.gold @%.raw @%.fs))))
                  (:+ total)
                  (unless verbose (princ-progress total len)))
           gold-fs)
      (when (zerop tp)
        (return-from f_ (values 0.0
                                0.0
                                0.0)))
      (let ((rec (/ tp (+ tp fn)))
            (prec (/ tp (+ tp fp))))
        (values (/ (* (+ 1.0 n2) prec rec)
                   (+ (* n2 prec) rec))
                (float prec)
                (float rec))))))

(defun f1 (model gold-fs class &key verbose)
  (f_ model gold-fs class :verbose verbose))

(defgeneric fs-importance (model &key)
  (:documentation
   "Calculate MODEL's feature importance."))

(defun conf-mat (model gold-fs &key (verbose t))
  "Calculate (and print when VERBOSE) the confusion matrix
   for the MODEL on GOLD-FS."
  (let ((rez #h())
        (len (length gold-fs))
        (cc 0))
    (map nil ^(prog1 (:+ (get# (classify model @%.fs)
                               (getset# @%.gold rez #h())
                               0))
                (princ-progress (:+ cc) len))
         gold-fs)
    (when verbose
      (dolist (row (sort (ht->pairs rez) '>
                         :key ^(sum 'just (vals (rt %)))))
        (with (((cat counts) row)
               (total (sum 'just (vals counts))))
          (format t "~(~A~) (~A): ~$%  |"
                  cat total
                  (float (/ (get# cat counts 0) total 0.01)))
          (dolist (entry (sort (ht->pairs counts) '> :key 'rt))
            (unless (eql (lt entry) cat)
              (format t "  ~$% ~(~A~)"
                      (float (/ (rt entry) total 0.01))
                      (lt entry))))
          (terpri))))
    rez))

(defun print-metrics (model gold-fs &key (out *standard-output*) classes)
  (with ((classes (or classes @model.classes))
         (len (length classes))
         (n (length gold-fs))
         (f1_ 0.0)
         (f1* 0.0)
         (prec_ 0.0)
         (prec* 0.0)
         (rec_ 0.0)
         (rec* 0.0)
         (span (- (reduce 'max (mapcar (=> length princ-to-string)
                                       classes))
                  (length " class")))
         (*debug-io* (make-broadcast-stream)))
    (format out (strcat "~{~C~}  class |  f1  | prec |  rec | count (% of total) ~%")
            (loop :repeat span :collect #\Space))
    (format out (strcat "~{~C~}--------|------|------|------|--------------------~%")
            (loop :repeat span :collect #\-))
    (dolist (class classes)
      (with ((f1 prec rec (f1 model gold-fs class))
             (count (count class gold-fs :key 'nlp:ex-gold)))
        (:+ f1_ f1)
        (:+ f1* (* f1 count))
        (:+ prec_ prec)
        (:+ prec* (* prec count))
        (:+ rec_ rec)
        (:+ rec* (* rec count))
        (format out "  ~5A | ~4F | ~4F | ~4F | ~D (~D%) ~%"
                (princ-to-string class)
                f1 prec rec count
                (floor (* (/ count n) 100)))))
    (terpri out)
    (format out "  avg   | ~4F | ~4F | ~4F | ~D ~%"
            (/ f1* n) (/ prec* n) (/ rec* n)
            (length gold-fs))
    (format out "  wavg  | ~4F | ~4F | ~4F | ~D ~%"
            (/ f1_ len) (/ prec_ len) (/ rec_ len)
            len)
    (format out "accuracy: ~4F~%" (/ (nlp:accuracy model gold-fs)
                                     100.0))))

(defun split-dev-test (data &optional (ratio 0.7))
  "Split DATA into dev & test sets with a given RATIO."
  (let ((split-pt (floor (* (length data) ratio)))
        (data (shuffle data)))
    (list (subseq data 0 split-pt)
          (subseq data split-pt))))
