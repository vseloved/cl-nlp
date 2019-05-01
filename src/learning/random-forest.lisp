;;; (c) 2015-2016 Vsevolod Dyomkin

(in-package #:nlp.learning)
(named-readtables:in-readtable rutilsx-readtable)


(defclass random-forest (ensemble-model)
  ((trees :accessor forest-trees :initform () :initarg :trees)
   (tree-type :accessor forest-tree-type :initform 'cart :initarg :tree-type)
   (random-state :accessor m-random-state :initform (make-random-state t)
                 :initarg :random-state)
   (classes :accessor tree-classes :initarg :classes)
   (max-depth :accessor tree-max-depth :initform nil :initarg :max-depth)
   (min-size :accessor tree-min-size :initform 0 :initarg :min-size)))


(defmethod fs-importance ((model random-forest) &key oobs &allow-other-keys)
  (let ((rez #h()))
    (loop :for dtree :in @model.trees
          :for oob :in oobs :do
          (loop :for ex :in oob :do
            (dotimes (i (length @ex.fs))
              (push (pair (eql @ex.gold (classify model @ex.fs))
                          (let ((alt (copy-seq @ex.fs)))
                            (:= (? alt i)
                                (etypecase (? alt i)
                                  (float (- (random 2000.0) 1000.0))
                                  (integer (- (random 2000) 1000))
                                  (boolean (not (? alt i)))))
                            (eql @ex.gold (classify model alt))))
                    (? rez i))))
          (princ "."))
    rez))

;; example:
;; (dotable (fs vals (nlearn::fs-importance *model* :oobs *oobs*))
;;   (print (list (? *fs-idx* fs)
;;                (float (/ (count-if-not ^(equal (lt %) (rt %)) vals)
;;                          (length vals))))))

(defmethod train ((model random-forest) data
                  &key (n 10) verbose threads fast)
  (when threads (eager-future2:advise-thread-pool-size threads))
  (let ((train-len (ceiling (* 2/3 (length data))))
        (train-rank (ceiling (sqrt (length @data#0.fs)))) ; also possible 1/2x & 2x
        (*random-state* @model.random-state)
        oobs
        futures)
    (dotimes (i n)
      (with ((dt (make 'cart-tree
                       :min-size (tree-min-size model)
                       :max-depth (tree-max-depth model)))
             (sample (sample data train-len))
             (rez (call (if threads 'eager-future2:pcall 'call)
                        ^(train dt sample :idx-count train-rank
                                :verbose verbose :fast fast)))
             (sample (uniq sample :raw t))
             (oob nil))
        (if threads
            (push rez futures)
            (format *debug-io* "+"))
        (dolist (ex data)
          (unless (in# ex sample)
            (push ex oob)))
        (push oob oobs)
        (push dt (forest-trees model))))
    (when threads
      (dolist (fut futures)
        (eager-future2:yield fut)
        (format *debug-io* "+")))
    (values model
            oobs)))

(defmethod rank ((model random-forest) fs &key classes)
  (let* ((trees (forest-trees model))
         (w (/ 1 (length trees)))
         (rez (mapcar ^(rank % fs :classes classes)
                      ;; ^(pairs->ht
                      ;;   (list (multiple-value-list
                      ;;          (classify % fs :classes classes))))
                      trees))
         (scores #h())
         (total 0))
    (dolist (r rez)
      (dotable (class weight r)
        (let ((score (* weight w)))
          (:+ total score)
          (:+ (get# class scores 0) score))))
    (maptab ^(/ %% total) scores)))

(defmethod save-model ((model random-forest) path)
  (zip:with-output-to-zipfile (zip path :if-exists :supersede)
    (zip:write-zipentry zip "random-state.txt"
                        (with-standard-io-syntax
                          (flex:make-in-memory-input-stream
                           (babel:string-to-octets 
                            (princ-to-string @model.random-state))))
                        :file-write-date (get-universal-time))
    (doindex (i tree @model.trees)
      (zip:write-zipentry zip (fmt "tree~A.txt.gz" (1+ i))
                          (flex:make-in-memory-input-stream
                           (gzip-stream:gzip-sequence 
                            (babel:string-to-octets (with-output-to-string (out)
                                                      (save-model tree out))
                                                    :encoding :utf-8)))
                          :file-write-date (get-universal-time))))
  path)
    
(defmethod load-model ((model random-forest) path &key)
  (zip:with-zipfile (zip path :force-utf-8 t)
    (:= @model.random-state (read-from-string (zipped-file-data
                                               zip "random-state.txt")))
    (dolist (tree-file (keep-if ^(starts-with "tree" %)
                                (keys (zip:zipfile-entries zip))))
      (push (load-model (make 'cart-tree)
                        (gzip-stream:make-gzip-input-stream
                         (flex:make-in-memory-input-stream
                          (zipped-file-data zip tree-file :encoding nil))))
            @model.trees)))
  model)

