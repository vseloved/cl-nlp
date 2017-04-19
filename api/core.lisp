;;; (c) 2017 Vsevolod Dyomkin

(in-package #:napi)
(named-readtables:in-readtable rutilsx-readtable)


;;; main functions

(defparameter *req* nil
  "Current request")

(defgeneric api-process (endpoint data)
  (:documentation
   "Specific processing of an API POST request to ENDPOINT
     containing the payload DATA."))

(defgeneric api-show (endpoint)
  (:documentation
   "Specific display for the API GET request to ENDPOINT."))

(defun api-log (severity &optional msg)
  (if *req*
      (let ((path (getf *req* :path-info))
            (method (getf *req* :request-method))
            (host (or (? (getf *req* :headers) "X-Forwarded-For")
                      (getf *req* :remote-addr))))
        (v:log severity :api "~A ~A ~A => ~A" host method path msg))
      (v:log :error :api "No *REQ* object")))

(defun napi (req)
  "Run API to process request REQ."
  (with ((*req* req)
         (path (getf req :path-info))
         (method (getf req :request-method))
         (endpoint (mkeyw (slice path 1 (position #\/ path :start 1)))))
    (handler-case
        (ecase method
          (:POST
           (if (find-method #'api-process nil (list `(eql ,endpoint) t) nil)
               (with ((buf (make-array (getf req :content-length)
                                       :element-type 'flex:octet))
                      (data (yason:parse
                             (babel:octets-to-string
                              (progn (read-sequence buf (getf req :raw-body))
                                     buf)
                              :encoding :utf-8)))
                      (rez (api-process endpoint data)))
                 (api-log :info (first rez))
                 rez)
               (error404)))
          (:GET (if (find-method #'api-show nil (list `(eql ,endpoint)) nil)
                    (let ((rez (api-show endpoint)))
                      (if (stringp rez)
                          (list 200 nil (list rez))
                          rez))
                    (error404))))
      (error (e)
        (api-log :error (fmt "500 - ~A" e))
        '(500 nil nil)))))

(defmethod api-show ((endpoint (eql :static)))
  (with ((path (getf *req* :path-info))
         (filename (asdf:system-relative-pathname
                    'cl-nlp-api (strcat "api" path))))
    (if-it (probe-file filename)
           (list 200
                 (list "Content-Type" (or (hunchentoot:mime-type it)
                                          "application/octet-stream")
                       "Last-Modified" (hunchentoot:rfc-1123-date
                                        (or (file-write-date it)
                                            (get-universal-time)))
                       "Content-Length"
                       (with-open-file (in it :element-type 'flex:octet)
                         (file-length in)))
                 it)
        '(404 nil nil))))
    
;;; utils

(defun error404 ()
  (api-log :info 404)
  '(404 nil nil))

(defmacro api-page ((title &key style script) &body body)
  "Standard page template."
  `(who:with-html-output-to-string (out)
     (:html
      (:head
       (:title (who:str ,title))
       (:link :rel "stylesheet" :type "text/css" :href "/static/main.css")
       (:link :rel "icon" :type "image/png" :href "/static/favico.png")
       (when ',style
         (who:htm (:link :rel "stylesheet" :type "text/css"
                         :href (fmt "/static/~A.css" ,style))))
       (:script :type "text/javascript" :src "/static/jquery-1.11.3.js" "")
       (:script :type "text/javascript" :src "/static/main.js" "")
       (when ',script
         (who:htm (:script :type "text/javascript"
                           :src (fmt "/static/~A.js" ,script) ""))))
      (:body
       (:div :class "page"
             ,@body)
       (:div :class "footer"
             (:span :class "tagline"
                    (who:fmt "&nbsp; ~A (m8n) Enjoy cl-nlp API at your own risk"
                             (nth-value 5 (decode-universal-time (get-universal-time))))))))))
