;;; (c) 2014 Vsevolod Dyomkin

(in-package #:nlp.core)
(named-readtables:in-readtable rutils-readtable)


(defparameter *number-regex*
  (re:create-scanner "^-*\\d+[0-9,\\.]+$"))
(defparameter *email-regex*
  (re:create-scanner
   "<?(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?)>?"
   :case-insensitive-mode t))
(defparameter *url-regex*
  (re:create-scanner
   "<?\\b(?:https?|ftp|file)://[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#/%=~_|]>?"
   :case-insensitive-mode t))


(defgeneric normalize (form data)
  (:documentation
   "Normalize DATA to some FORM."))

(defgeneric denormalize (form data)
  (:documentation
   "Denormalize DATA from some FORM."))
