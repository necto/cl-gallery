
(defpackage :files-locator
  (:use :cl-user :cl)
  (:export #:files-store
           #:upload-dir
           #:download-dir

           #:file-path
           #:file-url
           #:file-pathname))

(in-package :files-locator)

(defclass files-store ()
  ((upload-dir
    :initarg :upload-dir
    :accessor upload-dir ;; <-- Should be reader ??
    :initform (error "cpecify the upload-dir field")
    :documentation
    "the directory (alogn with the trailing / -- it's important),
     being used to store all received files")
   (download-dir
    :initarg :download-dir
    :accessor download-dir ;; <-- It must be reader!! 
    :initform (error "cpecify the download-dir")
    :documentation
    "the prefix of the file name as the client will see the url to it.")))

(defgeneric file-path (store fname)
  (:documentation
   "generate the pathname, pointing to the file named fname in the store"))
(defgeneric file-pathname (store fname)
  (:documentation
   "the path name of the path, see file-path"))
(defgeneric file-url (store fname)
  (:documentation
   "The url for downloading the given file (named fname) from the store"))

(defmethod file-path ((store files-store) fname)
  (make-pathname :name fname
                 :type :unspecific ;; <- nil value causes a crash
                 :directory (upload-dir store)))

(defmethod file-pathname ((store files-store) fname)
  (format nil "~a~a" (upload-dir store) fname))

(defmethod file-url ((store files-store) fname)
  (format nil "~a~a" (download-dir store) fname))
