(restas:define-module #:upload
    (:use #:cl #:cl-who #:files-locator)
  (:export #:*file-stored-callback*
           #:*store*

           #:form

           #:upload-empty-url
           #:upload-file
           #:upload-form-main))

(in-package #:upload)

;(defvar *upload-dir* "/tmp/"
;  "the directory, being used to store all received files")
;(defvar *download-dir* "fl/"
;  "the prefix of the file as the client will see it, after uploading.")
(defvar *store* (make-instance 'files-store :upload-dir "/tmp/" :download-dir "fl/"))
(defvar *file-stored-callback*
  #'(lambda (fname) (format nil "parent.alert(\"finished at: ~a\");" (file-url *store* fname)))
  "The function, being called after the file is received by the hunchentoot,
   copyed into the *upload-dir* under a uniq name.
   The argument is the name of the result file, with attached *download-dir*.
   The result will be inserted in the JS script body of the server response, 
   so, you can inform the parent page, that the uploading is complete.")

(defun form (target)
  "The part responsible for the open-and-upload file dialog"
  (with-html-output-to-string (sss)
    (htm (:div :id "upload-div"
               (:form :method "post" :action target
                      :enctype "multipart/form-data" :id "file-upload"
                      :target "upload_target"
                      (:input :id "file-upload-input" :type "file"
                              :onChange "document.getElementById(\"file-upload\").submit();"
                              :name "file"))
               (:iframe :id "upload_target" :name "upload_target" :src "upload-empty-url"
                        :style "width:0;height:0;border:0px solid #fff;")))))

(restas:define-route upload-form-main ("form")
  (with-html-output-to-string (sss)
    (htm (:html (:head)
                (:body (str (form (restas:genurl 'upload-file))))))))

(defun generate-uniq-fname (base)
  (format nil "~a.~a" (get-universal-time) base))

(defun handle-file-param (param-name)
  (destructuring-bind (path file-name content-type)
      (hunchentoot:post-parameter param-name)
    (declare (ignore content-type))
    (let ((fname (generate-uniq-fname file-name)))
      (let ((new-path (file-path *store* fname)))
        (rename-file path (ensure-directories-exist new-path)))
    fname)))

(restas:define-route upload-file ("upload-file" :method :post)
  (with-html-output-to-string (sss)
    (:script :language "javascript" :type "text/javascript"
             (str (funcall *file-stored-callback*
                           (handle-file-param "file"))))))

(restas:define-route upload-empty-url ("upload-empty-url")
  (with-html-output-to-string (sss)
    (:h1 "This is a stub page just for convenience of ajax photo uploader")))
