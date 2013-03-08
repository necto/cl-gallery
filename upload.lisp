(restas:define-module #:upload
    (:use #:cl #:cl-who #:files-locator)
  (:export #:*file-stored-callback*
           #:*store*
           #:*multiple*

           #:form

           #:upload-empty-url
           #:upload-file
           #:upload-form-main))

(in-package #:upload)

(defvar *multiple* nil
  "Whether or not allow user upload multiple files at a time")
(defvar *store* (make-instance 'files-store :upload-dir "/tmp/" :download-dir "fl/")
  "The location, where the uploaded files will be stored, and where it can be downloaded")
(defvar *file-stored-callback*
  #'(lambda (fname)
      (format nil "parent.alert(\"finished at: ~a\");" (file-url *store* fname)))
  "The function, being called after the file is received by the hunchentoot,
   copyed into the *upload-dir* under a uniq name.
   The argument is the name of the result file, with attached *download-dir*.
   The result will be inserted in the JS script body of the server response, 
   so, you can inform the parent page, that the uploading is complete.
   Depending on *multiple* the fnames will be the string, or the list of
   strings, corresponding to the list of accepted files.")

(defun form (target empty-target)
  "The part responsible for the open-and-upload file dialog.
  CAUTION: the functin must be called from the context of your upload submodule!"
  (with-html-output-to-string (sss)
    (htm (:div :id "upload-div"
               (:form :method "post" :action target
                      :enctype "multipart/form-data" :id "file-upload"
                      :target "upload_target_iframe"
                       ;^^ comment this line to view the post-request respond
                      (:input :id "file-upload-input" :type "file"
                              :onChange "document.getElementById(\"file-upload\").submit();"
                              :name "file" :multiple *multiple*))
               (:iframe :id "upload_target_iframe" :name "upload_target_iframe"
                        :src empty-target
                        :style "width:0;height:0;border:0px solid #fff;")))))

(restas:define-route upload-form-main ("form")
  (with-html-output-to-string (sss)
    (htm (:html (:head)
                (:body (str (form (restas:genurl 'upload-file)
                                  (restas:genurl 'upload-empty-url))))))))

(defun generate-uniq-fname (base)
  (format nil "~a.~a" (get-universal-time) base))

(defun handle-file-param (param)
  (destructuring-bind (path file-name content-type) param
    (declare (ignore content-type))
    (let ((fname (generate-uniq-fname file-name)))
      (let ((new-path (file-path *store* fname)))
        (rename-file path (ensure-directories-exist new-path)))
    fname)))

(restas:define-route upload-file ("upload-file" :method :post)
  (with-html-output-to-string (sss)
    (:script :language "javascript" :type "text/javascript"
       (str (funcall *file-stored-callback*
                     (if *multiple*
                         (mapcar #'(lambda (param)
                                     (handle-file-param (cdr param)))
                                 (remove "file" (hunchentoot:post-parameters*)
                                         :test (complement #'equal)
                                         :key #'car))
                         (handle-file-param (hunchentoot:post-parameter "file"))))))))

(restas:define-route upload-empty-url ("upload-empty-url")
  (with-html-output-to-string (sss)
    (:h1 "This is a stub page just for convenience of ajax file uploader")))
