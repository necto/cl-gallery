(restas:define-module #:upload
    (:use #:cl #:cl-who #:files-locator)
  (:export #:*file-stored-callback*
           #:*store*
           #:*mime-type*

           #:form

           #:upload-empty-url
           #:upload-file
           #:upload-form-main))

(in-package #:upload)

(defvar *mime-type* nil
  "The substring to be present in the MIME type of the uploaded file.
   And the filter for files in file-open dialog.
   Should look like 'image', or 'text' -- one of common media types.
   If it is not present, no file accepted.")
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

(defun form (&key (multiple nil))
  "The part responsible for the open-and-upload file dialog.
  multiple: whether or not allow multiple files upload.
  CAUTION: the functin must be called from the context of your upload submodule!"
  (restas:assert-native-module)
  (let ((accept (when *mime-type* (format nil "~a/*" *mime-type*))))
    (with-html-output-to-string (sss)
      (htm (:div :id "upload-div"
                 (:form :method "post" :action (restas:genurl* 'upload-file)
                        :enctype "multipart/form-data" :id "file-upload"
                        :target "upload_target_iframe"
                              ;^^ comment this line to view the post-request respond
                        (:input :id "file-upload-input" :type "file"
                                :onChange "document.getElementById(\"file-upload\").submit();"
                                :name "file" :accept accept
                              :multiple multiple))
                 (:iframe :id "upload_target_iframe" :name "upload_target_iframe"
                          :src (restas:genurl* 'upload-empty-url)
                          :style "width:0;height:0;border:0px solid #fff;"))))))

(restas:define-route upload-form-main ("form")
  (with-html-output-to-string (sss)
    (htm (:html (:head) (:body (str (form)))))))

(defun generate-uniq-fname (base)
  (format nil "~a.~a" (get-universal-time) base))

(defun handle-file-param (param)
  (destructuring-bind (path file-name content-type) param
    (declare (ignore content-type))
    (let ((fname (generate-uniq-fname file-name)))
      (let ((new-path (file-path *store* fname)))
        (rename-file path (ensure-directories-exist new-path)))
    fname)))

(defun valid-type (param)
  (destructuring-bind (path file-name content-type) param
    (declare (ignore path file-name))
    (when (search *mime-type* content-type)
        param)))

(restas:define-route upload-file ("upload-file" :method :post)
  (with-html-output-to-string (sss)
    (:script :language "javascript" :type "text/javascript"
       (str (funcall *file-stored-callback*
                     (mapcar #'(lambda (param)
                                 (handle-file-param (cdr param)))
                             (remove-if (complement #'valid-type)
                                        (remove "file" (hunchentoot:post-parameters*)
                                                :test (complement #'equal)
                                                :key #'car)
                                        :key #'cdr)))))))

(restas:define-route upload-empty-url ("upload-empty-url")
  (with-html-output-to-string (sss)
    (:h1 "This is a stub page just for convenience of ajax file uploader")))
