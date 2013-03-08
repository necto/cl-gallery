
(restas:define-module #:gallery
    (:use :cl :cl-who :files-locator)
  (:export #:main
           #:add-pic
           #:receive-pic))

(in-package #:gallery)

(restas:mount-submodule files (#:restas.directory-publisher)
  (restas.directory-publisher:*directory* #p"/tmp/")
  (restas.directory-publisher:*baseurl* '("files"))
  (restas.directory-publisher:*autoindex* t))

(defparameter *current-file* nil)

(defparameter *pictures* nil)

(defvar *store* (make-instance 'files-store :upload-dir "/tmp/" :download-dir "files/"))

(restas:mount-submodule upl (#:upload)
  (upload:*baseurl* '("upload"))
  (upload:*store* *store*)
  (upload:*multiple* nil)
  (upload:*file-stored-callback*
   (lambda (file)
     (when *current-file*
         (delete-file (file-path *store* *current-file*)))
     (setf *current-file* file)
     (format nil "parent.done(\"~a\", \"~a\");" (file-url *store* file) file))))

(restas:define-route add-pic ("add")
  (with-html-output-to-string (sss)
    (htm "<!DOCTYPE html>"
         (:html (:head (:script :language "javascript" :type "text/javascript"
                                "function done(preview, file) {
                                                      document.getElementById(\"preview\").src = preview;
                                                      document.getElementById(\"pic\").value = file;}"))
                (:body (str (restas:with-context (second (gethash 'upl *submodules*))
                              (upload:form (restas:genurl-submodule
                                            'upl 'upload:upload-file)
                                           (restas:genurl-submodule
                                            'upl 'upload:upload-empty-url))))
                       (:img :id "preview")
                       (:form :method "get" :action (restas:genurl 'receive-pic)
                              (:input :type "hidden" :name "pic" :value "no-value" :id "pic")
                              (:input :type "submit" :value "like it!")))))))

(defun gen-small-pic-fname (fname)
  (format nil "~a.thumb.~a" (subseq fname 0 (- (length fname) 4))
          (subseq fname (- (length fname) 3) (length fname))))

(defun make-thumb (fname)
  (let ((small-fname (gen-small-pic-fname fname)))
    (sb-ext:run-program "/usr/bin/convert" 
                        (list "-scale" "100x100" 
                              (file-pathname *store* fname)
                              (file-pathname *store* small-fname))
                        :wait t)
    (file-url *store* small-fname)))

(restas:define-route receive-pic ("likeit")
  (setf *current-file* nil)
  (let ((file (hunchentoot:get-parameter "pic")))
    (push (list (file-url *store* file)
                (make-thumb file))
          *pictures*)
    (restas:redirect 'main)))

(restas:define-route main ("")
  (with-html-output-to-string (sss)
    "<!DOCTYPE html>"
    (:html (:head)
           (:body (:a :href (restas:genurl 'add-pic)
                      "add a picture")
                  (loop for (pic thumb) in *pictures* do
                       (htm (:a :href pic (:img :src thumb))))))))
