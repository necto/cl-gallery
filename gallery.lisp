
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

(restas:mount-submodule scripts (#:restas.directory-publisher)
  (restas.directory-publisher:*directory* #p"./static/")
  (restas.directory-publisher:*baseurl* '("static")))

(defparameter *current-files* nil)

(defparameter *pictures* nil)

(defvar *store* (make-instance 'files-store :upload-dir "/tmp/" :download-dir "files/"))

(restas:mount-submodule upl (#:upload)
  (upload:*baseurl* '("upload"))
  (upload:*store* *store*)
  (upload:*multiple* t)
  (upload:*mime-type* nil)
  (upload:*file-stored-callback*
   (lambda (files)
     (when *current-files*
       (mapcar #'(lambda (file)
                   (delete-file (file-path *store* file)))
               *current-files*))
     (setf *current-files* files)
     (let ((*print-pretty* nil))
       (format nil "parent.done([~{\"~a\"~^, ~}], ~
                                \'(~{\"~a\"~^ ~})\');"
               (mapcar #'(lambda (file)
                           (file-url *store* file))
                       files)
               files)))))

(restas:define-route add-pic ("add")
  (with-html-output-to-string (sss)
    (htm "<!DOCTYPE html>"
         (:html (:head (:script :language "javascript" :type "text/javascript"
                                :src "static/js/preview-updater.js"))
                (:body (str (restas:with-context (second (gethash 'upl *submodules*))
                              (upload:form (restas:genurl-submodule
                                            'upl 'upload:upload-file)
                                           (restas:genurl-submodule
                                            'upl 'upload:upload-empty-url))))
                       (:form :method "get" :action (restas:genurl 'receive-pic)
                              "Title:" (:input :type "text" :name "title" :value "")
                              "Comment:" (:input :type "comment" :name "comment" :value "")
                              (:input :type "hidden" :name "pic" :value "no-value" :id "pic")
                              (:input :type "submit" :value "like it!"))
                       (:div :id "preview"))))))

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
  (setf *current-files* nil)
  (with-input-from-string (files-param (hunchentoot:get-parameter "pic"))
    (let ((files (read files-param))
          (title (hunchentoot:get-parameter "title"))
          (comment (hunchentoot:get-parameter "comment")))
      (setf *pictures*
            (nconc (mapcar #'(lambda (file)
                               (list (file-url *store* file)
                                     (make-thumb file)
                                     title
                                     comment))
                           files)
                   *pictures*))
      (restas:redirect 'main))))

(restas:define-route main ("")
  (with-html-output-to-string (sss)
    "<!DOCTYPE html>"
    (:html (:head (:script :language "javascript" :type "text/javascript"
                           :src "http://code.jquery.com/jquery-1.9.1.min.js")
                  (:script :language "javascript" :type "text/javascript"
                           :src "static/js/jquery.mousewheel-3.0.6.pack.js")
                  (:link :rel "stylesheet" :type "text/css" :media "screen"
                           :href "static/css/jquery.fancybox.css")
                  (:script :language "javascript" :type "text/javascript"
                           :src "static/js/jquery.fancybox.pack.js")
                  (:link :rel "stylesheet" :type "text/css" :media "screen"
                           :href "static/css/jquery.fancybox-thumbs.css")
                  (:script :language "javascript" :type "text/javascript"
                           :src "static/js/jquery.fancybox-thumbs.js")
                  (:link :rel "stylesheet" :type "text/css" :media "screen"
                         :href "static/css/gallery.css"))
           (:body (:div :id "controls")
                  (:script :language "javascript" :type "text/javascript"
                           :src "static/js/run-gallery.js")
                  (:center (:a :href (restas:genurl 'add-pic)
                               "add a picture"))
                  (:br)
                  (loop for (pic thumb title comment) in *pictures* do
                       (htm (:div :class "img" 
                                  (:a :href pic :rel "group" :class "fancybox-thumb" :title title
                                      (:img :src thumb))
                                  (:div :class "desc" (:b (str title))
                                        (:br) (str comment)))))))))
