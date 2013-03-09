
(restas:define-module #:gallery
    (:use :cl :cl-who :files-locator :gal-content)
  (:export #:main
           #:add-pic
           #:receive-pic
           #:receive-album
           #:add-album
           #:view-album))

(in-package #:gallery)

(restas:mount-submodule files (#:restas.directory-publisher)
  (restas.directory-publisher:*directory* #p"/tmp/")
  (restas.directory-publisher:*baseurl* '("files"))
  (restas.directory-publisher:*autoindex* t))

(restas:mount-submodule static (#:restas.directory-publisher)
  (restas.directory-publisher:*directory* #p"./static/")
  (restas.directory-publisher:*baseurl* '("static")))

(defparameter *current-files* nil)

(defparameter *albums* nil)

(defvar *store* (make-instance 'files-store :upload-dir "/tmp/" :download-dir "/gal/files/"))

(defmethod restas:initialize-module-instance :before ((module (eql #.*package*)) context)
  (restas:with-context context
    (setf *store* (make-instance 'files-store :upload-dir "/tmp/"
                                 :download-dir (format nil "/~a/~a" (car *baseurl*) "files/")))))

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

(defun get-album (name)
  (find name *albums* :key #'album-name :test #'string=))

(restas:define-route add-pic ("add")
  (let ((album (hunchentoot:get-parameter "album")))
    (with-html-output-to-string (sss nil :prologue t :indent t)
      (:html (:head (:script :language "javascript" :type "text/javascript"
                             :src "static/js/preview-updater.js"))
             (:body (str (restas:with-context (second (gethash 'upl *submodules*))
                           (upload:form (restas:genurl-submodule
                                         'upl 'upload:upload-file)
                                        (restas:genurl-submodule
                                         'upl 'upload:upload-empty-url))))
                    (:form :method "get" :action (restas:genurl 'receive-pic)
                           "Title:" (:input :type "text" :name "title" :value "")
                           "Comment:" (:input :type "text" :name "comment" :value "")
                           "Album:" (:input :type "text" :name "album" :value album :readonly t)
                           (:input :type "hidden" :name "pic" :value "no-value" :id "pic")
                           (:input :type "submit" :value "like it!"))
                    (:div :id "preview"))))))

(restas:define-route add-album ("new-album")
  (with-html-output-to-string (sss nil :prologue t :indent t)
    (:html (:head (:script :language "javascript" :type "text/javascript"
                           :src "static/js/preview-updater.js"))
           (:body (str (restas:with-context (second (gethash 'upl *submodules*))
                         (upload:form (restas:genurl-submodule
                                       'upl 'upload:upload-file)
                                      (restas:genurl-submodule
                                       'upl 'upload:upload-empty-url))))
                  (:form :method "get" :action (restas:genurl 'receive-album)
                         "Title:" (:input :type "text" :name "title" :value "")
                         "Comment:" (:input :type "text" :name "comment" :value "")
                         (:input :type "hidden" :name "pic" :value "no-value" :id "pic")
                         (:input :type "submit" :value "That's right!"))
                  (:div :id "preview")))))

(defun get-uploaded-pictures (param-name)
  (setf *current-files* nil)
  (with-input-from-string (files-param (hunchentoot:get-parameter param-name))
    (read files-param)))

(restas:define-route receive-pic ("likeit")
  (let ((files (get-uploaded-pictures "pic"))
        (title (hunchentoot:get-parameter "title"))
        (comment (hunchentoot:get-parameter "comment"))
        (album-name (hunchentoot:get-parameter "album")))
    (let ((pics (mapcar #'(lambda (file)
                            (make-picture *store* file title comment))
                        files))
          (album (get-album album-name)))
      (if album
          (progn
            (setf (album-items album) (nconc pics (album-items album)))
            (restas:redirect 'view-album :name album-name))
          (format nil "album ~a not found" album-name)))))

(restas:define-route receive-album ("thatsright")
  (let ((files (get-uploaded-pictures "pic"))
        (title (hunchentoot:get-parameter "title"))
        (comment (hunchentoot:get-parameter "comment")))
    (push (make-album *store* (first files) title comment)
          *albums*)
    (restas:redirect 'main)))

(defun gen-static-url (path)
  (restas:genurl-submodule 'static 'restas.directory-publisher:route
                           :path path))

(restas:define-route main ("")
  (with-html-output-to-string (stream nil :prologue t :indent t)
    (:html (:head (:link :rel "stylesheet" :type "text/css" :media "screen"
                         :href (gen-static-url "css/gallery.css")))
           (:body (:center (:a :href (restas:genurl 'add-album)
                           "new album"))
                  (:br)
                  (loop for album in *albums* do
                       (draw-preview album stream))))))

(restas:define-route view-album ("album/:name")
  (let ((album (get-album name)))
    (if album
        (with-html-output-to-string (stream nil :prologue t :indent t)
          (:html (:head (:script :language "javascript" :type "text/javascript"
                           :src "http://code.jquery.com/jquery-1.9.1.min.js")
                  (:script :language "javascript" :type "text/javascript"
                           :src (gen-static-url "js/jquery.mousewheel-3.0.6.pack.js"))
                  (:link :rel "stylesheet" :type "text/css" :media "screen"
                         :href (gen-static-url "css/jquery.fancybox.css"))
                  (:script :language "javascript" :type "text/javascript"
                           :src (gen-static-url "js/jquery.fancybox.pack.js"))
                  (:link :rel "stylesheet" :type "text/css" :media "screen"
                         :href (gen-static-url "css/jquery.fancybox-thumbs.css"))
                  (:script :language "javascript" :type "text/javascript"
                           :src (gen-static-url "js/jquery.fancybox-thumbs.js"))
                  (:link :rel "stylesheet" :type "text/css" :media "screen"
                         :href (gen-static-url "css/gallery.css")))
                 (:body (:script :language "javascript" :type "text/javascript"
                                 :src (gen-static-url "js/run-gallery.js"))
                        (:h1 (str (item-title album)))
                        (:p (str (item-comment album)))
                        (:center (:a :href (restas:genurl 'add-pic :album (album-name album))
                                     "add a picture"))
                        (:br)
                        (loop for pic in (album-items album) do
                             (draw-preview pic stream)))))
        (format nil "There is no album with name ~a." name))))
