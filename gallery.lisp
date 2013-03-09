
(restas:define-module #:gallery
    (:use :cl :cl-who :files-locator :gal-content)
  (:export #:main
           #:add-pic
           #:receive-pic
           #:receive-album
           #:add-album
           #:view-album
           
           #:add-pic-render
           #:add-album-render
           #:album-list-render
           #:view-album-render

           #:*drawer*
           #:*store*))

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

(defclass default-drawer () ())

(defvar *drawer* (make-instance 'default-drawer))
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

(defgeneric add-pic-render (drawer form album))
(defgeneric add-album-render (drawer form) )
(defgeneric album-list-render (drawer add-album-url albums))
(defgeneric view-album-render (drawer add-pic-url album))

(restas:define-route add-pic ("add")
  (let ((album (hunchentoot:get-parameter "album")))
    (add-pic-render *drawer*
                    (restas:with-context (second (gethash 'upl *submodules*))
                      (upload:form (restas:genurl-submodule
                                    'upl 'upload:upload-file)
                                   (restas:genurl-submodule
                                    'upl 'upload:upload-empty-url)))
                    album)))

(restas:define-route add-album ("new-album")
  (add-album-render *drawer*
                    (restas:with-context (second (gethash 'upl *submodules*))
                         (upload:form (restas:genurl-submodule
                                       'upl 'upload:upload-file)
                                      (restas:genurl-submodule
                                       'upl 'upload:upload-empty-url)))))

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
  (album-list-render *drawer* (restas:genurl 'add-album) *albums*))

(restas:define-route view-album ("album/:name")
  (let ((album (get-album name)))
    (if album
        (view-album-render *drawer* (restas:genurl 'add-pic :album (album-name album)) album)
        (format nil "There is no album with name ~a." name))))

