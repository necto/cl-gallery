
(restas:define-policy render
  (:interface-package #:gallery.policy.render)
  (:interface-method-template "THEME.~A")
  (:internal-package #:gallery.internal.render)
  (:internal-function-template "~A-RENDER")
  
  (define-method add-pic (form album)
    "Draw a page with form for a picture addendum")
  (define-method add-album (form)
    "Draw a page for new album form")
  (define-method album-list (add-album-url albums)
    "Draw a list of all albums")
  (define-method view-album (add-pic-url album)
    "Draw all pictures in the album"))

(restas:define-module #:gallery
    (:use :cl :files-locator :gallery.content
          :gallery.internal.render)
  (:export #:main
           #:add-pic
           #:receive-pic
           #:receive-album
           #:add-album
           #:view-album

           #:static.route

           #:*drawer*
           #:*store*))

(in-package #:gallery)

(restas:mount-module files (#:restas.directory-publisher)
  (:inherit-parent-context t)
  (:url "files")
  (restas.directory-publisher:*directory* #p"/tmp/")
  (restas.directory-publisher:*autoindex* t))

(restas:mount-module static (#:restas.directory-publisher)
  (:inherit-parent-context t)
  (:url "static")
  (restas.directory-publisher:*directory*
   (asdf:system-relative-pathname '#:gallery #p"static/")))

(defparameter *current-files* nil)
(defparameter *albums* nil)

(defvar *store* (make-instance 'files-store :upload-dir "/tmp/" :download-dir "wrong"))

(defmethod restas:initialize-module-instance :after ((module (eql #.*package*)) context)
  (restas:with-context context
    (setf (download-dir *store*) (restas:genurl 'files.route :path ""))))

(restas:mount-module upl (#:upload)
  (:url "upload")
  (:inherit-parent-context t)
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

(defun upload-form ()
  (restas::with-module (restas:find-submodule 'upl)
    (upload:form (restas:genurl 'upl.upload-file)
                 (restas:genurl 'upl.upload-empty-url))))

(restas:define-route add-pic ("add")
  (let ((album (hunchentoot:get-parameter "album")))
    (add-pic-render (upload-form)
                    album)))

(restas:define-route add-album ("new-album")
  (add-album-render (upload-form)))

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

(restas:define-route main ("")
  (album-list-render (restas:genurl 'add-album) *albums*))

(restas:define-route view-album ("album/:name")
  (let ((album (get-album name)))
    (if album
        (view-album-render (restas:genurl 'add-pic :album (album-name album)) album)
        (format nil "There is no album with name ~a." name))))
