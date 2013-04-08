
(restas:define-policy render
  (:interface-package #:gallery.policy.render)
  (:interface-method-template "THEME.~A")
  (:internal-package #:gallery.internal.render)
  (:internal-function-template "~A-RENDER")
  
  (define-method add-pic (form album)
    "Draw a page with form for a picture addendum")
  (define-method add-album (form)
    "Draw a page for new album form")
  (define-method album-list (add-album-url rem-album-url albums)
    "Draw a list of all albums")
  (define-method view-album (add-pic-url rem-pic-url album)
    "Draw all pictures in the album")
  (define-method choose-album (action albums)
    "Show the table with checkboxes for user to choose some albums")
  (define-method choose-picture (action album)
    "Show the current album for user to choose some pictures from it")
  (define-method no-such-album (name)
    "Show the not found message fro the album named name"))

(restas:define-module #:gallery
    (:use :cl :files-locator :gallery.content
          :gallery.internal.render)
  (:export #:main
           #:add-pic
           #:receive-pic
           #:receive-album
           #:add-album
           #:view-album
           #:choose-pic
           #:choose-album
           #:delete-pic
           #:delete-album

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
(defparameter *items* nil)

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

(restas:define-route receive-pic ("accept-pic")
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
            (setf (album-items album) (append pics (album-items album)))
            (setf *items* (nconc pics *items*))
            (restas:redirect 'view-album :name album-name))
          (format nil "album ~a not found" album-name)))))

(restas:define-route receive-album ("accept-album")
  (let ((files (get-uploaded-pictures "pic"))
        (title (hunchentoot:get-parameter "title"))
        (comment (hunchentoot:get-parameter "comment")))
    (let ((album (make-album *store* (first files) title comment)))
      (push album *albums*)
      (push album *items*))
    (restas:redirect 'main)))

(restas:define-route main ("")
  (album-list-render (restas:genurl 'add-album)
                     (restas:genurl 'choose-album
                                    :action (restas:genurl 'delete-album))
                     *albums*))

(restas:define-route view-album ("album/:name")
  (let ((album (get-album name)))
    (if album
        (view-album-render (restas:genurl 'add-pic :album (album-name album))
                           (restas:genurl 'choose-pic :name name
                                          :action (restas:genurl 'delete-pic :name name))
                           album)
        (no-such-album-render name))))

(restas:define-route choose-pic ("album/choose/:name")
  (let ((album (get-album name))
        (action (hunchentoot:get-parameter "action")))
    (if album
        (choose-picture-render action album)
        (no-such-album-render name))))

(restas:define-route choose-album ("choose")
  (let ((action (hunchentoot:get-parameter "action")))
    (choose-album-render action *albums*)))

(defun get-parameter-values (name)
  (mapcar #'cdr
          (remove name (hunchentoot:get-parameters*)
                  :test (complement #'equal)
                  :key #'car)))

(restas:define-route delete-pic ("album/delete/:name" :method :get)
  (let ((pics (get-parameter-values "chosen"))
        (album (get-album name)))
    (if album
        (album-delete-items album (mapcar #'parse-integer pics)))
    (restas:redirect 'view-album :name name)))

(restas:define-route delete-album ("delete-album")
  (let ((albums (mapcar #'parse-integer (get-parameter-values "chosen"))))
    (setf *albums* (remove-if #'(lambda (item)
                                  (find (item-id item) albums :test #'equal))
                              *albums*))
    (restas:redirect 'main)))
