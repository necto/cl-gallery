;;TODO: provide default thumbnails for albums without the ones.

(restas:define-module #:gallery
    (:use :cl :files-locator :gallery.content
          :gallery.internal.render
          :gallery.internal.pics-collection)
  (:export #:main
           #:add-pic
           #:receive-pic
           #:receive-album
           #:add-album
           #:view-album
           #:choose-pic
           #:delete-pic
           #:update-item
           
           #:albums-grid
           #:album-pics-grid

           #:static.route

           #:*drawer*
           #:*store*

           #:*extra-params*))

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

(defparameter *extra-params* nil)
(defparameter *current-files* nil)

(defvar *store* (make-instance 'files-store :upload-dir "/tmp/" :download-dir "wrong"))

(defmethod restas:initialize-module-instance :after ((module (eql #.*package*)) context)
  (restas:with-context context
    (setf (download-dir *store*) (restas:genurl 'files.route :path ""))))

(restas:mount-module upl (#:upload)
  (:url "upload")
  (:inherit-parent-context t)
  (upload:*store* *store*)
  (upload:*mime-type* nil)
  (upload:*file-stored-callback*
   #'files-stored-callback))

(defun files-stored-callback (files)
  (when *current-files*
    (mapcar #'(lambda (file)
                (delete-file (file-path *store* file)))
            *current-files*))
  (setf *current-files* files)
  (let ((*print-pretty* nil))
    (format nil "parent.done([~:{{url: \"~a\",~
                                  file: \"~a\",~
                                  date: \"~a\"}~^, ~}]);"
            (mapcar #'(lambda (file)
                        (list (file-url *store* file) file (local-time:now)))
                    files))))

(defun safe-parse-integer (str)
  (let ((int (parse-integer str :junk-allowed t)))
    (if int int 0)))

(defun upload-form (multiple)
  (restas:assert-native-module)
  (restas:in-submodule 'upl
    (upload:form :multiple multiple)))

(restas:define-route add-pic ("add")
  (let ((father (hunchentoot:get-parameter "father"))
        (father-name (hunchentoot:get-parameter "father-name")))
    (add-pic-render (upload-form t)
                    father
                    father-name)))

(restas:define-route add-album ("new-album")
  (let ((father (hunchentoot:get-parameter "father"))
        (father-name (hunchentoot:get-parameter "father-name")))
    (add-album-render (upload-form nil) father father-name )))

;; Parse a list, transmitted through the url get-parameter,
;; named param-name
(defun get-list-param (param-name)
  (with-input-from-string (lst (hunchentoot:get-parameter param-name))
    (read lst)))

;; Get a list of uploaded files, given by the url get-parameter,
;; named param-name
(defun get-uploaded-pictures (param-name)
  (setf *current-files* nil)
  (get-list-param param-name))

;; Make a list of pictures using the same title and comment from
;; a list of just raw files.
(defun make-pictures (files owner titles comments dates)
  (mapcar #'(lambda (file title comment date)
              (make-picture *store* owner file title comment date))
          files titles comments dates))

(restas:define-route receive-pic ("accept-pic")
  (let ((files (get-uploaded-pictures "pic"))
        (titles (get-list-param "title"))
        (comments (get-list-param "comment"))
        (dates (mapcar #'local-time:parse-timestring
                       (get-list-param "time")))
        (father-id (safe-parse-integer (hunchentoot:get-parameter "father"))))
    (if (save-pictures-pic-coll (make-pictures files father-id
                                               titles comments dates)
                                father-id)
        (restas:redirect 'view-album :id father-id)
        (no-such-album-render father-id))))

(restas:define-route receive-album ("accept-album")
  (let ((files (get-uploaded-pictures "pic"))
        (titles (get-list-param "title"))
        (comments (get-list-param "comment"))
        (father-id (safe-parse-integer (hunchentoot:get-parameter "father"))))
    (if (save-album-pic-coll 
         (make-album *store* father-id (first files)
                     (first titles) (first comments)
                     (item-time (get-item-pic-coll father-id)))
         father-id)
        (restas:redirect 'view-album :id father-id)
        (no-such-album-render father-id))))
          
(restas:define-route main ("")
  (restas:redirect 'view-album :id (root-album-id-pic-coll)))

(restas:define-route view-album ("album/:id")
  (:sift-variables (id #'safe-parse-integer))
  (let ((album (get-item-pic-coll id)))
    (if album
        (view-album-render (restas:genurl 'add-pic :father (item-id album)
                                          :father-name (album-name album))
                           (restas:genurl 'add-album :father (item-id album)
                                          :father-name (album-name album))
                           (restas:genurl 'choose-pic :id id
                                          :action (restas:genurl 'delete-pic :id id))
                           album)
        (no-such-album-render id))))

(restas:define-route choose-pic ("album/choose/:id")
  (:sift-variables (id #'safe-parse-integer))
  (let ((album (get-item-pic-coll id))
        (action (hunchentoot:get-parameter "action")))
    (if album
        (choose-picture-render action album)
        (no-such-album-render id))))

(defun get-parameter-values (name)
  (mapcar #'cdr
          (remove name (hunchentoot:get-parameters*)
                  :test (complement #'equal)
                  :key #'car)))

(restas:define-route delete-pic ("album/delete/:id" :method :get)
  (:sift-variables (id #'safe-parse-integer))
  (let ((pics (get-parameter-values "chosen"))
        (album (get-item-pic-coll id)))
    (when album
        (album-delete-items album (mapcar #'parse-integer pics))
        (update-item-pic-coll album))
    (restas:redirect 'view-album :id id)))

(restas:define-route update-item ("update/:id" :method :get)
  (:sift-variables (id #'safe-parse-integer))
  (let ((item (get-item-pic-coll id))
        (new-title (hunchentoot:get-parameter "title"))
        (new-comment (hunchentoot:get-parameter "comment")))
    (when item
      (setf (item-title item) new-title
            (item-comment item) new-comment)
      (update-item-pic-coll item))
    (preview-render item nil)))
    

(defun album-pics-grid (album &optional (chkbox nil))
  (restas:assert-native-module)
  (pics-grid-render album chkbox))

(defun draw-preview (content &optional (chkbox nil))
  (preview-render content chkbox))

