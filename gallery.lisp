
(restas:define-policy render
  (:interface-package #:gallery.policy.render)
  (:interface-method-template "THEME.~A")
  (:internal-package #:gallery.internal.render)
  (:internal-function-template "~A-RENDER")
  
  (define-method add-pic (form album album-name)
    "Draw a page with form for a picture addendum")
  (define-method add-album (form father father-name)
    "Draw a page for new album form")
  (define-method view-album (add-pic-url add-alb-url rem-pic-url album)
    "Draw all pictures in the album")
  (define-method choose-album (action albums)
    "Show the table with checkboxes for user to choose some albums")
  (define-method choose-picture (action album)
    "Show the current album for user to choose some pictures from it")
  (define-method no-such-album (id)
    "Show the not found message fro the album named name")
  (define-method pics-grid (album chkbox)
    "Draw all pictures from the given album, and supply them by
     the checkbox if given.")
  (define-method preview (content chkbox)
  "draw a small preview composition.
   The chkbox is the name of checkbox group, if nil - no checkbox"))

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
           #:delete-pic
           
           #:albums-grid
           #:album-pics-grid

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
(defparameter *items* (list (make-root-album "Hi, bro." "root album")))
(defparameter *root-album-id* (item-id (first *items*)))

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

(defun safe-parse-integer (str)
  (let ((int (parse-integer str :junk-allowed t)))
    (if int int 0)))

(defun get-item (id)
  (find id *items* :key #'item-id :test #'=))

;; Persist a list of pictures pics in album with id father-id.
;; Returns nil if father is absent, and non-nil otherwise.
(defun save-pictures (pics father-id)
  (let ((father (get-item father-id)))
    (when father
      (setf (album-items father) (append pics (album-items father)))
      (setf *items* (nconc pics *items*)))))

;; Persist the album from the album with id father-id.
;; Return nil if father is not found, and non-nil otherwise.
(defun save-album (album father-id)
  (let ((father (get-item father-id)))
    (when father
      (push album (album-items father))
      (push album *items*))))

(defun upload-form ()
  (restas:assert-native-module)
  (restas:in-submodule 'upl
    (upload:form)))

(restas:define-route add-pic ("add")
  (let ((father (hunchentoot:get-parameter "father"))
        (father-name (hunchentoot:get-parameter "father-name")))
    (add-pic-render (upload-form)
                    father
                    father-name)))

(restas:define-route add-album ("new-album")
  (let ((father (hunchentoot:get-parameter "father"))
        (father-name (hunchentoot:get-parameter "father-name")))
    (add-album-render (upload-form) father father-name )))

;; Get a list of uploaded files, given by the url get-parameter,
;; named param-name
(defun get-uploaded-pictures (param-name)
  (setf *current-files* nil)
  (with-input-from-string (files-param (hunchentoot:get-parameter param-name))
    (read files-param)))

;; Make a list of pictures using the same title and comment from
;; a list of just raw files.
(defun make-pictures (files title comment)
  (mapcar #'(lambda (file)
              (make-picture *store* file title comment))
          files))

(restas:define-route receive-pic ("accept-pic")
  (let ((files (get-uploaded-pictures "pic"))
        (title (hunchentoot:get-parameter "title"))
        (comment (hunchentoot:get-parameter "comment"))
        (father-id (safe-parse-integer (hunchentoot:get-parameter "father"))))
    (if (save-pictures (make-pictures files title comment) father-id)
        (restas:redirect 'view-album :id father-id)
        (format nil "album ~a not found" father-id))))

(restas:define-route receive-album ("accept-album")
  (let ((files (get-uploaded-pictures "pic"))
        (title (hunchentoot:get-parameter "title"))
        (comment (hunchentoot:get-parameter "comment"))
        (father-id (safe-parse-integer (hunchentoot:get-parameter "father"))))
    (if (save-album (make-album *store* (first files) title comment) father-id)
        (restas:redirect 'view-album :id father-id)
        (format nil "album ~a not found" father-id))))
          
(restas:define-route main ("")
  (restas:redirect 'view-album :id *root-album-id*))

(restas:define-route view-album ("album/:id")
  (:sift-variables (id #'safe-parse-integer))
  (let ((album (get-item id)))
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
  (let ((album (get-item id))
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
        (album (get-item id)))
    (if album
        (album-delete-items album (mapcar #'parse-integer pics)))
    (restas:redirect 'view-album :id id)))

(defun album-pics-grid (album &optional (chkbox nil))
  (restas:assert-native-module)
  (pics-grid-render album chkbox))

(defun draw-preview (content &optional (chkbox nil))
  (preview-render content chkbox))

