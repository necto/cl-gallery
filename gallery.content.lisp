(defpackage :gallery.content
  (:use :cl-user :cl :files-locator :transliterate
        :gallery.internal.pics-collection)
  (:export #:period
           #:period-begin
           #:period-end
           #:period-contains-p
           #:adjust-album-period
           #:make-embracing-period

           #:item
           #:item-id
           #:item-owner-id
           #:item-thumbnail
           #:item-title
           #:item-comment
           #:item-time

           #:picture
           #:make-picture
           #:pic-url
           
           #:album
           #:make-album
           #:make-root-album
           #:album-name
           #:album-items
           #:album-delete-items))

(in-package :gallery.content)

(defclass period ()
  ((begin
    :initarg :begin
    :initform (local-time:now)
    :type local-time:timestamp
    :reader period-begin
    :documentation
    "The least moment in the time period")
   (end
    :initarg :end
    :initform (local-time:now)
    :type local-time:timestamp
    :reader period-end
    :documentation
    "The most moment in the time period"))
  (:documentation
   "A time period - a segment of time. begin is timestamp< than the end."))

(defgeneric find-newest-moment (t1 other-times)
  (:documentation "Get the most recent moment of n+1 times or time periods"))
(defgeneric find-oldest-moment (t1 other-times)
  (:documentation "Get the most ancient moment of n+1 times or time periods"))
(defgeneric period-contains-one-p (p time)
  (:documentation "Check whether the period p contains the time"))

(defun make-embracing-period (times)
  "Make the shortest period, contatining t1 and t2"
  (make-instance 'period :begin (find-oldest-moment (car times) (cdr times))
                 :end (find-newest-moment (car times) (cdr times))))

(defun period-contains-p (p &rest times)
  "Check whether the period p contains all the given times"
  (if (null times)
      t
      (every #'(lambda (time) (period-contains-one-p p time)) times)))           

(defmethod find-newest-moment ((t1 period) (other-times list))
  (find-newest-moment (period-end t1) other-times))
(defmethod find-newest-moment ((t1 local-time:timestamp) (other-times list))
  (if (null other-times)
      t1
      (let ((t2 (find-newest-moment (car other-times) (cdr other-times))))
        (if (local-time:timestamp< t1 t2) t2 t1))))

(defmethod find-oldest-moment ((t1 period) (other-times list))
  (find-oldest-moment (period-begin t1) other-times))
(defmethod find-oldest-moment ((t1 local-time:timestamp) (other-times list))
  (if (null other-times)
      t1
      (let ((t2 (find-oldest-moment (car other-times) (cdr other-times))))
        (if (local-time:timestamp< t1 t2) t1 t2))))

(defmethod period-contains-one-p ((p period) (time period))
  (and (period-contains-one-p p (period-begin time))
       (period-contains-one-p p (period-end time))))
(defmethod period-contains-one-p ((p period) (time local-time:timestamp))
  (and (local-time:timestamp< (period-begin p) time)
       (local-time:timestamp< time (period-end p))))

(defclass item ()
  ((id
    :initarg :id
    :reader item-id
    :documentation
    "An unique identificator for the item")
   (owner-id
    :initarg :owner-id
    :reader item-owner-id
    :documentation
    "An id of the container, owning the given item")
   (thumbnail
    :initarg :thumbnail
    :reader item-thumbnail
    :documentation
    "The url to the small preview picture")
   (title
    :initarg :title
    :accessor item-title)
   (comment
    :initarg :comment
    :accessor item-comment
    :documentation
    "The commentary to the content.")
   (time
    :initarg :time
    :reader item-time
    :documentation
    "A time moment or period, when the item took place"))
  (:documentation
   "A general item, representing a gallery-managed item"))

(defclass picture (item)
  ((url
    :initarg :url
    :reader pic-url
    :documentation
    "The address of the actual full-size content")
   (time
    :type local-time:timestamp)))

(defclass album (item)
  ((name
    :initarg :name
    :reader album-name
    :documentation
    "The uniq string, used to designate the album among others.")
   (items
    :initform nil
    :initarg :items
    :accessor album-items
    :documentation
    "The collection of all items, contained in the album")
   (time
    :type period)))

(defun gen-small-pic-fname (fname)
  (format nil "~a.thumb.~a" (subseq fname 0 (- (length fname) 4))
          (subseq fname (- (length fname) 3) (length fname))));reattach the extension

(defun make-thumb (store fname)
  (let ((small-fname (gen-small-pic-fname fname)))
    (sb-ext:run-program "/usr/bin/convert" 
                        (list "-scale" "100x100" 
                              (file-pathname store fname)
                              (file-pathname store small-fname))
                        :wait t)
    small-fname))

(defun make-picture (store owner-id file title comment date)
  (make-instance 'picture
                 :id (gen-uniq-id-pic-coll)
                 :owner-id owner-id
                 :url (file-url store file)
                 :thumbnail (file-url store (make-thumb store file))
                 :title title
                 :comment comment
                 :time date))

(defun make-album-name (title)
  (transliterate (string-downcase (string-trim " " title))))

(defun make-album (store owner-id file title comment period)
  (make-instance 'album
                 :id (gen-uniq-id-pic-coll)
                 :owner-id owner-id
                 :name (make-album-name title)
                 :title title
                 :comment comment
                 :thumbnail (file-url store (make-thumb store file))
                 :time period))

(defun make-root-album (title comment)
  (make-instance 'album
                 :id (gen-uniq-id-pic-coll)
                 :owner-id nil
                 :name (make-album-name title)
                 :title title
                 :comment comment
                 :thumbnail nil
                 :time (make-instance 'period)))

;; TODO: if a time of any item is ajacent to the album period border,
;; recalculate album perioid
(defun album-delete-items (album ids)
  (setf (album-items album)
        (remove-if #'(lambda (item)
                       (find (item-id item) ids :test #'equal))
                   (album-items album))))

(defmethod find-oldest-moment ((i item) other-times)
  (find-oldest-moment (item-time i) other-times))
(defmethod find-newest-moment ((i item) other-times)
  (find-newest-moment (item-time i) other-times))

(defmethod find-oldest-moment ((i string) other-times)
  (find-oldest-moment (local-time:parse-timestring i) other-times))
(defmethod find-newest-moment ((i string) other-times)
  (find-newest-moment (local-time:parse-timestring i) other-times))

(defun adjust-direct-album-period (album period)
  (setf (slot-value album 'time)
        (make-embracing-period (list (slot-value album 'time) period))))

(defun adjust-album-period (item-updater album period)
  (unless (period-contains-p (item-time album) period)
    (adjust-direct-album-period album period)
    (funcall item-updater album)))

