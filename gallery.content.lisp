(defpackage :gallery.content
  (:use :cl-user :cl :files-locator :transliterate
        :gallery.internal.pics-collection)
  (:export #:item
           #:item-id
           #:item-thumbnail
           #:item-title
           #:item-comment
           #:draw-preview

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

(defclass item ()
  ((id
    :initarg :id
    :reader item-id
    :documentation
    "An unique identificator for the item")
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
    "The commentary to the content."))
  (:documentation
   "A general item, representing a gallery-managed item"))

(defclass picture (item)
  ((url
    :initarg :url
    :reader pic-url
    :documentation
    "The address of the actual full-size content")))

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
    "The collection of all items, contained in the album")))

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

(defun make-picture (store file title comment)
  (make-instance 'picture
                 :id (gen-uniq-id-pic-coll)
                 :url (file-url store file)
                 :thumbnail (file-url store (make-thumb store file))
                 :title title
                 :comment comment))

(defun make-album-name (title)
  (transliterate (string-downcase (string-trim " " title))))

(defun make-album (store file title comment)
  (make-instance 'album
                 :id (gen-uniq-id-pic-coll)
                 :name (make-album-name title)
                 :title title
                 :comment comment
                 :thumbnail (file-url store (make-thumb store file))))

(defun make-root-album (title comment)
  (make-instance 'album
                 :id (gen-uniq-id-pic-coll)
                 :name (make-album-name title)
                 :title title
                 :comment comment
                 :thumbnail nil))

(defun album-delete-items (album ids)
  (setf (album-items album)
        (remove-if #'(lambda (item)
                       (find (item-id item) ids :test #'equal))
                   (album-items album))))
