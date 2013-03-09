(defpackage :gal-content
  (:use :cl-user :cl :cl-who :files-locator)
  (:export #:item
           #:draw-preview

           #:picture
           #:make-picture
           #:pic-url
           #:pic-thumbnail
           #:pic-title
           #:pic-comment))

(in-package :gal-content)

(defclass item () ()
  (:documentation
   "A general item, representing a gallery-managed item"))

(defclass picture (item)
  ((url
    :initarg :url
    :reader pic-url
    :documentation
    "The address of the actual full-size content")
   (thumbnail
    :initarg :thumbnail
    :reader pic-thumbnail
    :documentation
    "The url to the small preview picture")
   (title
    :initarg :title
    :reader pic-title)
   (comment
    :initarg :comment
    :reader pic-comment
    :documentation
    "The commentary to the content.")))

(defgeneric draw-preview (content stream)
  (:documentation "draw a small preview composition"))

(defmethod draw-preview ((content item) stream)
  (error "You must redefine draw-preview in order to display your content in a gallery"))

(defmethod draw-preview ((content picture) stream)
  (with-accessors ((url pic-url) (thumbnail pic-thumbnail)
                   (title pic-title) (comment pic-comment)) content
    (cl-who:with-html-output (sss stream)
      (:div :class "img" 
            (:a :href url :rel "group" :class "fancybox-thumb" :title title
                (:img :src thumbnail))
            (:div :class "desc" (:b (str title))
                  (:br) (str comment))))))

(defun gen-small-pic-fname (fname)
  (format nil "~a.thumb.~a" (subseq fname 0 (- (length fname) 4))
          (subseq fname (- (length fname) 3) (length fname))))

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
                 :url (file-url store file)
                 :thumbnail (file-url store (make-thumb store file))
                 :title title
                 :comment comment))
