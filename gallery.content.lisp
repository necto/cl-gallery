(defpackage :gallery.content
  (:use :cl-user :cl :cl-who :files-locator :transliterate)
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
           #:album-name
           #:album-items
           #:album-delete-items
           #:album-find-item))


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
    :reader item-title)
   (comment
    :initarg :comment
    :reader item-comment
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
    :accessor album-items
    :documentation
    "The collection of all items, contained in the album")))

(defgeneric draw-preview-impl (content chkbox stream)
  (:documentation "draw a small preview composition.
   The chkbox is the name of checkbox group, if nil - no checkbox"))

(defmethod draw-preview-impl ((content item) chkbox stream)
  (error "You must redefine draw-preview in order to display your content in a gallery"))

(defmethod draw-preview-impl ((content picture) chkbox stream)
  (with-accessors ((url pic-url) (thumbnail item-thumbnail)
                   (title item-title) (comment item-comment)
                   (id item-id)) content
    (with-html-output (sss stream)
      (:div :class "img" 
            (:a :href url :rel "group" :class "fancybox-thumb" :title title
                (:img :src thumbnail))
            (:div :class "desc" (:b (str title))
                  (:br) (str comment))
            (when chkbox
              (htm (:input :type "checkbox" :name chkbox :value id)))))))

(defmethod draw-preview-impl ((content album) chkbox stream)
  (with-accessors ((name album-name) (thumbnail item-thumbnail)
                   (title item-title) (comment item-comment)
                   (items album-items) (id item-id))
      content
    (let ((url (format nil "album/~a" name)))
      (with-html-output (sss stream)
        (:div :class "img"
              (:a :href url :title title
                  (:img :src thumbnail))
              (:div :class "desc" (:b (str title))
                    (:br) (str comment))
              (when chkbox
                (htm (:input :type "checkbox" :name chkbox :value id))))))))

(defun draw-preview (content &optional (chkbox nil) (stream nil))
  (if stream
      (draw-preview-impl content chkbox stream)
      (with-html-output-to-string (stream nil :prologue nil :indent t)
        (draw-preview-impl content chkbox stream))))

(let ((counter 0))
  (defun generate-next-id ()
    (incf counter)))

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
                 :id (generate-next-id)
                 :url (file-url store file)
                 :thumbnail (file-url store (make-thumb store file))
                 :title title
                 :comment comment))

(defun make-album-name (title)
  (transliterate (string-downcase (string-trim " " title))))

(defun make-album (store file title comment)
  (make-instance 'album
                 :id (generate-next-id)
                 :name (make-album-name title)
                 :title title
                 :comment comment
                 :thumbnail (file-url store (make-thumb store file))))

(defun album-find-item (album item-id)
  (let ((items (album-items album)))
    (find item-id items :test #'equal :key #'item-id)))

(defun album-delete-items (album ids)
  (setf (album-items album)
        (remove-if #'(lambda (item)
                       (find (item-id item) ids :test #'equal))
                   (album-items album))))
