(defpackage #:gallery.pics-memory-collection
  (:use #:cl #:gallery.content
        #:gallery.policy.pics-collection)
  (:export #:handler
           #:make))

(in-package #:gallery.pics-memory-collection)

(defclass handler ()
  ((items
    :initform (list (make-root-album "" ""))
    :initarg :items
    :accessor items
    :documentation
    "A flat collection of all items in the memory")))

(defun make (root-title root-comment)
  (make-instance 'handler :items (list (make-root-album root-title root-comment))))

(defmethod p-coll.get-item ((mem handler) id)
  (find id (items mem) :key #'item-id :test #'=))

(defmethod p-coll.save-pictures ((mem handler) pics father-id)
  (let ((father (p-coll.get-item mem father-id)))
    (when father
      (setf (album-items father) (append pics (album-items father)))
      (setf (items mem) (nconc pics (items mem))))))

(defmethod p-coll.save-albume ((mem handler) album father-id)
  (let ((father (p-coll.get-item mem father-id)))
    (when father
      (push album (album-items father))
      (push album (items mem)))))

(defmethod p-coll.root-album-id ((mem handler))
  (item-id (first (items mem))))