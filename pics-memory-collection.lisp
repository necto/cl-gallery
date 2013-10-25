(defpackage #:gallery.pics-memory-collection
  (:use #:cl #:gallery.content
        #:gallery.policy.pics-collection)
  (:export #:handler
           #:make))

(in-package #:gallery.pics-memory-collection)

(defclass handler ()
  ((items
    :initform nil
    :initarg :items
    :accessor items
    :documentation
    "A flat collection of all items in the memory")))

(defun make ()
  (make-instance 'handler ))

(defmethod p-coll.get-item ((mem handler) id)
  (find id (items mem) :key #'item-id :test #'=))

(defmethod p-coll.save-pictures ((mem handler) pics father-id)
  (let ((father (p-coll.get-item mem father-id)))
    (when father
      (setf (album-items father) (append pics (album-items father)))
      (setf (items mem) (nconc pics (items mem))))))

(defmethod p-coll.save-album ((mem handler) album father-id)
  (let ((father (p-coll.get-item mem father-id)))
    (when father
      (push album (album-items father))
      (push album (items mem)))))

(defmethod p-coll.update-album ((mem handler) album)
  ;Nothing to do, the album is allready in memory, so it updates automatically
  )
(defmethod p-coll.update-item ((mem handler) item)
  ;Nothing to do
  )

(let ((counter 0))
  (defmethod p-coll.gen-uniq-id ((mem handler))
    (incf counter)))

(defmethod p-coll.root-album-id ((mem handler))
  (item-id 
   (first (if (null (items mem))
              (push (make-root-album "Hi, bro..." "I'm here the root")
                    (items mem))
              (items mem)))))
  