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
  (let ((father (p-coll.get-item mem father-id))
        (period (make-embracing-period pics)))
    (when father
      (adjust-album-period #'(lambda (it) (p-coll.update-item mem it))
                           father period)
      (setf (album-items father) (append pics (album-items father)))
      (setf (items mem) (nconc pics (items mem))))))


(defmethod p-coll.save-album ((mem handler) album father-id)
  (let ((father (p-coll.get-item mem father-id)))
    (when father
      (adjust-album-period #'(lambda (it) (p-coll.update-item mem it))
                           father (item-time album))
      (push album (album-items father))
      (push album (items mem)))))


(defmethod p-coll.update-item ((mem handler) (item item))
  (when (item-owner-id item)
    (adjust-album-period #'(lambda (it) (p-coll.update-item mem it))
                         (p-coll.get-item mem (item-owner-id item))
                         (item-time item)))
  ;Nothing to do, the item is in memory, so it is allways up to date
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
  