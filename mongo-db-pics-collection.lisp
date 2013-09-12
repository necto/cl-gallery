(defpackage #:gallery.mongo-db-pics-collection
  (:use #:cl #:gallery.content #:son-sugar #:iterate
        #:gallery.policy.pics-collection)
  (:export #:handler
           #:make))

(in-package #:gallery.mongo-db-pics-collection)

(defclass handler ()
  ((dbspec :initarg :dbspec :initform '(:name "gallery") :reader dbspec)
   (db :accessor actual-db :initform nil
       :documentation "The actual connection to the mongo database")
   (db-open-counter :accessor db-open-counter :initform 0
                    :documentation "The counter of open - closes")))

(defun make (&rest dbspec)
  (make-instance 'handler :dbspec dbspec))

(defun open-db (db)
  (incf (db-open-counter db))
  (if (actual-db db)
      (actual-db db)
      (setf (actual-db db) (apply 'make-instance 'mongo:database (dbspec db)))))

(defun close-db (db)
  (when (> (db-open-counter db) 0)
    (decf (db-open-counter db))
    (when (<= (db-open-counter db) 0)
      (mongo:close-database (actual-db db))
      (setf (actual-db db) nil))))

(defmacro with-open-db ((db-name db) &body body)
  `(let ((,db-name (open-db ,db)))
     (unwind-protect
          (progn ,@body)
       (close-db ,db))))

(defmacro with-a-collection ((coll name db) &body body)
  (let ((base-name (gensym)))
    `(with-open-db (,base-name ,db)
       (let ((,coll (mongo:collection ,base-name ,name)))
         ,@body))))

(defmacro with-pics-collection ((name db) &body body)
  `(with-a-collection (,name "galitems" ,db) ,@body))

(defmacro with-misc-collection ((name db) &body body)
  `(with-a-collection (,name "galmisc" ,db) ,@body))

(defun init-db (db)
  (with-misc-collection (coll db)
    (mongo:insert-op coll (son "_id" "nextid" "seq" 0))
    (let ((root-album (make-root-album "Hi, \<bro\> ..." "I'm your father, Luke")))
      (mongo:insert-op coll (son "_id" "rootid" "val" (item-id root-album)))
      (with-pics-collection (pics db)
        (mongo:insert-op pics (item-to-ht root-album)))
      (item-id root-album))))

(defgeneric read-item-from-hash-table (type hash-table db)
  (:documentation "Get an item from the given hash-table "))
(defgeneric write-item-to-hash-table (item)
  (:documentation "Write all neccesary data to a hash table,
   for further recreation a copy of the item"))

(defmethod write-item-to-hash-table ((item item))
  (let ((table (make-hash-table :test 'equal)))
    (setf (gethash "_id" table) (item-id item))
    (setf (gethash "thumbnail" table) (item-thumbnail item))
    (setf (gethash "title" table) (item-title item))
    (setf (gethash "comment" table) (item-comment item))
    table))

(defmethod write-item-to-hash-table ((item picture))
  (let ((table (call-next-method)))
    (setf (gethash "url" table) (pic-url item))
    table))

(defmethod write-item-to-hash-table ((item album))
  (let ((table (call-next-method)))
    (setf (gethash "name" table) (album-name item))
    (setf (gethash "items" table) (mapcar #'item-id (album-items item)))
    table))
          

(defmethod read-item-from-hash-table ((type (eql :picture)) ht db)
  (make-instance 'picture
                 :id (gethash "_id" ht)
                 :url (gethash "url" ht)
                 :thumbnail (gethash "thumbnail" ht)
                 :title (gethash "title" ht)
                 :comment (gethash "comment" ht)))

(defmethod read-item-from-hash-table ((type (eql :album)) ht db)
  (make-instance 'album
                 :id (gethash "_id" ht)
                 :name (gethash "name" ht)
                 :title (gethash "title" ht)
                 :comment (gethash "comment" ht)
                 :thumbnail (gethash "thumbnail" ht)
                 :items (mapcar #'(lambda (id)
                                    (p-coll.get-item db id))
                                (gethash "items" ht))))

(defun item-from-ht (hash-table db)
  (when hash-table
    (let ((type (intern (string-upcase (gethash "type" hash-table)) :keyword)))
      (read-item-from-hash-table type hash-table db))))

(defun item-to-ht (item)
  (let ((table (write-item-to-hash-table item)))
    (setf (gethash "type" table) (symbol-name (type-of item)))
    table))

;; TODO: restrict the depth
(defmethod p-coll.get-item ((db handler) (id number))
  (with-pics-collection (pics db)
    (item-from-ht
     (mongo:find-one pics
                     (son "_id" id)) db)))

(defmethod p-coll.save-pictures ((db handler) pics father-id)
  (let ((father (p-coll.get-item db father-id)))
    (when father
      (setf (album-items father) (append pics (album-items father)))
      (with-pics-collection (items db)
        (iter (for pic in pics)
              (mongo:insert-op items (item-to-ht pic)))
        (mongo:update-op items (son "_id" (item-id father)) (item-to-ht father)))
      t)))

(defmethod p-coll.save-album ((db handler) album father-id)
  (let ((father (p-coll.get-item db father-id)))
    (when father
      (push album (album-items father))
      (with-pics-collection (pics db)
        (mongo:insert-op pics  (item-to-ht album))
        (mongo:update-op pics (son "_id" (item-id father)) (item-to-ht father)))
      t)))

;; Todo: check for deleted pictures from the album, and delete them?
(defmethod p-coll.update-album ((db handler) album)
  (with-pics-collection (pics db)
    (mongo:update-op pics (son "_id" (item-id album)) (item-to-ht album))))

(defmethod p-coll.gen-uniq-id ((db handler))
  (with-misc-collection (misc db)
    (let ((id (mongo:find-one misc (son "_id" "nextid"))))
      (mongo:update-op misc (son "_id" "nextid") (son "seq" (incf (gethash "seq" id))))
      (gethash "seq" id))))

(defmethod p-coll.root-album-id ((db handler))
  (let ((root-id (with-misc-collection (misc db)
                   (mongo:find-one misc (son "_id" "rootid")))))
    (if root-id
        (gethash "val" root-id)
        (init-db db))))
  