
(defsystem #:mongo-db-pics-collection
  :depends-on (#:gallery #:mongo-cl-driver.usocket #:iterate)
  :components ((:file "mongo-db-pics-collection")))
