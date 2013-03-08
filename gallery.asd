
(defsystem gallery
    :depends-on (#:restas #:cl-who #:restas-directory-publisher)
    :components ((:file "hello" :depends-on ("gallery"))
                 (:file "gallery" :depends-on ("upload" "files-locator"))
                 (:file "upload" :depends-on ("files-locator"))
                 (:file "files-locator")))
