
(defsystem gallery
    :depends-on (#:restas #:cl-who #:restas-directory-publisher)
    :components ((:file "hello" :depends-on ("gallery"))
                 (:file "draw" :depends-on ("gallery"))
                 (:file "gallery" :depends-on ("upload" "files-locator" "gal-content"))
                 (:file "upload" :depends-on ("files-locator"))
                 (:file "files-locator")
                 (:file "gal-content" :depends-on ("files-locator" "transliterate"))
                 (:file "transliterate")))
