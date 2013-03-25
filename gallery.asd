
(defsystem gallery
    :depends-on (#:restas #:cl-who #:restas-directory-publisher)
    :components ((:file "draw" :depends-on ("gallery" "gallery.content"))
                 (:file "gallery" :depends-on ("upload" "files-locator" "gallery.content"))
                 (:file "upload" :depends-on ("files-locator"))
                 (:file "files-locator")
                 (:file "gallery.content" :depends-on ("files-locator" "transliterate"))
                 (:file "transliterate")))
