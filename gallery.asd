
(defsystem gallery
    :depends-on (#:restas #:cl-who #:restas-directory-publisher #:local-time)
    :components ((:file "draw" :depends-on ("gallery" "gallery.content"))
                 (:file "gallery.policies")
                 (:file "gallery" :depends-on ("upload" "gallery.policies" "files-locator" "gallery.content"))
                 (:file "pics-memory-collection"
                        :depends-on ("gallery.policies" "gallery.content"))
                 (:file "upload" :depends-on ("files-locator"))
                 (:file "files-locator")
                 (:file "gallery.content" :depends-on ("gallery.policies" "files-locator" "transliterate"))
                 (:file "transliterate")))
