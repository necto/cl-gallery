(defpackage #:gallery.default-render
  (:use #:cl #:cl-who #:gallery #:gallery.policy.render)
  (:export #:handler))

(in-package #:gallery.default-render)

(defclass handler () ())

(defun gen-static-url (path)
  (restas:genurl 'static.route :path path))

(defmethod add-pic-render ((drawer handler) form album)
  (with-html-output-to-string (sss nil :prologue t :indent t)
    (:html (:head (:script :language "javascript" :type "text/javascript"
                           :src "static/js/preview-updater.js"))
           (:body (str form)
                  (:form :method "get" :action (restas:genurl 'receive-pic)
                         "Title:" (:input :type "text" :name "title" :value "")
                         "Comment:" (:input :type "text" :name "comment" :value "")
                         "Album:" (:input :type "text" :name "album" :value album :readonly t)
                         (:input :type "hidden" :name "pic" :value "no-value" :id "pic")
                         (:input :type "submit" :value "like it!"))
                  (:div :id "preview")))))

(defmethod add-album-render ((drawer handler) form)
  (with-html-output-to-string (sss nil :prologue t :indent t)
    (:html (:head (:script :language "javascript" :type "text/javascript"
                           :src "static/js/preview-updater.js"))
           (:body (str form)
                  (:form :method "get" :action (restas:genurl 'receive-album)
                         "Title:" (:input :type "text" :name "title" :value "")
                         "Comment:" (:input :type "text" :name "comment" :value "")
                         (:input :type "hidden" :name "pic" :value "no-value" :id "pic")
                         (:input :type "submit" :value "That is right!"))
                  (:div :id "preview")))))

(defmethod album-list-render ((drawer handler) add-album-url albums)
  (with-html-output-to-string (stream nil :prologue t :indent t)
    (:html (:head (:link :rel "stylesheet" :type "text/css" :media "screen"
                         :href (gen-static-url "css/gallery.css")))
           (:body (:center (:a :href add-album-url
                           "new album"))
                  (:br)
                  (loop for album in albums do
                       (draw-preview album stream))))))

(defmethod view-album-render ((drawer handler) add-pic-url album)
  (with-html-output-to-string (stream nil :prologue t :indent t)
    (:html (:head (:script :language "javascript" :type "text/javascript"
                           :src "http://code.jquery.com/jquery-1.9.1.min.js")
                  (:script :language "javascript" :type "text/javascript"
                           :src (gen-static-url "js/jquery.mousewheel-3.0.6.pack.js"))
                  (:link :rel "stylesheet" :type "text/css" :media "screen"
                         :href (gen-static-url "css/jquery.fancybox.css"))
                  (:script :language "javascript" :type "text/javascript"
                           :src (gen-static-url "js/jquery.fancybox.pack.js"))
                  (:link :rel "stylesheet" :type "text/css" :media "screen"
                         :href (gen-static-url "css/jquery.fancybox-thumbs.css"))
                  (:script :language "javascript" :type "text/javascript"
                           :src (gen-static-url "js/jquery.fancybox-thumbs.js"))
                  (:link :rel "stylesheet" :type "text/css" :media "screen"
                         :href (gen-static-url "css/gallery.css")))
           (:body (:script :language "javascript" :type "text/javascript"
                           :src (gen-static-url "js/run-gallery.js"))
                  (:h1 (str (item-title album)))
                  (:p (str (item-comment album)))
                  (:center (:a :href add-pic-url
                               "add a picture"))
                  (:br)
                  (loop for pic in (album-items album) do
                       (draw-preview pic stream))))))
