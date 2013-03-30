(defpackage #:gallery.default-render
  (:use #:cl #:cl-who #:gallery #:gallery.content #:gallery.policy.render)
  (:export #:handler))

(in-package #:gallery.default-render)

(defclass handler () ())

(defun gen-static-url (path)
  (restas:genurl 'static.route :path path))

(defmethod theme.add-pic ((drawer handler) form album)
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

(defmethod theme.add-album ((drawer handler) form)
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

(defmethod theme.album-list ((drawer handler) add-album-url rem-album-url albums)
  (with-html-output-to-string (stream nil :prologue t :indent t)
    (:html (:head (:link :rel "stylesheet" :type "text/css" :media "screen"
                         :href (gen-static-url "css/gallery.css")))
           (:body (:center (:a :href add-album-url
                           "new album")
                           (:a :href rem-album-url
                               "delete album"))
                  (:br)
                  (loop for album in albums do
                       (draw-preview album nil stream))))))

(defmethod theme.choose-album ((drawer handler) action albums)
  (with-html-output-to-string (stream nil :prologue t :indent t)
    (:html (:head (:link :rel "stylesheet" :type "text/css" :media "screen"
                         :href (gen-static-url "css/gallery.css")))
           (:body (:form :action action
                         (:center (:input :type "submit"))
                         (:br)
                         (loop for album in albums do
                              (draw-preview album "chosen" stream)))))))

(defmacro in-album-page (stream &body body)
  `(with-html-output-to-string (,stream nil :prologue t :indent t)
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
                   ,@body))))

(defmethod theme.view-album ((drawer handler) add-pic-url rem-pic-url album)
  (in-album-page stream
    (:center (:a :href add-pic-url
                 "add a picture")
             (:a :href rem-pic-url
                 "remove some pictures"))
    (:br)
    (loop for pic in (album-items album) do
         (draw-preview pic nil stream))))

(defmethod theme.choose-picture ((drawer handler) action album)
  (in-album-page stream
    (:form :action action
           (:center (:input :type "submit"))
           (:br)
           (loop for pic in (album-items album) do
                (draw-preview pic "chosen" stream)))))

(defmethod theme.no-such-album ((drawer handler) name)
  (format nil "<h2> There is no album named ~a </h2>" name))
