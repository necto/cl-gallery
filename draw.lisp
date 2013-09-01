(defpackage #:gallery.default-render
  (:use #:cl #:cl-who #:gallery #:gallery.content
        #:gallery.policy.render #:gallery.internal.render)
  (:export #:handler))

(in-package #:gallery.default-render)

(defclass handler () ())

(defun gen-static-url (path)
  (restas:genurl 'static.route :path path))

(defmethod theme.add-pic ((drawer handler) form father father-name)
  (with-html-output-to-string (sss nil :prologue t :indent t)
    (:html (:head (:script :language "javascript" :type "text/javascript"
                           :src "static/js/preview-updater.js"))
           (:body (str form)
                  (:form :method "get" :action (restas:genurl 'receive-pic)
                         "Title:" (:input :type "text" :name "title" :value "")
                         "Comment:" (:input :type "text" :name "comment" :value "")
                         "Father:" (str father-name)
                         (:input :type "hidden" :name "father" :value father :readonly t)
                         (:input :type "hidden" :name "pic" :value "no-value" :id "pic")
                         (:input :type "submit" :value "like it!"))
                  (:div :id "preview")))))

(defmethod theme.add-album ((drawer handler) form father father-name)
  (with-html-output-to-string (sss nil :prologue t :indent t)
    (:html (:head (:script :language "javascript" :type "text/javascript"
                           :src "static/js/preview-updater.js"))
           (:body (str form)
                  (:form :method "get" :action (restas:genurl 'receive-album)
                         "Title:" (:input :type "text" :name "title" :value "")
                         "Comment:" (:input :type "text" :name "comment" :value "")
                         "Father:" (str father-name)
                         (:input :type "hidden" :name "father" :value father :readonly t)
                         (:input :type "hidden" :name "pic" :value "no-value" :id "pic")
                         (:input :type "submit" :value "That is right!"))
                  (:div :id "preview")))))

(defmacro in-album-page (stream album &body body)
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
                   (:h1 (str (item-title ,album)))
                   (:p (str (item-comment ,album)))
                   ,@body))))

(defmethod theme.view-album ((drawer handler) add-pic-url add-alb-url rem-pic-url album)
  (in-album-page stream album
    (:center (:a :href add-pic-url
                 "add a picture")
             (:a :href add-alb-url
                 "add an album")
             (:a :href rem-pic-url
                 "remove some items"))
    (:br)
    (str (pics-grid-render album nil))))

(defmethod theme.choose-picture ((drawer handler) action album)
  (in-album-page stream album
    (:form :action action
           (:center (:input :type "submit"))
           (:br)
           (str (pics-grid-render album "chosen")))))

(defmethod theme.pics-grid ((drawer handler) album chkbox)
  (with-output-to-string (str)
    (loop for pic in (album-items album) do
         (write-string (preview-render pic chkbox) str))))

(defmethod theme.preview ((drawer handler) (content picture) chkbox)
  (with-accessors ((url pic-url) (thumbnail item-thumbnail)
                   (title item-title) (comment item-comment)
                   (id item-id)) content
    (with-html-output-to-string (str nil :prologue nil :indent t)
      (:div :class "img" 
            (:a :href url :rel "group" :class "fancybox-thumb" :title title
                (:img :src thumbnail))
            (:div :class "desc" (:b (str title))
                  (:br) (str comment))
            (when chkbox
              (htm (:input :type "checkbox" :name chkbox :value id)))))))

(defmethod theme.preview ((drawer handler) (content album) chkbox)
  (with-accessors ((name album-name) (thumbnail item-thumbnail)
                   (title item-title) (comment item-comment)
                   (items album-items) (id item-id))
      content
    (let ((url (restas:genurl 'gallery:view-album :id id)))
      (with-html-output-to-string (str nil :prologue nil :indent t)
        (:div :class "img"
              (:a :href url :title title
                  (:img :src thumbnail))
              (:div :class "desc" (:b (str title))
                    (:br) (str comment))
              (when chkbox
                (htm (:input :type "checkbox" :name chkbox :value id))))))))

(defmethod theme.no-such-album ((drawer handler) name)
  (format nil "<h2> There is no album with id ~a </h2>" name))
