(defpackage #:gallery.default-render
  (:use #:cl #:cl-who #:gallery #:gallery.content
        #:gallery.policy.render #:gallery.internal.render)
  (:export #:handler
           #:*edit-items*))

(in-package #:gallery.default-render)

(defclass handler () ())

(defvar *edit-items* t)

(defun gen-static-url (path)
  (restas:genurl 'static.route :path path))

(defmethod theme.add-pic ((drawer handler) form father father-name)
  (with-html-output-to-string (sss nil :prologue t :indent t)
    (:html (:head (:script :language "javascript" :type "text/javascript"
                           :src "static/js/preview-updater.js"))
           (:body (str form)
                  (:form :method "get" :action (restas:genurl 'receive-pic)
                         (:input :type "hidden" :id "title" :name "title" :value "")
                         (:input :type "hidden" :id "comment" :name "comment" :value "")
                         (:input :type "hidden" :id "time" :name "time" :value "")
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
                         (:input :type "hidden" :id "title" :name "title" :value "")
                         (:input :type "hidden" :id "comment" :name "comment" :value "")
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

(defmethod theme.update-item-form ((drawer handler) (content item))
  (let* ((update-url (restas:genurl 'gallery:update-item :id (item-id content)))
         (update-call (format nil "updateItem(~a, \"~a\")" (item-id content) update-url)))
    (with-html-output-to-string (str nil :prologue nil :indent t)
      (:form :action (format nil "javascript:~a" update-call)
             :method "get" :id (format nil "update-form-~a" (item-id content))
             (:input :type "hidden" :name "id" :value (item-id content))
             (:input :type "text" :class "title" :name "title" :value (item-title content))
           (:input :type "text" :class "comment" :name "comment" :value (item-comment content))
           (:input :type "submit" :class "update-btn" :onclick update-call)))))

(defmethod theme.preview ((drawer handler) (content picture) chkbox)
  (with-accessors ((url pic-url) (thumbnail item-thumbnail)
                   (title item-title) (comment item-comment)
                   (id item-id)) content
    (with-html-output-to-string (str nil :prologue nil :indent t)
      (:div :class "img" :id (format nil "img-~a" id)
            (:a :href url :rel "group" :class "fancybox-thumb" :title title
                (:img :src thumbnail))
            (:div :class "desc"
                  (when (and *edit-items* (not chkbox))
                    (htm (:div :class "edit-btn"
                               :onclick (format nil "editItem(~a, event)" (item-id content))
                               *edit-items*)))
                  (:div :class "title" (str title))
                  (:div :class "comment" (str comment)))
            (when (and *edit-items* (not chkbox))
              (htm (:div :hidden t :id (format nil "edit-~a" (item-id content))
                         :class "edit-box"
                         (str (theme.update-item-form drawer content)))))
            (when chkbox
              (htm (:input :type "checkbox" :name chkbox :value id)))))))

(defmethod theme.preview ((drawer handler) (content album) chkbox)
  (with-accessors ((name album-name) (thumbnail item-thumbnail)
                   (title item-title) (comment item-comment)
                   (items album-items) (id item-id))
      content
    (let ((url (restas:genurl 'gallery:view-album :id id)))
      (with-html-output-to-string (str nil :prologue nil :indent t)
        (:div :class "img album" :id (format nil "img-~a" id)
              (:a :href url :title title
                  (:img :src thumbnail))
              (:div :class "desc"
                    (when (and *edit-items* (not chkbox))
                      (htm (:div :class "edit-btn"
                                 :onclick (format nil "editItem(~a, event)" (item-id content))
                                 *edit-items*)))
                    (:div :class "title" (str title))
                    (:div :class "comment" (str comment)))
              (when (and *edit-items* (not chkbox))
                (htm (:div :hidden t :id (format nil "edit-~a" (item-id content))
                           :class "edit-box"
                           (str (theme.update-item-form drawer content)))))
              (when chkbox
                (htm (:input :type "checkbox" :name chkbox :value id))))))))

(defmethod theme.no-such-album ((drawer handler) name)
  (format nil "<h2> There is no album with id ~a </h2>" name))
