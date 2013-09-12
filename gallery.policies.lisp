(restas:define-policy render
  (:interface-package #:gallery.policy.render)
  (:interface-method-template "THEME.~A")
  (:internal-package #:gallery.internal.render)
  (:internal-function-template "~A-RENDER")
  
  (define-method add-pic (form album album-name)
    "Draw a page with form for a picture addendum")
  (define-method add-album (form father father-name)
    "Draw a page for new album form")
  (define-method view-album (add-pic-url add-alb-url rem-pic-url album)
    "Draw all pictures in the album")
  (define-method choose-album (action albums)
    "Show the table with checkboxes for user to choose some albums")
  (define-method choose-picture (action album)
    "Show the current album for user to choose some pictures from it")
  (define-method no-such-album (id)
    "Show the not found message fro the album named name")
  (define-method pics-grid (album chkbox)
    "Draw all pictures from the given album, and supply them by
     the checkbox if given.")
  (define-method preview (content chkbox)
    "draw a small preview composition.
     The chkbox is the name of checkbox group, if nil - no checkbox"))

(restas:define-policy pics-collection
  (:interface-package #:gallery.policy.pics-collection)
  (:interface-method-template "P-COLL.~A")
  (:internal-package #:gallery.internal.pics-collection)
  (:internal-function-template "~A-PIC-COLL")

  (define-method get-item (id)
    "Get an item (album or picture) from the database by it's unique id")
  (define-method save-pictures (pics father-id)
    "Persist a list of pictures pics in album with id father-id.
     Returns nil if father is absent, and non-nil otherwise. ")
  (define-method save-album (album father-id)
    "Persist the album from the album with id father-id.
     Return nil if father is not found, and non-nil otherwise.")
  (define-method update-album (album)
    "Persist all changes, made in the album")
  (define-method gen-uniq-id ()
    "Generate a new unique id for a next item")
  (define-method root-album-id ()
    "Get the id of the very first album - the root in the tree of all albums"))



