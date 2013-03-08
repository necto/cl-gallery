(restas:define-module #:restas.hello-world
    (:use :cl)
  (:export main))

(in-package #:restas.hello-world)

(restas:define-route main ("")
  "<H1> hello wooo! </h1>")

(restas:mount-submodule test-gallery (#:gallery)
  (gallery:*baseurl* '("gal")))

(restas:start '#:restas.hello-world :port 8080)