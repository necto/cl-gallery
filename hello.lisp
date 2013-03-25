(restas:define-module #:restas.hello-world
    (:use :cl)
  (:export main))

(in-package #:restas.hello-world)

(restas:define-route main ("")
  "<H1> hello wooo! </h1>")

(restas:mount-module test-gallery (#:gallery)
  (:url "gal"))

(restas:start '#:restas.hello-world :port 8081)