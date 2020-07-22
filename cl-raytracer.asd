;;;; cl-raytracer.asd

(asdf:defsystem #:cl-raytracer
  :description "Describe cl-raytracer here"
  :author "Jakob L. Kreuze"
  :license  "GPL"
  :version "0.0.1"
  :serial t
  :depends-on (#:trivia)
  :components ((:file "cl-raytracer"))
  :entry-point (uiop:symbol-call :cl-raytracer "MAIN"))
