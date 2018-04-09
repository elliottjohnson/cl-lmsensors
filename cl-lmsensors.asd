;;;; cl-lmsensors.asd

(asdf:defsystem #:cl-lmsensors
  :description "A simple means of capturing lm_sensors data."
  :author "Elliott Johnson <elliott@elliottjohnson.net>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:jonathan #:uiop #:cl-ppcre)
  :components ((:file "package")
               (:file "cl-lmsensors")))
