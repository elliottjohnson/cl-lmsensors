;;;; cl-lmsensors.asd

(asdf:defsystem #:cl-lmsensors
  :description "A foreign function interface to libsensors."
  :author "Elliott Johnson <elliott@elliottjohnson.net>"
  :license  "MIT"
  :version "0.5.0"
  :defsystem-depends-on (#:cffi-grovel)
  :depends-on (#:cl-ppcre #:cffi #:cffi-libffi #:dynamic-mixins)
  :serial t
  :components ((:file "package")
	       (:cffi-grovel-file "grovel")
	       (:file "libsensors")
	       (:file "sensors")
	       (:file "cl-lmsensors")))


