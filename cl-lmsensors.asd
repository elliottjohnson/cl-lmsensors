;;;; cl-lmsensors.asd

(asdf:defsystem #:cl-lmsensors
  :description "A foreign function interface to libsensors."
  :author "Elliott Johnson <elliott@elliottjohnson.net>"
  :license  "MIT"
  :version "0.4.1"
  :defsystem-depends-on (#:cffi-grovel)
  :depends-on (#:cl-ppcre #:cffi #:cffi-libffi #:dynamic-mixins)
  :components ((:file "package")
	       (:module "cffi"
			:serial t
			:depends-on ("package")
			:components ((:cffi-grovel-file "grovel")
				     (:file "libsensors")
				     (:file "sensors")))
	       (:module "src"
			:serial t
			:depends-on ("package" "cffi")
			:components ((:file "classes")))))


