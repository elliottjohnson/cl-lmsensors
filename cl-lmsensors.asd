;;;; cl-lmsensors.asd

;; Override the default behavior for dynamic-mixins.. see:
;;  https://github.com/rpav/dynamic-mixins/issues/1
(pushnew :disable-mixin-object-print-object cl:*features*)

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


