;;;; cl-lmsensors.asd

(asdf:defsystem #:cl-lmsensors
  :description "A simple means of capturing lm_sensors data."
  :author "Elliott Johnson <elliott@elliottjohnson.net>"
  :license  "MIT"
  :version "0.1.0"
  :depends-on (#:cl-ppcre #:uiop #:jonathan)
  :components ((:module "core"
			:pathname ""
			:components ((:file "package")
				     (:file "cl-lmsensors"
					    :depends-on ("package"))))
	       ;; TODO allow for individual methods to be loaded.
	       ;;   If we don't have JSON support, then why load jonathan?
	       (:module methods
			:depends-on ("core")
			:components ((:file "json")))
	       (:module hardware
			:depends-on ("core")
			:components ((:file "atk0110-acpi")
				     (:file "coretemp-isa")))))

