;;;; package.lisp

(defpackage #:cl-lmsensors
  (:use #:cl #:cl-ppcre #:jonathan #:uiop/run-program)
  (:nicknames #:lm_sensors)
  (:export #:*sensors-binary*
	   #:*default-sensors-data-method*
	   #:fetch-sensor-data
	   #:fetch-default-sensor-data
	   #:parse-sensor-data
	   #:parse-default-sensor-data))


