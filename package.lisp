;;;; package.lisp

(defpackage #:cl-lmsensors
  (:use #:cl #:cl-ppcre #:jonathan #:uiop/run-program)
  (:export #:*sensors-binary*
	   #:fetch-sensor-data
	   #:fetch-default-sensor-data
	   #:parse-sensor-data
	   #:parse-default-sensor-data))

