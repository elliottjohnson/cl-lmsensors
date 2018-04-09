;;;; package.lisp

(defpackage #:cl-lmsensors
  (:use #:cl #:cl-ppcre #:jonathan #:uiop/run-program)
  (:export #:*sensors-binary*
	   #:fetch-sensor-data
	   #:fetch-default-sensor-data
	   #:parse-alist-data
	   #:parse-default-sensor-data))

