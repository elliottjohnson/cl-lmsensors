;;;; cl-lmsensors.lisp

(in-package #:cl-lmsensors)

(defun execute-external-program (program-string &rest error-args)
  (handler-case
      (string-right-trim '(#\Newline)
        (with-output-to-string (string)
	  (run-program program-string :output string)))
    (subprocess-error ()
      (apply #'error error-args))))

(defvar *sensors-binary* nil
  "The path to the sensors binary, used to pull info from lm_sensors.")

(defun find-sensors-binary ()
  (setf *sensors-binary*
	(execute-external-program
	 "which sensors"
	 "Cannot locate SENSORS binary, is it in $PATH?")))

(defun sensors (arguments &rest error-args)
  (apply #'execute-external-program
	 (concatenate 'string *sensors-binary* " " arguments)
	 error-args))

(defvar *sensors-version* nil
  "The version string as returned by the *SENSORS-BINARY*")

(defun sensors-version ()
  (setf *sensors-version*
	(sensors "--version"
		 "Cannot find version string for sensors binary!")))

(defvar *default-sensors-data-method* nil
  "The default method used to pull data from lm-sensors.")

(defvar *default-sensors-data-methods* ()
  "A list of registered data methods.")

(defun register-sensors-data-method (method)
  (push method *default-sensors-data-methods*))

(defgeneric fetch-sensor-data (method)
  (:documentation "Fetches sensor data and returns an alist tree of data. ")
  (:method :before (method)
	   (unless *sensors-binary* (find-sensors-binary))))

(defvar *sensors-data-methods-directory*
  (merge-pathnames "methods/*.*"
		   (asdf/system:system-source-directory :cl-lmsensors)))

(defun load-sensor-data-methods ()
  (dolist (file (directory *sensors-data-methods-directory*))
    (load file)))

(load-sensor-data-methods)

(defun fetch-default-sensor-data (&optional
				    (method *default-sensors-data-method*))
  (fetch-sensor-data method))

(defun parse-default-sensor-data (&optional
				    (method *default-sensors-data-method*))
  "Fetches a parsed alist of data using the default sensors method."
  (parse-sensor-data (fetch-default-sensor-data method)))

(defun parse-sensor-data (alist)
  (loop for sensor in alist
     collect (dispatch-sensor-data-parsing sensor)))

(defvar *hardware-table* (make-hash-table :test 'equal)
  "A table used to map hardware names to parsing methods.")

(defun add-hardware-parsing-method (hardware-name function)
  "Adds a parsing method as function accepting a single argument
in the form of an alist and maps it to HARDWARE-NAME."
  (setf (gethash hardware-name *hardware-table*) function))

(defvar *default-hardware-methods-directory*
  (merge-pathnames "hardware/*.*"
		   (asdf/system:system-source-directory :cl-lmsensors)))

(defun load-hardware-parsing-methods ()
  (loop for file in (directory *default-hardware-methods-directory*)
     do (load file)))

(load-hardware-parsing-methods)

(defun dispatch-sensor-data-parsing (sensor-alist)
  (labels ((sensor-type (type)
	     (scan type (car sensor-alist))))
    (loop for name being the hash-keys in *hardware-table*
       using (hash-value function)
       when (sensor-type name)
       collect (funcall function sensor-alist))))
