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

(defvar *default-sensors-data-method* :json
  "The default method used to pull data from lm-sensors.")

(defgeneric fetch-sensors-data (method)
  (:documentation "Fetches sensor data and returns an alist tree of data. ")
  (:method :before (method)
	   (unless *sensors-binary* (find-sensors-binary)))
  (:method ((method (eql :json)))
    (parse (sensors "-j"
		    "Failed to access json from ~A.  Does your binary support JSON? '~A'"
		    *sensors-binary*
		    *sensors-version*)
	   :as :alist)))

(defun parse-alist-data (alist)
  (loop for sensor in alist
     collect (parse-sensor-data sensor)))

(defun parse-sensor-data (sensor-alist)
  (labels ((sensor-type (type)
	     (scan type (car sensor-alist))))
    (cond ((sensor-type "coretemp-isa")
	   (parse-coretemp-isa-alist sensor-alist))
	  ((sensor-type "atk0110-acpi")
	   (parse-atk0110-acpi-alist sensor-alist))
	  (t (warn "Unknown sensor data type.  Please add a condition to PARSE-SENSOR-DATA.")))))

(defun parse-coretemp-isa-alist (alist)
  (cons (car alist)
	(loop for data-line in (cdr alist)
	   when (listp (cdr data-line))
	   collect
	     (let ((name (car data-line))
		   crit-alarm
		   crit
		   max
		   input)
	       (loop for (key . value) in (cdr data-line)
		  do (cond ((scan "_crit_alarm$" key)
			    (setf crit-alarm value))
			   ((scan "_crit$" key)
			    (setf crit value))
			   ((scan "_max$" key)
			    (setf max value))
			   ((scan "_input$" key)
			    (setf input value))))
	       (list name
		     input
		     ;; status :ok :warn :critical
		     (cond ((and input max (<= input max)) :ok)
			   ((and input crit (<= input crit)) :warn)
			   ((and input crit (>  input crit)) :critical))
		     ;; percent of max
		     (when (and input max (not (zerop max)))
		       (* 100 (/ input max)))
		     ;; percent of critical
		     (when (and input crit (not (zerop crit)))
		       (* 100 (/ input crit)))
		     ;; percent of critical alarm.
		     (when (and input crit-alarm (not (zerop crit-alarm)))
		       (* 100 (/ input crit-alarm))))))))

(defun parse-atk0110-acpi-alist (alist)
  (cons (car alist)
	(loop for data-line in (cdr alist)
	   when (listp (cdr data-line))
	   collect
	     (let ((name (car data-line))
		   crit
		   max
		   min
		   input)
	       (loop for (key . value) in (cdr data-line)
		  do (cond ((scan "_crit$"  key)
			    (setf crit  value))
			   ((scan "_max$"   key)
			    (setf max   value))
			   ((scan "_input$" key)
			    (setf input value))
			   ((scan "_min$"   key)
			    (setf min   value))))
	       (list name
		     input
		     ;; status :ok :warn or :critical
		     (cond ((and input max min (<= min input max)) :ok)
			   ((and input min) (<= input min) :warn)
			   ((and input max (>= input max) (if crit :warn :critical)))
			   ((and input crit (>= input crit)) :critical))
		     ;; percent of max.. or optionally percent between min and max.
		     (cond ((and min input max (not (zerop (- max min))))
			    (* 100 (/ (- input min) (- max min))))
			   ((and input max (not (zerop max)))
			    (* 100 (/ input max))))
		     ;; percent of crit
		     (when (and input crit (not (zerop crit)))
		       (* 100 (/ input crit))))))))
