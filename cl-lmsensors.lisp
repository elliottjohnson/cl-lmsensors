;;;; cl-lmsensors.lisp

(in-package #:cl-lmsensors)

;;;
;;; Utilities for interacting directly with lm_sensors
;;;

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
  "Executes the external program WHICH to locate the sensors binary.
Sets the value of *SENSORS-BINARY* and returns the pathname if found. "
  (setf *sensors-binary*
	(execute-external-program
	 "which sensors"
	 "Cannot locate SENSORS binary, is it in $PATH?")))

(defun lm_sensors (arguments &rest error-args)
  "Executes the program stored in *SENSORS-BINARY* with provided ARGUMENTS
and for non-zero return values calls ERROR with the provided ERROR-ARGS."
  (apply #'execute-external-program
	 (concatenate 'string *sensors-binary* " " arguments)
	 error-args))

(defvar *sensors-version* nil
  "The version string as returned by the *SENSORS-BINARY*")

(defun sensors-version ()
  "Sets *SENSORS-VERSION* to the current lm_sensors version string."
  (unless *sensors-binary* (find-sensors-binary))
  (setf *sensors-version*
	(lm_sensors "--version"
		    "Cannot find version string for sensors binary! '~A'"
		    *sensors-binary*)))

;;;;
;;;; Parsing infrastructure
;;;;

;;;
;;; Hardware
;;;

(defclass hardware ()
  ((name :documentation
	 "The hardware component name as reported by lm_sensors."
	 :accessor name
	 :initarg :name)
   (adaptor :documentation "The adaptor name string."
	    :accessor adaptor
	    :initarg :adaptor)
   (sensors :documentation "A list of sensor data associated with hardware."
	    :accessor sensors
	    :initform ()
	    :initarg :sensors))
  (:documentation "A parent class of all sensor hardware"))

(defmethod print-object ((hardware hardware) stream)
  (print-unreadable-object (hardware stream :type t :identity t)
    (when (slot-boundp hardware 'name)
      (format stream "~S" (name hardware)))))

(defgeneric hardware-class (object)
  (:documentation
   "A method of converting a hardware name into a class symbol")
  (:method ((hardware string))
    (register-groups-bind (keyword)
	("\(.*\)-[0-9]+" (string-upcase hardware))
      (when (and keyword (stringp keyword))
	(find-class (intern keyword)))))
  (:method ((hardware hardware))
    (when (slot-boundp hardware 'name)
      (with-accessors ((name name))
	  hardware
	(when (and name (stringp name))
	  (hardware-class name))))))

(defun parse-sensor-data (alist)
  "Parses an alist of hardware information into a list of hardware objects
populated with sensor data."
  (loop for hardware-alist in alist
     collect (parse-hardware hardware-alist)))

(defun parse-hardware (alist)
  "Parses the sensor data relating to a single hardware type."
  (let ((hardware (make-instance (hardware-class (car alist))
				 :name (car alist)
				 :adaptor (cdr (assoc "Adapter"
						      (cdr alist)
						      :test 'equal)))))
    (setf (sensors hardware)
	  (loop for sensor in (cdr alist)
	     when (listp (cdr sensor))
	     collect (parse-hardware-sensor-data hardware sensor)))
    hardware))

(defgeneric sensor-class (hardware sensor-name)
  (:method-combination or)
  (:documentation
   "A method of converting SENSOR-NAME into a class symbol")
  (:method or (hardware sensor-name)
	   (error "Unknown sensor type (~A) for hardware class '~A'!"
		  sensor-name
		  (hardware-class hardware))))

;;;
;;; Hardware classes
;;;

(defclass lmsensors-mixin ()
  ()
  (:documentation "A class of mixins related to lm_sensors."))

(defclass voltage-mixin (lmsensors-mixin)
  ((voltage-class :initarg :voltage-class :accessor voltage-class))
  (:documentation "A mixin for handling voltage sensors."))

(defmethod sensor-class or ((hardware voltage-mixin) (class string))
  (when (scan " Voltage$" class)
    (voltage-class hardware)))

(defclass fan-mixin (lmsensors-mixin)
  ((fan-class :initarg :fan-class :accessor fan-class))
  (:documentation "A mixin for handling cooling fan sensors"))

(defmethod sensor-class or ((hardware fan-mixin) (class string))
  (when (scan " FAN Speed$" class)
    (fan-class hardware)))

(defclass temperature-mixin (lmsensors-mixin)
  ((temperature-class :initarg :temperature-class :accessor temperature-class))
  (:documentation "A mixin for handling temperature sensors."))

(defmethod sensor-class or ((hardware temperature-mixin) (class string))
  (when (scan " Temperature$" class)
    (temperature-class hardware)))

;;;
;;; Sensor data
;;;

(defclass sensor-data ()
  ((name :accessor name :initarg :name
	 :documentation "The individual sensor name.")
   (value :accessor value :initarg :value
	  :documentation "The sensor value, also known as the input value.")
   (units :accessor units
	  :initarg :units
	  :documentation "The unit type that value represents.")
   (status :accessor status
	   :initarg :status
	   :initform nil
	   :documentation "A keyword that is one of :OK, :WARN, :CRITICAL"))
  (:documentation
   "The parent class of all sensor-data."))

(defmethod print-object ((sensor-data sensor-data) stream)
  (print-unreadable-object (sensor-data stream :type t :identity t)
    (when (and (slot-boundp sensor-data 'name)
	       (slot-boundp sensor-data 'value))
      (format stream
	      "~A: ~A ~@[~A~]"
	      (name sensor-data)
	      (value sensor-data)
	      (status sensor-data)))))

(defgeneric parse-hardware-sensor-data (object sensor-alist)
  (:documentation
   "Accepts an object & a data alist & returns a sensor-data object.")
  (:method ((hardware hardware) (sensor-alist list))
    (parse-hardware-sensor-data (make-instance
				 (sensor-class hardware (car sensor-alist))
				 :name (car sensor-alist))
				(cdr sensor-alist)))
  (:method ((data sensor-data) (sensor-alist list))
    (loop for (name . value) in sensor-alist
       do (set-sensor-data data name value))
    data)
  (:method :around ((data sensor-data) alist)
	   (call-next-method)
	   (with-accessors ((status status))
	       data
	     (unless status (setf status :ok)))
	   data))

(defgeneric set-sensor-data (sensor-data name value)
  (:method-combination or)
  (:documentation
   "Sets a value in SENSOR-DATA based upon the data attribute NAME & VALUE.")
  (:method or (sensor-data name value)
	   (if (scan "_input$" name)
	       (setf (value sensor-data) value)
	       (error "Unknown sensor data NAME (~A) for sensor: ~A.  Cannot set value to: ~A"
		      name
		      sensor-data
		      value))))

;;;
;;; Sensor classes
;;;

(defclass sensor-min-mixin (lmsensors-mixin)
  ((minimum :accessor minimum
	    :initarg :minimum
	    :documentation "The lower limit of the input value."))
  (:documentation "A mixin class that provides a minimum field value."))

(defmethod set-sensor-data or ((sensor sensor-min-mixin) (name string) value)
  (when (scan "_min$" name)
    (setf (minimum sensor) value)))

(defun sensor-minimum-value (sensor)
  "A function to return a sensors minimum value or otherwise 0."
  (if (typep sensor 'sensor-min-mixin)
      (minimum sensor)
      0))

(defmethod parse-hardware-sensor-data :after ((sensor sensor-min-mixin)
					      alist)
  (with-accessors ((min minimum) (value value) (status status))
      sensor
    (unless (eql status :critical)
      (when (<= value min)
	(setf status :warn)))))

(defclass sensor-max-mixin (lmsensors-mixin)
  ((maximum :accessor maximum
	    :initarg :maximum
	    :documentation "The upper limit of the input value.")
   (%maximum :initarg :%maximum
	     :accessor %maximum
	     :documentation
	     "A percentage of VALUE between MINIMUM and MAXIMUM")) 
  (:documentation "A mixin class that provides a maximum field value."))

(defmethod set-sensor-data or ((sensor sensor-max-mixin) (name string) value)
  (when (scan "_max$" name)
    (setf (maximum sensor) value)))

(defmethod parse-hardware-sensor-data :after ((sensor sensor-max-mixin) alist)
  (let ((min (sensor-minimum-value sensor)))
    (with-accessors ((max maximum)(value value)(%max %maximum) (status status))
	sensor
      (setf %max (* 100 (/ (- max value min) (- max min))))
      (unless (eql status :critical)
	(when (>= value max)
	  (setf status (if (slot-exists-p sensor 'critical)
			   :warn
			   :critical)))))))

(defclass sensor-critical-mixin (lmsensors-mixin)
  ((critical :accessor critical
	     :initarg :critical
	     :documentation "A critical limit of the input value.")
   (%critical :initarg :%critical :accessor %critical
	      :documentation
	      "A percentage of VALUE between MINIMUM and CRITICAL"))
  (:documentation
   "A mixin class that provides a value beyond which damage may result."))

(defmethod set-sensor-data or ((sensor sensor-critical-mixin)
			       (name string)
			       value)
  (when (scan "_crit$" name)
    (setf (critical sensor) value)))

(defmethod parse-hardware-sensor-data :after ((sensor sensor-critical-mixin)
					      alist)
  (let ((min (sensor-minimum-value sensor)))
    (with-accessors ((crit critical)
		     (value value)
		     (%crit %critical)
		     (status status))
	sensor
      (setf %crit (* 100 (/ (- crit value min) (- crit min))))
      (unless (eql status :critical)
	(when (>= value crit)
	  (setf status :critical))))))

(defclass sensor-critical-alarm-mixin (lmsensors-mixin)
  ((critical-alarm :documentation "A level at which a critical alarm occurs."
		   :accessor critical-alarm
		   :initarg :critical-alarm))
  (:documentation
   "A mixin class to provide a value beyond which an Out-of-spec bit is
set, which never clears and CPU integrity isn't assured."))

(defmethod set-sensor-data or ((sensor sensor-critical-alarm-mixin)
			       (name string)
			       (value number))
  (when (scan "_crit_alarm$" name)
    (setf (critical-alarm sensor) value)))

(defmethod parse-hardware-sensor-data :after ((sensor
					       sensor-critical-alarm-mixin)
					      alist)
  (with-accessors ((crit-alarm critical-alarm)
		   (status status))
      sensor
    (unless (= 0 crit-alarm)
      (setf status :critical))))

;;;
;;; Data input methods
;;;

(defvar *default-sensors-data-method* nil
  "The default method used to pull data from lm-sensors.")

;; User adjustable to suit site specific configuration.
(setf *default-sensors-data-method* :json)

(defgeneric fetch-sensor-data (method)
  (:documentation "Fetches sensor data and returns an alist tree of data. ")
  (:method :before (method)
	   (unless *sensors-version* (sensors-version))))

(defun fetch-default-sensor-data (&optional
				    (method *default-sensors-data-method*))
  "Returns an alist of data from a call to *SENSORS-BINARY* using the
*DEFAULT-SENSORS-DATA-METHOD*.  METHOD can be supplied to override
the default method."
  (fetch-sensor-data method))

(defun parse-default-sensor-data (&optional
				    (method *default-sensors-data-method*))
  "Fetches a parsed alist of data using the default sensors method."
  (let ((alist (fetch-default-sensor-data method)))
    (unless alist
      (error "Failed to fetch default sensor data."))
    (parse-sensor-data alist)))
