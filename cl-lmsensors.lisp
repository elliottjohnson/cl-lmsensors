(in-package :cl-lmsensors)

(defclass chip ()
  ((name :accessor chip.name :initarg :name :initform nil
	 :documentation "The chip name string.")
   (adapter :accessor chip.adapter :initarg :adapter :initform nil
	    :documentation "The chip adapter name string.")
   (features :accessor chip.features :initarg :features :initform ()
	     :documentation "A list of feature objects for this chip."))
  (:documentation "A class of objects to represent chip data."))

(defmethod print-object ((chip chip) stream)
  (print-unreadable-object (chip stream :type t :identity t)
    (with-accessors ((name chip.name)
		     (features chip.features))
	chip
      (when name
	(format stream "~A" name))
      (when features
	(format stream " : ~A" features)))))

(defclass feature ()
  ((name :accessor feature.name :initarg :label :initform nil
	 :documentation "The feature name string.")
   (value :accessor feature.value :initarg :value :initarg nil
	  :documentation "A numeric value for this feature."))
  (:documentation
   "A basic feature class.  Includes a VALUE slot accessable by VALUE."))

(defmethod print-object ((feature feature) stream)
  (print-unreadable-object (feature stream :identity t)
    (with-accessors ((name feature.name)
		     (value feature.value))
	feature
      (format stream "FEATURE")
      (when name
	(format stream " ~A" name))
      (when value
	(format stream " = ~A" value)))
    (when (and (slot-exists-p feature 'units)
	       (feature.units feature))
      (format stream " ~A" (feature.units feature)))))

(defun get-detected-chips (&optional chip-name)
  "Returns a list of CHIP objects populated with data from detected chips.
If CHIP-NAME is specified and as a string, a specific chip will be
processed.  If CHIP-NAME is a list of strings then the specified
chips will be processed.  If CHIP-NAME is an empty list (ie. nil)
then all detected chips will be processed."
  (assert (or (null chip-name)
	      (stringp chip-name)
	      (and (listp chip-name)
		   (every #'stringp chip-name))))
  (let ((chips (if (or (null chip-name)
		       (stringp chip-name))
		   (sensors-get-detected-chips chip-name)
		   (mapcar (lambda (chip)
			     (car (sensors-get-detected-chips chip)))
			   chip-name))))
    (if (null chips)
	(warn "Failed to fetch any chips.. did you execute (SENSORS-INIT)?")
	(loop for chip in chips
	   while chip
	   collect (apply #'make-instance 'chip chip)))))

(defmethod initialize-instance :after ((chip chip) &key)
  (with-accessors ((features chip.features))
      chip
    (setf features
	  (mapcar (lambda (feature)
		    (apply #'make-instance 'feature feature))
		  features))))

(defmethod initialize-instance :after ((feature feature) &key subfeatures)
  (loop for subfeature in subfeatures
     do (let ((name (getf subfeature :name)))
	  (set-feature-class feature name)
	  (set-subfeature-data feature name subfeature)))
  (set-computed-data feature))

(defun parse-feature-string (string)
  (assert (stringp string))
  (macrolet ((grouping-parse (var-list regexp)
	       `(register-groups-bind ,(remove-if #'null var-list)
		    (,regexp string)
		  (mapcar (lambda (v) (when v
					(if (some #'alpha-char-p v)
					    (intern (string-upcase v))
					    (parse-integer v))))
			  (list ,@var-list)))))  ; Tempted to double backtick
    (or (grouping-parse (feature number subfeature)
			"([a-z]+)([0-9]+)_([a-z_]+)")
	(grouping-parse (feature nil subfeature)
			"([a-z]+)_([a-z_]+)")
	(grouping-parse (feature nil nil)
			"([a-z]+)"))))

(defgeneric set-feature-class (feature class)
  (:method (feature identifier)
    (error "Failed to find a feature-class '~A' based on subfeature: '~A'."
	   feature
	   identifier))
  (:method (feature (class string))
    (destructuring-bind (f n s)
	(parse-feature-string class)
      (declare (ignore n s))
      (set-feature-class feature f)
      ;; Kluge to work around the fact that PWMs are in a "temp" subfeature.
      (when (scan "auto_point" class)
	(ensure-mix 'pwm-mixin))))
  (:documentation
   "A generic function to map an identifier to a feature-class symbol."))

(defgeneric set-subfeature-data (feature name subfeature)
  (:method (feature name subfeature)
    (error "Failed to set feature data, '~A' of subfeature '~A' for feature '~A'."
	   name
	   subfeature
	   feature))
  (:method ((feature feature) (name string) subfeature)
    (destructuring-bind (f n s)
	(parse-feature-string name)
      (declare (ignore f n))
      (set-subfeature-data feature s subfeature)))
  (:documentation
   "A generic fuction to set a feature's subfeature data."))

(defgeneric set-computed-data (feature)
  (:documentation
   "Performs a series of calculations and setups computed values.
This function is called at the end of object initialization.
The method combination is PROGN to ensure all methods are
run.")
  (:method-combination progn)
  (:method progn ((feature feature))
	   feature))

;;; Define mixins.

(defclass lmsensors-mixin ()
  ()
  (:documentation "A class of mixins related to lm_sensors."))

(defclass feature-mixin ()
  ((units :initarg :units :accessor feature.units :initform nil))
  (:documentation "The parent class of all feature mixins."))

(defclass subfeature-mixin ()
  ()
  (:documentation "A parent class of all subfeatures of a feature."))

;;; Feature level mixins

;;  Voltage

(defclass voltage-mixin (feature-mixin)
  ((units :initform 'volt))
  (:documentation "A mixin for all voltage features."))

;; Several subfeatures for voltage "in", "cpu", and "vrm"

;; I wish there was an (or (eql 'symbol1) (eql 'symbol2)) style allowed.
(defmethod set-feature-class ((feature feature) (class (eql 'in)))
  (ensure-mix feature 'voltage-mixin))
(defmethod set-feature-class ((feature feature) (class (eql 'cpu)))
  (set-feature-class feature 'in))
(defmethod set-feature-class ((feature feature) (class (eql 'vrm)))
  (set-feature-class feature 'in))

;; Fan rpms

(defclass fan-mixin (feature-mixin)
  ((units :initform  'revolution/min))
  (:documentation "A mixin for handing cooling fan sensors"))

(defmethod set-feature-class ((feature feature) (class (eql 'fan)))
  (ensure-mix feature 'fan-mixin))

;; PWM for fan control

(defclass pwm-mixin (feature-mixin)
  ()
  (:documentation "A mixin for handling pulse width modulation."))

(defmethod set-feature-class ((feature feature) (class (eql 'pwm)))
  (ensure-mix feature 'pwm-mixin))

;; Temperature

(defclass temperature-mixin (feature-mixin)
  ((units :initform 'celsius))
  (:documentation "A mixin for handling temperature sensors."))

(defmethod set-feature-class ((feature feature) (class (eql 'temp)))
  (ensure-mix feature 'temperature-mixin))

;; Current

(defclass current-mixin (feature-mixin)
  ((units :initform 'amps))
  (:documentation "A mixin for handling current sensors."))

(defmethod set-feature-class ((feature feature) (class (eql 'curr)))
  (ensure-mix feature 'current-mixin))

;; Power

(defclass power-mixin (feature-mixin)
  ((units :initform 'watt))
  (:documentation "A mixin for handling power sensors."))

(defmethod set-feature-class ((feature feature) (class (eql 'power)))
  (ensure-mix feature 'power-mixin))

;; Energy

(defclass energy-mixin (feature-mixin)
  ((units :initform 'joule))
  (:documentation "A mixin for handling energy sensors."))

(defmethod set-feature-class ((feature feature) (class (eql 'energy)))
  (ensure-mix feature 'energy-mixin))

;; Humidity

(defclass humidity-mixin (feature-mixin)
  ((units :initform 'percent))
  (:documentation "A mixin for handling humidity sensors."))

(defmethod set-feature-class ((feature feature) (class (eql 'humidity)))
  (ensure-mix feature 'humidity-mixin))

;; alarms

(defclass alarm-mixin (feature-mixin subfeature-mixin)
  ()
  (:documentation "A mixin for handling sensor alarms."))

;; Alarms are handled differently than other classes,
;; since alarams are differnt for each type of sensor.
;; "_alarm$"
;; "_fault$"
;; "_beep$"

(defmethod set-feature-class ((feature feature) (class (eql 'beep)))
   (ensure-mix feature 'alarm-class))  ; The only alarm that does not match a subfeature.

;;  Not supporting old alarm bitmasks or beep_masks... sorry, should be easy to add if needed.

;; Intrusion detection

(defclass intrusion-mixin (feature-mixin)
  ()
  (:documentation "Chassis intrusion detection handling."))

(defmethod set-feature-class ((feature feature) (class (eql 'intrusion)))
  (ensure-mix feature 'intrusion-mixin))

;;; Subfeature mixins
(eval-when (:compile-toplevel)
  (defun mixin-name (symbol)
    (intern (concatenate 'string
			 (symbol-name symbol)
			 "-MIXIN")))
  
  (defun accessor-name (symbol &optional percent)
    (intern (concatenate 'string
			 "FEATURE."
			 (symbol-name symbol)
			 (if percent "%" ""))))
  
  (defun percent-name (symbol)
    (intern (concatenate 'string (symbol-name symbol) "%")))
  
  (defun percent-var (symbol)
    (list symbol (accessor-name symbol))))

(defmacro generate-subfeature-mixin (symbol &optional percent)
  "Creates new mixins and methods for subfeatures."
  `(progn
     (defclass ,(mixin-name symbol) (subfeature-mixin)
       ((,symbol :accessor ,(accessor-name symbol)
		 :initform nil)
	,@(when percent
	    `((,(percent-name symbol) :accessor ,(accessor-name symbol t)
		:initform nil)))))
     (export ',(mixin-name symbol))
     (export ',(accessor-name symbol))
     ,@(when percent
	 `((export ',(accessor-name symbol))))
     (defmethod set-subfeature-data ((feature feature)
				     (name (eql ',symbol))
				     (subfeature list))
       (ensure-mix feature ',(mixin-name symbol))
       (setf (,(accessor-name symbol) feature) (getf subfeature :value)))
     ,@(when percent
	 `((defmethod set-computed-data progn ((feature ,(mixin-name symbol)))
		      (setf (,(accessor-name symbol t) feature)
			    ,(if (third percent)
				 `(if (typep feature ',(mixin-name (third percent)))
				      ;; if we have a minimum value we take that into consideration.
				      (with-accessors ,(mapcar #'percent-var percent)
					  feature
					(let ((diff-value (- ,(first percent) ,(third percent)))
					      (diff-total (- ,(second percent) ,(third percent))))
					  (unless (zerop diff-total)
					    (/ diff-value diff-total))))
				      ;; If not we do a true percent value.
				      (with-accessors ,(mapcar #'percent-var (butlast percent))
					  feature
					(unless (zerop ,(second percent))
					  (/ ,(first percent)
					     ,(second percent)))))
				 `(with-accessors ,(mapcar #'percent-var percent)
				      feature
				    (unless (zerop ,(second percent))
				      (/ ,(first percent)
					 ,(second percent)))))))))))


;; min
(generate-subfeature-mixin min)

;; lcrit
(generate-subfeature-mixin lcrit)

;; max
(generate-subfeature-mixin max (value max min))

;; crit
(generate-subfeature-mixin crit (value crit lcrit))

;; input -> see default method for feature
(defmethod set-subfeature-data ((feature feature)
				(name (eql 'input))
				(subfeature list))
  (setf (feature.value feature) (getf subfeature :value)))

;; average 
(generate-subfeature-mixin average (value average))

;; lowest
(generate-subfeature-mixin lowest)

;; highest
(generate-subfeature-mixin highest (value highest lowest))

;; reset_history
(generate-subfeature-mixin reset_history)


;; Label 
(generate-subfeature-mixin label)

;; vid
(generate-subfeature-mixin vid)

;; div - for fams, this is the divisor
(generate-subfeature-mixin div)

;; pulses -- The number of tachometer pulses per fan revolution.
(generate-subfeature-mixin pulses)

;; target -- the desired fan speed
(generate-subfeature-mixin target (value target min))

;; pwm -- who designed this feature!  so many exceptions.

;; The pwm value is stored in just a pwm[1-*] entry... whaaa?
(defmethod set-subfeature-data ((feature pwm-mixin)
				(name null)
				(subfeature list))
  (setf (feature.value feature) (getf subfeature :value)))

;; Enable -- 0 = No fan control, 1 = manual, 2+ = auto enabled -- todo make this nicer.
(generate-subfeature-mixin enable)

;; mode -- 0 = DC mode, 1 = PWM mode
(generate-subfeature-mixin mode)

;; freq
(generate-subfeature-mixin freq)

;; auto-channels-temp
(generate-subfeature-mixin auto_channels_temp)

;; auto_point is handled in the around method due to needed a string to parse.

;; type -- some temps have 1 = CPU embedded code,
;;                         2 = 3904 thermal diode.
;;                         3 = thermal diode.
;;                         4 = thermistor
;;                         5 = AMD AMDSI
;;                         6 = Intel PECI
(generate-subfeature-mixin type)

;; min_hyst 
(generate-subfeature-mixin min_hyst)

;; max_hyst 
(generate-subfeature-mixin max_hyst (value max_hyst min_hyst))

;; lcrit_hyst
(generate-subfeature-mixin lcrit_hyst)

;; crit_hyst
(generate-subfeature-mixin crit_hyst (value crit_hyst lcrit_hyst))

;; emergency
(generate-subfeature-mixin emergency)

;; emergency_hyst
(generate-subfeature-mixin emergency_hyst (value emergency_hyst lcrit_hyst))

;; average_interval
(generate-subfeature-mixin average_interval)

;; average_interval_min
(generate-subfeature-mixin average_interval_min)

;; average_interval_max
(generate-subfeature-mixin average_interval_max
			   (average_interval
			    average_interval_max
			    average_interval_min))

;; average_lowest
(generate-subfeature-mixin average_lowest)

;; average_highest
(generate-subfeature-mixin average_highest
			   (average average_highest average_lowest))

;; average_min
(generate-subfeature-mixin average_min)

;; average max
(generate-subfeature-mixin average_max (average average_max average_min))

;; input_lowest
(generate-subfeature-mixin input_lowest)

;; input_highest
(generate-subfeature-mixin input_highest (value input_highest input_lowest))

;; accuracy
(generate-subfeature-mixin accuracy)

;; cap
(generate-subfeature-mixin cap)

;; cap_hyst
(generate-subfeature-mixin cap_hyst)

;; cap_min
(generate-subfeature-mixin cap_min)

;; cap_max
(generate-subfeature-mixin cap_max (cap cap_max cap_min))

;; alarm
(generate-subfeature-mixin alarm)

;; min_alarm
(generate-subfeature-mixin min_alarm)

;; max_alarm
(generate-subfeature-mixin max_alarm (alarm max_alarm min_alarm))

;; lcrit_alarm
(generate-subfeature-mixin lcrit_alarm)

;; crit_alarm
(generate-subfeature-mixin crit_alarm (value crit_alarm lcrit_alarm))

;; cap_alarm
(generate-subfeature-mixin cap_alarm)

;; emergency_alarm
(generate-subfeature-mixin emergency_alarm)

;; fault
(generate-subfeature-mixin fault)

;; beep
(generate-subfeature-mixin beep)

(defun find-chip-feature (chip-list chip-name feature-name)
  "Accepts a list of chips and returns a feature that has FEATURE-NAME
and is part of CHIP-NAME or nil otherwise."
  (let ((chip (find-if (lambda (c)
			 (string-equal (chip.name c) chip-name))
		       chip-list)))
    (when chip
      (find-if (lambda (f)
		 (string-equal (feature.name f) feature-name))
	       (chip.features chip)))))


