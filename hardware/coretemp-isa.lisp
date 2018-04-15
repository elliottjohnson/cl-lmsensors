(in-package :cl-lmsensors)

;;; https://www.kernel.org/doc/Documentation/hwmon/coretemp

(defclass coretemp (sensor-critical-alarm-mixin
		    sensor-critical-mixin
		    sensor-max-mixin
		    sensor-data)
  ()
  (:default-initargs :units 'celsius)
  (:documentation "A coretemp data entry."))

(defclass coretemp-isa (temperature-mixin
			hardware)
  ()
  (:default-initargs :temperature-class 'coretemp)
  (:documentation
   "A class of hardware matching \"coretemp-isa-####\"."))

(defmethod sensor-class or ((coretemp coretemp-isa) (class string))
  (when (scan "^Core [0-9]+$" class)
    (temperature-class coretemp)))

