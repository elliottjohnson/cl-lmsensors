(in-package :cl-lmsensors)

(defclass acpi-voltage (sensor-max-mixin
			sensor-min-mixin
			sensor-data)
  ()
  (:default-initargs :units 'volts)
  (:documentation
   "A general class for all voltages read by acpi hardware."))

(defclass acpi-fan (sensor-max-mixin
		    sensor-min-mixin
		    sensor-data)
  ()
  (:default-initargs :units 'rpms)
  (:documentation
   "A class for all fan speeds read by by acpi hardware."))

(defclass acpi-temp (sensor-critical-mixin
		     sensor-max-mixin
		     sensor-data)
  ()
  (:default-initargs :units 'celsius)
  (:documentation
   "A class for all temperatures read by acpi hardware."))

(defclass atk0110-acpi (fan-mixin
			voltage-mixin
			temperature-mixin
			hardware)
  ()
  (:default-initargs 
    :fan-class 'acpi-fan
    :voltage-class 'acpi-voltage
    :temperature-class 'acpi-temp)
  (:documentation
   "A class representing the atk0110-apci hardware."))
