(in-package :cl-lmsensors)

(register-sensors-data-method :json)

(setf *default-sensors-data-method* :json)

(defmethod fetch-sensor-data ((method (eql :json)))
    (parse (sensors "-j"
		    "Failed to access json from ~A.  Does your binary support JSON? '~A'"
		    *sensors-binary*
		    *sensors-version*)
	   :as :alist))
