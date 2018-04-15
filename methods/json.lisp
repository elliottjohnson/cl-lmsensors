(in-package :cl-lmsensors)

(defmethod fetch-sensor-data ((method (eql :json)))
    (parse (lm_sensors "-j"
		    "Failed to access json when executing '~A -j'.  Does your binary version support JSON? '~A'"
		    *sensors-binary*
		    *sensors-version*)
	   :as :alist))
