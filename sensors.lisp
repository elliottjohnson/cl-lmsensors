(in-package :cl-lmsensors)

(defvar *lm-sensors-config-filename*
  nil
  "A variable that holds a path to a libsensors configuration file, or NIL
to use system defaults.")

(define-condition sensors-error (error)
  ((return-value :initarg :return-value
		 :reader return-value
		 :documentation
		 "A C style return value passable to CFFI-SENSORS-STRERROR."))
  (:report (lambda (condition stream)
	     (format stream
		     "Error '~A'"
		     (cffi-sensors-strerror (return-value condition)))))
  (:documentation "A base class for all sensors errors."))

(define-condition c-file-handle-error (error)
  ((openp :initarg :openp
	  :reader openp
	  :documentation "A boolean value that is true when the file is open.")
   (name-string :initarg :name-string
		:reader name-string
		:documentation
		"A file name string to open.")
   (file-mode :initarg :file-mode
	      :reader file-mode
	      :documentation "A file access mode string passed to CFFI-FOPEN.")
   (file-pointer :initarg :file-pointer
		 :reader file-pointer
		 :documentation
		 "A C file pointer as returned by CFFI-FOPEN."))
  (:report (lambda (condition stream)
	     (format stream
		     "Error ~:[closing~;opening~] file '~A', mode '~A', with file pointer '~A'."
		     (openp condition)
		     (name-string condition)
		     (file-mode condition)
		     (file-pointer condition)))))

(defmacro with-foreign-file-pointer ((pointer filespec &key (mode "r"))
				     &body body)
  "Opens FILESPEC using MODE and binds POINTER to a C file pointer before
excuting BODY.  Ensures the file pointer is closed upon exit."
  `(let ((,pointer (gensym)))
     (unwind-protect
	  (progn (setf ,pointer (cffi-fopen ,filespec ,mode))
		 (when (pointer-eq (null-pointer) ,pointer)
		   (error 'c-file-handle-error
			  :openp t
			  :name-string ,filespec
			  :file-mode ,mode
			  :file-pointer ,pointer))
		 ,@body)
       (unless (= 0 (cffi-fclose ,pointer))
	 (error 'c-file-handle-error
		:openp nil
		:name-string ,filespec
		:file-mode ,mode
		:file-pointer ,pointer)))))

(define-condition sensors-initialization-error (sensors-error)
  ((config-file :initarg :config-file
		:reader config-file
		:initform nil))
  (:report (lambda (condition stream)
	     (format stream
		     "Failed to initialize sensor config: '~A' with error '~A'"
		     (or (config-file condition) "System defaults")
		     (cffi-sensors-strerror (return-value condition))))))

(defun sensors-init (&optional (config-file *lm-sensors-config-filename*))
  "Loads the configuration file, CONFIG-FILE, and the detected chips list.
If you want to reload the configuration file or load a different configuration
file, call SENSORS-CLEANUP before calling SENSORS-INIT again.  This means you
can't load multiple configuration files at once by calling SENSORS-INIT
multiple times."
  (let ((return-value (if config-file
			  (with-foreign-file-pointer (pointer config-file)
			    (cffi-sensors-init pointer))
			  (cffi-sensors-init (null-pointer)))))
      (if (zerop return-value)
	  t
	  (error 'sensors-initialization-error
		 :config-file config-file
		 :return-value return-value))))

(defun sensors-cleanup ()
  "Cleans everything up.  You can't access anything after this, until the
next SENSORS-INIT call.  Returns no value."
  (cffi-sensors-cleanup))

(define-condition sensors-parse-error (sensors-error)
  ((name-string :initarg :name-string
		:initform nil
		:reader name-string
		:documentation "A string to be parsed into a chip name."))
  (:report (lambda (condition stream)
	     (format stream
		     "Parse error in chip name: '~A' error description'~A'"
		     (name-string condition)
		     (cffi-sensors-strerror (return-value condition)))))
  (:documentation
   "An error returned if call to CFFI-SENSORS-PARSE-CHIP-NAME fails."))

(defmacro with-parsed-chip-name ((variable
				  sensors-chip-name
				  &optional name-string)
				 &body body)
  "Binds a parsed chip name to VARIABLE based upon chip NAME-STRING and a 
foreign SENSORS-CHIP-NAME struct.  BODY is executed and any parsed
SENSOR-CHIP-NAMES are auto cleaned up."
  (let ((return-value (gensym)))
    `(let ((,variable nil))
       (if ,name-string
	   (unwind-protect
		(progn
		  (let ((,return-value
			 (cffi-sensors-parse-chip-name ,name-string
						       ,sensors-chip-name)))
		    (when (< 0 ,return-value)
		      (error 'sensors-parse-error
			     :name-string ,name-string
			     :return-value ,return-value)))
		  (setf ,variable ,sensors-chip-name)
		  ,@body)
	     (cffi-sensors-free-chip-name ,variable))
	   (progn
	     (setf ,variable (null-pointer))
	     ,@body)))))

(defvar *default-snprintf-buffer-size* 256
  "Default size of a character buffer for cffi-sensors-snprintf-chip-name.")

(defun sensors-get-chip-name (sensors-chip-name)
  "Returns a chip name from its internal representation.  Note that chip
should not contain wildcard values!"
  (with-foreign-objects ((str :char *default-snprintf-buffer-size*)
			 (size 'size-t))
    (setf (mem-aref size 'size-t) *default-snprintf-buffer-size*)
    (let ((return-value
	   (cffi-sensors-snprintf-chip-name str size sensors-chip-name)))
      (unless (> return-value 0)
	(error 'sensors-error
	       :return-value return-value)))
    (foreign-string-to-lisp str)))

(defun sensors-get-adapter-name (sensors-chip-name)
  "Returns the adapter name of a bus type, number pair, as used within
the SENSORS-CHIP-NAME structure.  If it could not be found, it returns nil."
  ;; Currently this is unsupported behaviour by the public API.
  ;;  See issue #102: https://github.com/groeck/lm-sensors/issues/102
  (cffi-sensors-get-adapter-name
   (foreign-slot-pointer sensors-chip-name
			 '(:struct sensors-chip-name)
			 'bus)))

(defun sensors-get-detected-chips (&optional name-string)
  "Returns an alist of detected chips that match an optional NAME-STRING.  
If NAME-STRING is null, then all detected chips are returned."
  (assert (or (null name-string) (stringp name-string)))
  (with-foreign-objects ((name '(:struct sensors-chip-name))
			 (nr :int))
    (setf (mem-aref nr :int) 0)
    (with-parsed-chip-name (chip-pointer name name-string)
      (loop for chip = (cffi-sensors-get-detected-chips chip-pointer nr)
	 while (not (or (null chip) (pointer-eq chip (null-pointer))))
	 collect (list
		  :name (sensors-get-chip-name chip)
		  :adapter (sensors-get-adapter-name chip)
		  :features (sensors-get-features chip))))))

(defun not-null-pointer-p (pointer)
  "An internal function that returns true if POINTER is a foreign-pointer
and is not equal to NULL-POINTER."
  (assert (or (null pointer) (typep pointer 'foreign-pointer)))
  (and pointer (not (pointer-eq pointer (null-pointer)))))

(defun sensors-get-features (sensors-chip-name)
  "Returns an alist of all main features for foreign struct 
SENSORS-CHIP-NAME."
  (with-foreign-object (a :int)
    (setf (mem-aref a :int) 0)
    (loop for feature = (cffi-sensors-get-features sensors-chip-name a)
       while (not-null-pointer-p feature)
       collect (list :label (sensors-get-label sensors-chip-name feature)
		     :subfeatures (sensors-get-all-subfeatures
				   sensors-chip-name
				   feature)))))

(defun sensors-get-label (sensors-chip-name feature)
  "Looks up the label which belongs to foreign struct FEATURE of struct 
SENSORS-CHIP-NAME.  Note that SENSORS-CHIP-NAME should not be based on
parsing wildcard string names!  On failure nil is returned.  If no label 
exists for this feature, its name is returned."
  (cffi-sensors-get-label sensors-chip-name feature))

(defun sensors-get-all-subfeatures (sensors-chip-name
				    feature
				    &optional (function #'subfeature-alist))
  "Returns an alist of subfeatures of a given foreign FEATURE struct.  
VAL is an internally used foreign :DOUBLE.  SENSORS-CHIP-NAME is 
also an internally used foreign struct.  FUNCTION is a function 
that accepts a SENSORS-CHIP-NAME object and a FEATURE object & 
should return a value to be collected by SENSORS-GET-ALL-SUBFEATURES."
  (assert (and (typep sensors-chip-name 'foreign-pointer)
	       (typep feature 'foreign-pointer)
	       (functionp function)))
  (with-foreign-object (nr :int)
    (setf (mem-aref nr :int) 0)
    (loop for subfeature = (cffi-sensors-get-all-subfeatures sensors-chip-name
							     feature
							     nr)
       while (not-null-pointer-p subfeature)
       collect (funcall function sensors-chip-name subfeature))))

(defun subfeature-alist (sensors-chip-name subfeature)
  "An internally used function to parse and group subfeatures into an alist."
  (with-foreign-object (val :double)
    (with-foreign-slots ((name number type mapping flags)
			 subfeature
			 (:struct sensors-subfeature))
      (sensors-get-value sensors-chip-name number val)
      (list :name name
	    :number number
	    :type type
	    :mapping mapping
	    :flags flags
	    :value (mem-aref val :double)))))

(define-condition sensors-subfeature-error (sensors-error)
  ((chip :initarg :chip :reader chip
	 :documentation "A foreign SENSORS-CHIP-NAME struct pointer.")
   (feature :initarg :feature :reader feature :initform nil
	    :documentation "A foreign SENSORS-FEATURE struct pointer.")
   (type :initarg :type :reader subfeature-type :initform nil
	 :documentation
	 "An integer value & member of SENSORS-SUBFEATURE-TYPE foreign enum."))
  (:report (lambda (condition stream)
	     (format stream
		     "Getting type '~A' for feature '~A' of chip '~A'."
		     (subfeature-type condition)
		     (feature condition)
		     (chip condition))
	     (when (return-value condition)
	       (format stream
		       "Error string: '~A'."
		       (cffi-sensors-strerror (return-value condition))))))
  (:documentation
   "A condition created when getting a subfeature type from a feature."))

(defun sensors-get-subfeature (sensors-chip-name feature type)
  "Returns an alist of a subfeature of given subfeature TYPE for foreign
SENSORS-CHIP-NAME struct.  See the SENSORS-SUBFEATURE-TYPE foreign 
enumeration for subfeature types."
  (let ((subfeature (cffi-sensors-get-subfeature sensors-chip-name
						 feature
						 type)))
    (unless (not-null-pointer-p subfeature)
      (error 'sensors-subfeature-error
	     :chip sensors-chip-name
	     :feature feature
	     :type type))
    (subfeature-alist sensors-chip-name subfeature)))

(defun sensors-get-value (sensors-chip-name subfeature-type val)
  "Reads the value of a subfeature of a foreign SENSORS-CHIP-NAME of 
SUBFEATURE-TYPE into VAL which should be a foreign :DOUBLE value.."
  (let ((err (cffi-sensors-get-value sensors-chip-name subfeature-type val)))
    (unless (zerop err)
      (error 'sensors-subfeature-error
	     :chip sensors-chip-name
	     :feature subfeature-type
	     :value val
	     :return-value err))
    (mem-aref val :double)))

(define-condition sensors-set-subfeature-value-error (sensors-subfeature-error)
  ((value :initarg :value :initform nil :reader value
	  :documentation "The value to set as the sensor subfeature's value."))
  (:report (lambda (condition stream)
	     (format stream
		     "Error: '~A' for chip '~A' subfeature '~A' to value '~A'" 
		     (cffi-sensors-strerror (return-value condition))
		     (chip condition)
		     (subfeature-type condition)
		     (value condition))))
  (:documentation "An error regarding the setting a subfeature value."))

(defun sensors-set-value (sensors-chip-name subfeature-type value)
  "Sets the value of SUBFEATURE-TYPE of a certain SENSORS-CHIP-NAME struct
to VALUE.  SUBFEATURE-TYPE is an integer value belonging to the foreign 
SENSORS-SUBFEATURE-TYPE enumeration."
  (assert (and (typep sensors-chip-name 'foreign-pointer)
	       (integerp subfeature-type)
	       (numberp value)))
  (with-foreign-object (val :double)
    (setf (mem-aref val :double) value)
    (let ((err (cffi-sensors-set-value sensors-chip-name subfeature-type val)))
      (if (zerop err)
	  t
	  (error 'sensors-set-subfeature-value-error
		 :chip sensors-chip-name
		 :type subfeature-type
		 :value value
		 :return-value err)))))

(defun sensors-do-chip-sets (&optional sensors-chip-name)
  "Executes all set statements for this particular chip.  The chip may
contain wildcards!"
  (let ((return-value
	 (if sensors-chip-name
	     (let ((string-length (1+ (length sensors-chip-name))))
	       (with-foreign-object (name :string string-length)
		 (when (stringp sensors-chip-name)
		   (lisp-string-to-foreign sensors-chip-name
					   name
					   string-length))
		 (cffi-sensors-do-chip-sets name)))
	     (cffi-sensors-do-chip-sets (null-pointer)))))
    (if (zerop return-value)
	t
	(error 'sensors-set-subfeature-value-error
	       :chip sensors-chip-name
	       :return-value return-value))))
