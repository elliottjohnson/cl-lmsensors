(in-package :cl-lmsensors)

(defvar *lm-sensors-config-filename*
  nil
  "A variable that holds a path to a libsensors configuration file, or NIL
to use system defaults.")

(defmacro with-foreign-open-file ((pointer filespec &key (mode "r")) &body body)
  `(let ((,pointer (gensym)))
     (unwind-protect
	  (progn (setf ,pointer (cffi-fopen ,filespec ,mode))
		 (when (pointer-eq (null-pointer) ,pointer)
		   (error "Failed to open file: ~A" ,filespec))
		 ,@body)
       (cffi-fclose ,pointer))))

(defun sensors-init (&optional (config-file *lm-sensors-config-filename*))
  (let ((return-value (if config-file
			  (with-foreign-open-file (pointer config-file)
			    (cffi-sensors-init pointer))
			  (cffi-sensors-init (null-pointer)))))
      (if (zerop return-value)
	  t
	  (error "Failed to open file: '~A' with error '~A'"
		 config-file
		 (cffi-sensors-strerror return-value)))))

(defun sensors-cleanup ()
  (cffi-sensors-cleanup))

(defun sensors-parse-chip-name (name-string sensors-chip-name)
  "Calls CFFI-SENSORS-PARSE-CHIP-NAME with a string, NAME-STRING and a foreign pointer, SENSORS-CHIP-NAME.
Returns SENSORS-CHIP-NAME or an error."
  (let ((parse-return (cffi-sensors-parse-chip-name name-string sensors-chip-name)))
    (when (< 0 parse-return)
      (error "Parse error in chip name: '~A' error description'~A'"
	     name-string
	     (cffi-sensors-strerror parse-return)))
    sensors-chip-name))

(defvar *default-snprintf-buffer-size* 256
  "The default size of a character buffer for cffi-sensors-snprintf-chip-name.")

(defun sensors-get-chip-name (sensors-chip-name)
  (with-foreign-objects ((str :char *default-snprintf-buffer-size*)
			 (size 'size-t))
    (setf (mem-aref size 'size-t) *default-snprintf-buffer-size*)
    (let ((return-value (cffi-sensors-snprintf-chip-name str size sensors-chip-name)))
      (unless (> return-value 0)
	(error "Failed to print chip name.  Errno '~A' meaning '~A'."
	       return-value
	       (cffi-sensors-strerror return-value))))
    (foreign-string-to-lisp str)))

(defun sensors-get-adapter-name (sensors-chip-name)
  ;; Currently this is unsupported behaviour by the public API.
  ;;  See issue #102: https://github.com/groeck/lm-sensors/issues/102
  (cffi-sensors-get-adapter-name
   (foreign-slot-pointer sensors-chip-name
			 '(:struct sensors-chip-name)
			 'bus)))

(defun sensors-get-detected-chips (&optional chip-name)
  (assert (or (null chip-name) (stringp chip-name)))
  (with-foreign-objects ((name '(:struct sensors-chip-name))
			 (nr :int))
    (setf (mem-aref nr :int) 0)
    (let ((chip-pointer (if chip-name (sensors-parse-chip-name chip-name name) (null-pointer))))
      (loop for chip = (cffi-sensors-get-detected-chips chip-pointer nr)
	 while (not (or (null chip) (pointer-eq chip (null-pointer))))
	 collect (list
		  :name (sensors-get-chip-name chip)
		  :adapter (sensors-get-adapter-name chip)
		  :features (sensors-get-features chip))))))

;; TODO add an error specializer

(defun sensors-get-features (chip-pointer)
  "Accepts a ffi :struct CHIP-POINTER and returns a plist of features and subfeatures
as returned by CFF-SENSORS-GET-FEATURES."
  (with-foreign-objects ((a :int) (b :int) (val :double))
    (setf (mem-aref a :int) 0)
    (loop for feature = (cffi-sensors-get-features chip-pointer a)
       while (and feature (not (pointer-eq feature (null-pointer))))
       collect (let ((label (sensors-get-label chip-pointer feature)))
		 (setf (mem-aref b :int) 0)
		 (list :label label
		       :subfeatures
		       (sensors-get-all-subfeatures chip-pointer feature b val))))))

(defun sensors-get-all-subfeatures (chip-pointer feature nr val)
  "Accepts a ffi :struct chip CHIP-POINTER, a ffi :struct FEATURE, a ffi :int NR, and
a ffi :double VAL and returns a plist of subfeature values. "
  (loop for subfeature = (cffi-sensors-get-all-subfeatures chip-pointer feature nr)
     while (and subfeature (not (pointer-eq subfeature (null-pointer))))
     collect (subfeature-alist chip-pointer subfeature val)))

(defun subfeature-alist (chip-pointer subfeature val)
  (with-foreign-slots ((name number type mapping flags)
		       subfeature
		       (:struct sensors-subfeature))
    (sensors-get-value chip-pointer number val name)
    (list :name name
	  :number number
	  :type type
	  :mapping mapping
	  :flags flags
	  :value (mem-aref val :double))))

(defun sensors-get-subfeature (chip-pointer feature type)
  (let ((subfeature (cffi-sensors-get-subfeature chip-pointer feature type)))
    (unless (and feature (not (pointer-eq feature (null-pointer))))
      (error "Failed to get subfeature for chip: '~A', feature '~A', and type '~A'"
	     chip-pointer feature type))
    (with-foreign-object (val :double)
      (subfeature-alist chip-pointer subfeature val))))

(defun sensors-get-label (chip-pointer feature)
  "Returns a string representation for feature or an error otherwise. "
  (let ((label (cffi-sensors-get-label chip-pointer feature)))
    (unless (stringp label)
      (error "Can't get label of feature '~A'!"
	     (foreign-slot-value feature '(:struct sensors-feature) 'name)))
    label))

(defun sensors-get-value (chip-pointer number val name)
  (let ((err (cffi-sensors-get-value chip-pointer number val)))
    (unless (zerop err)
      (error "Can't get value of subfeature '~A': '~A'"
	     name
	     (cffi-sensors-strerror err)))
    (mem-aref val :double)))

(defun sensors-set-value (chip-pointer number val)
  (let ((err (cffi-sensors-set-value chip-pointer number val)))
    (unless (zerop err)
      (error "Can't set value of '~A' subfeature '~A' to value '~A'"
	     chip-pointer
	     subfeature
	     val))
    (mem-aref val :double)))

(defun sensors-do-chip-sets (&optional sensors-chip-name)
  (let ((string-length (1+ (length sensors-chip-name))))
    (with-foreign-object (name :string string-length)
      (when (stringp sensors-chip-name)
	(lisp-string-to-foreign sensors-chip-name name string-length))
      (let ((return-value (cffi-sensors-do-chip-sets (if sensors-chip-name
							 name
							 (null-pointer)))))
	(if (zerop return-value)
	    t
	    (error "Failed to execute chip set statement when requesting chip-name '~A' error:'~A'."
		   sensors-chip-name
		   (cffi-sensors-strerror return-value)))))))
