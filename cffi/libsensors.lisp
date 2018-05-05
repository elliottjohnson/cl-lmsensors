(in-package :cl-lmsensors)

(define-foreign-library libsensors
  (:unix (:or "libsensors.so.4" "libsensors.so"))
  (t (:default "libsensors.so")))

(use-foreign-library libsensors)

(defcvar "libsensors_version" :string)

(defcfun ("sensors_init" cffi-sensors-init) :int
  "Loads the configuration file and the detected chips list.  If this
returns a value unequal to zero, you are in trouble.  To reload the
configuration call SENSORS-CLEANUP before calling SENSORS-INIT again."
  (file :pointer))

(defcfun ("sensors_cleanup" cffi-sensors-cleanup) :void
  "Clean-up function: You can't access anything after this, until the next 
sensors_init() call!")

(defcfun ("sensors_parse_chip_name" cffi-sensors-parse-chip-name) :int
  "Parse a chip name to the internal representation.  Return 0 on success,
<0 on error."
  (orig-name :string)
  (res (:pointer (:struct sensors-chip-name))))

(defcfun ("sensors_free_chip_name" cffi-sensors-free-chip-name) :void
  "Free memory allocated for the internal representation of a chip name."
  (chip (:pointer (:struct sensors-chip-name))))

(defcfun ("sensors_snprintf_chip_name" cffi-sensors-snprintf-chip-name) :int
  "Prints a chip name from its internal representation.  Note that chip
should not contain wildcard values!  Returns the number of characters 
printed on success (same as snprintf), <0 on error."
  (str :string)
  (size (:pointer size-t))
  (chip (:pointer (:struct sensors-chip-name))))

(defcfun ("sensors_get_adapter_name" cffi-sensors-get-adapter-name) :string
  "This function returns the adapter name of a bus, as used within the 
sensors_chip_name structure. If it could not be found, it returns NULL"
  (bus (:pointer (:struct sensors-bus-id))))

(defcfun ("sensors_get_detected_chips" cffi-sensors-get-detected-chips)
    (:pointer (:struct sensors-chip-name))
  (match (:pointer (:struct sensors-chip-name)))
  (nr (:pointer :int)))

(defcfun ("sensors_get_features" cffi-sensors-get-features)
    (:pointer (:struct sensors-feature))
  "This returns all main features of a specific chip.  NR is an internally
used variable.  Set it to zero to start at the begining of the list.  If
no more features are found NULL is returned.  Do not try to change the
returned structure, you will corrupt internal data structures."
  (name (:pointer (:struct sensors-chip-name)))
  (nr (:pointer :int)))

(defcfun ("sensors_get_all_subfeatures" cffi-sensors-get-all-subfeatures) (:pointer (:struct sensors-subfeature))
  "This returns all subfeatures of a given main feature. nr is an internally
used variable. Set it to zero to start at the begin of the list. If no more 
features are found NULL is returned.  Do not try to change the returned 
structure; you will corrupt internal data structures."
  (name (:pointer (:struct sensors-chip-name)))
  (feature (:pointer (:struct sensors-feature)))
  (nr (:pointer :int)))

(defcfun ("sensors_get_subfeature" cffi-sensors-get-subfeature)
    (:pointer (:struct sensors-subfeature))
    "This function returns a subfeature of the given type for a given feature, 
if it exists, NULL otherwise."
  (name (:pointer (:struct sensors-chip-name)))
  (feature (:pointer (:struct sensors-feature)))
  (type sensors-subfeature-type))

(defcfun ("sensors_get_label" cffi-sensors-get-label) :string
  "Look up the label for a given feature.  Note that chip should not contain
wildcard values!  The returned string is newly allocated (free it yourself).
On failure, NULL is returned.  If no label exists for this feature, its name
is returned itself."
  (name (:pointer (:struct sensors-chip-name)))
  (feature (:pointer (:struct sensors-feature))))

(defcfun ("sensors_get_value" cffi-sensors-get-value) :int
  "Read the value of a subfeature of a certain chip.  Note that chip should 
not container wildcard values!  This function will return 0 on success, and
<0 on failure."
  (name (:pointer (:struct sensors-chip-name)))
  (subfeature-number :int)
  (value (:pointer :double)))

(defcfun ("sensors_set_value" cffi-sensors-set-value) :int
  "Set the value of a subfeature of a certain chip.  Note that chip should
not contain wildcard values!  This function will return 0 on success, and
<0 on failure."
  (name (:pointer (:struct sensors-chip-name)))
  (subfeature-number :int)
  (value :double))

(defcfun ("sensors_do_chip_sets" cffi-sensors-do-chip-sets) :int
  "Execute all set statements for this particular chip. The chip may contain
wildcards!  This function will return 0 on success, and <0 on failure."
  (name (:pointer (:struct sensors-chip-name))))

(defcfun ("sensors_strerror" cffi-sensors-strerror) :string
  (errnum :int))

(defcfun ("sensors_parse_error" cffi-sensors-parse-error) :pointer
  (err :string)
  (lineno :int))

(defcfun ("sensors_parse_error_wfn" cffi-sensors-parse-error-wfn) :pointer
  (err :string)
  (filename :string)
  (lineno :int))

(defcfun ("sensors_fatal_error" cffi-sensors-fatal-error) :pointer
  (proc :string)
  (err :string))

(defcfun ("fopen" cffi-fopen) :pointer
  (path :string)
  (mode :string))

(defcfun ("fclose" cffi-fclose) :int
  (file :pointer))
