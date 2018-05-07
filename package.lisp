;;;; package.lisp

(defpackage #:cl-lmsensors
  (:use #:cl #:cffi #:cl-ppcre #:dynamic-mixins)
  (:nicknames #:lm_sensors)
  (:export
   ;; export the high level extractions of the libsensors api.
   ;; some of these functions require instantiated objects, but
   ;; do perform error checking.
   #:*lm-sensors-config-filename*
   #:sensors-init
   #:sensors-cleanup
   #:sensors-parse-chip-name
   #:sensors-get-adapter-name
   #:sensors-get-detected-chips
   #:sensors-get-features
   #:sensors-get-all-subfeatures
   #:sensors-get-subfeature
   #:sensors-get-label
   #:sensors-get-value
   #:sensors-set-value
   #:sensors-do-chip-sets

   #:chip
   #:chip.name
   #:chip.adapter
   #:chip.features
   
   #:feature
   #:feature.name
   #:feature.value
   #:feature.units
   ;; Other feature accessors are built and exported dynamically.
  
   #:get-detected-chips
))


