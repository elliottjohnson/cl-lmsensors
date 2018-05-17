;;;; package.lisp

(defpackage #:cl-lmsensors
  (:use #:cl #:cffi #:cl-ppcre #:dynamic-mixins)
  (:nicknames #:lm_sensors)
  (:export

   ;; exports for the libsensors api:
   ;;   some of these functions pass/accept foreign objects, but
   ;;   do provide error handing, memory management, and other low
   ;;   level coding to make a more lisp like interface.

   ;; The following exported symbols are to allow others to do low level
   ;;   programming with this library akin to directly coding with libsensors.
   #:*libsensors-version*
   #:*lm-sensors-config-filename*

   #:+sensors-api-version+ ; in octal
   #:+sensors-chip-name-prefix-any+
   #:+sensors-chip-name-address-any+

   #:+sensors-bus-type-any+
   #:+sensors-bus-type-i2c+
   #:+sensors-bus-type-isa+
   #:+sensors-bus-type-pci+
   #:+sensors-bus-type-spi+
   #:+sensors-bus-type-virtual+
   #:+sensors-bus-type-acpi+
   #:+sensors-bus-type-hid+
   #:+sensors-bus-type-mdio+
   #:+sensors-bus-nr-any+
   #:+sensors-bus-nr-ignore+

   #:sensors-bus-id
   #:sensors-chip-name

   #:sensors-feature-type    ; enumeration
   #:sensors-subfeature-type ; enumeration

   #:sensors-feature
   #:sensors-subfeature
   
   #:sensors-error
   #:c-file-handle-error
   #:sensors-initialization-error
   #:sensors-parse-error
   #:sensors-subfeature-error
   #:sensors-set-subfeature-value-error
   
   #:with-foreign-file-pointer
   #:with-parsed-chip-name

   #:sensors-init
   #:sensors-cleanup
   #:sensors-get-chip-name
   #:sensors-get-adapter-name
   #:sensors-get-detected-chips
   #:sensors-get-features
   #:sensors-get-label
   #:sensors-get-all-subfeatures
   #:sensors-get-subfeature
   #:sensors-get-value
   #:sensors-set-value
   #:sensors-do-chip-sets

   ;; High level CLOS interface
   
   #:chip
   #:chip.name
   #:chip.adapter
   #:chip.features
   
   #:feature
   #:feature.name
   #:feature.value
   #:feature.units
   ;; Other feature accessors are built and exported dynamically.
  
   #:get-detected-chips  ; main function to call
   #:find-chip-feature   ; a way of selecting a chip feature.
   
   ;; Feature mixins
   #:lmsensors-mixin
   #:feature-mixin
   #:voltage-mixin
   #:fan-mixin
   #:pwm-mixin
   #:temperature-mixin
   #:current-mixin
   #:power-mixin
   #:energy-mixin
   #:humidity-mixin
   #:alarm-mixin
   #:intrusion-mixin

   ;; Extras
   #:with-foreign-file-pointer))
