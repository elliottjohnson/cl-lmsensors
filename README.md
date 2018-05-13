# cl-lmsensors
##### _Elliott Johnson <elliott@elliottjohnson.net>_

CL-LMSENSORS: a utility for interacting with 
[lm_sensors](https://hwmon.wiki.kernel.org/) data.

It was built/tested against lm_sensors/libsensors version 3.4.0
and uses CFFI to load and interact with the library directly.

The library provides a low level library, see the functions
exported with the SENSORS-* prefix.  These interact directly
with foreign objects to extract the sensor data.

The library also provides parsing functions to process the
data and create CLOS objects dynamically.

### Usage Examples

    CL-USER> (ql:quickload 'cl-lmsensors)
    (CL-LMSENSORS)
    CL-USER> (in-package :cl-lmsensors)
    #<PACKAGE "CL-LMSENSORS">
    LM_SENSORS> (sensors-get-detected-chips)
    NIL
    LM_SENSORS> (sensors-init)
    T
    LM_SENSORS> (sensors-get-detected-chips)
    ((:NAME "atk0110-acpi-0" :ADAPTER "ACPI interface" :FEATURES
      ((:LABEL "Vcore Voltage" :SUBFEATURES
        ((:NAME "in0_input" :NUMBER 0 :TYPE :SENSORS-SUBFEATURE-IN-INPUT :MAPPING 0
          :FLAGS 5 :VALUE 1.28d0)
         (:NAME "in0_min" :NUMBER 1 :TYPE :SENSORS-SUBFEATURE-IN-MIN :MAPPING 0
          :FLAGS 5 :VALUE 0.8d0)
         (:NAME "in0_max" :NUMBER 2 :TYPE :SENSORS-SUBFEATURE-IN-MAX :MAPPING 0
          :FLAGS 5 :VALUE 1.6d0)))
       ...)
     ...))
    LM_SENSORS> (get-detected-chips)
    (#<CHIP atk0110-acpi-0 : (#<MIXIN-OBJECT (MAX-MIXIN MIN-MIXIN VOLTAGE-MIXIN
                                              FEATURE) {100600F793}>
    			      ...)
    ...))

### Exported functions and values

#### CFFI functions

All exported functions have documentation strings adapted from libsensors to
model the way the lisp library is implmented.  Looking up the docstrings is
suggested.

### Ideas for the future.

1) Add an input method for reading RAW data from lm_sensors. \*done\*
2) Expand the supported hardware. \*done\*
3) StumpWM modeline with temp information.
     * Useful when compiling a lot on a poorly cooled machine.
4) sensorsd type data collection running in a thread.
     * Data Storage: RRDBS?
5) Data representation
     * RRDBS has it's own tools, is there a lisp method (vecto, etc)

## License

MIT

Copyright 2018, Elliott Johnson <elliott@elliottjohnson.net>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#### References to lm-sensors (docstrings, etc) are all LGPL

LICENSE
-------

The library (libsensors) is released under the GNU Lesser General Public
License (LGPL), as included in the file COPYING.LGPL. The rest of this
package may be distributed according to the GNU General Public License
(GPL), as included in the file COPYING of the libsensors codebase.
