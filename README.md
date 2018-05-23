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

#### Low level API

---

*Variable* **\*LIBSENSORS-VERSION\***

**Value Type:**

a string value.

**Initial Value:**

site specific string, loaded from the C library.

**Description:**

A string representing the version of libsensors.

---

*Variable* **\*LM-SENSORS-CONFIG-FILENAME\***

**Value Type:**

A string pathname or NIL.

**Initial Value:**

NIL

**Description:**

A variable holding a pathname to a libsensors configuration file
or NIL to use site specific defaults.  The default configuration files
tried first are typically /etc/sensors3.conf, /etc/sensors.conf and
the directory /etc/sensors.d/.  See sensors.conf(5) for more info.

---

*cstruct* **SENSORS-BUS-ID**

**Description:**

A sensors-bus-id is largely an internally used foreign struct.  It
holds information about the bus/adapter used for communication with
the sensor chip.  It is mainly defined to allow
SENSORS-GET-ADAPTER-NAME  to work as designed.

---

*cstruct* **SENSORS-CHIP-NAME**

**Members:**

bus -- A foreign pointer to a sensors-bus-id struct.

**Description:**

A sensors-chip-name struct is used through out the low level interface
as an object passed between functions to facilitate the parsing of
wildcard chip names and to hold sensor chip information.  It only has
one member that is directly accessed, which is the BUS slot that
is used primarily in accessing the sensor chip's adapter/bus name via
the function SENSORS-GET-ADAPTER-NAME.

---

*cstruct* **SENSORS-FEATURE**

**Members:**

name -- A foreign string.

number -- A foreign integer.

type -- A foreign enum value of type SENSORS-FEATURE-TYPE.

**Description:**

The SENSORS-FEATURE cstruct is an object that represents an aspect of
a sensor chip.  Features have a NAME, which is a string representation
of what is being measured.  The number represents a unique ID for this
feature and type is an enumeration keyword that describes what class
of feature is being represented.

---

*cstruct* **SENSORS-SUBFEATURE**

**Members:**

name -- A foreign string.

number -- A foreign integer.

type -- A foreign enum value of type SENSORS-SUBFEATURE-TYPE.

mapping -- A foreign integer.

flags -- a foreign bitfield (unsigned integer).

**Description**

A subfeature maps a series of values to different attributes of a the
sensors feature.  Each subfeature has a name and a type that describe
the subfeature.  The number gives it a unique identifier amonst other
subfeatures.  The mapping integer is used to match subfeatures with a
parent feature.  The flags member is a bitfield that contains the
read/write mode as well as a compute_mapping that is affected by the
main feature). 

---

*cenum* **SENSORS-FEATURE-TYPE**

**Description:**

A foreign enumeration that defines the different types of features.

---

*cenum* **SENSORS-SUBFEATURE-TYPE**

**Description:**

A foreign enumeration that defines the different types of subfeatures.

---

*Condition Type* **SENSORS-ERROR**

**Class Precedence List:**

sensors-error, error, serious-condition, condition, t

**Description:**

A parent class of all libsensors errors that occur when accessing
sensor data.  The return-value of the C function call is initialized
by the initialization argument :return-value and read using the
RETURN-VALUE function.

---

*Condition Type* **C-FILE-HANDLE-ERROR**

**Class Precedence List:**

c-file-handle-error, error, serious-condition, condition, t

**Description:**

An error condition that occurs during the opening or closing of
files using the C function fopen and fclose.

The openp boolean value representing if the error occured while
opening (T) or closing (NIL) a file is initialized with the :openp
initialization argument and read using the OPENP function.

The name-string value is a string representing the pathname to
be opened and is initialized using the :name-string initialization
argument and read using the NAME-STRING reader function.

The file-mode value is a string representing the file access mode
argument to the C function fopen.  It is initialized using the
:file-mode initialization argument and read using the FILE-MODE
reader function.

The file-pointer value is a file pointer value as returned from the
C function fopen.  It is initialized using the :file-pointer
initialization argument and read using the FILE-POINTER reader
function.

---

*Condition Type* **SENSORS-INITIALIZATION-ERROR**

**Class Precedence List:**

sensors-initialization-error, sensors-error, error, serious-condition, condition, t

**Description:**

A error condition that occurs when initalizing the use of libsensors
typically from a call to SENSORS-INIT.

The config-file value is a string representing a file pathname or
NIL if system defaults were requested to be used.  The :config-file
initialization argument is used for initialization and the reader
function CONFIG-FILE is used to read the value.

---

*Condition Type* **SENSORS-PARSE-ERROR**

**Class Precedence List:**

sensors-parse-error, sensors-error, error, serious-condition, condition, t

**Description:**

A error that occurs when parsing a sensor name.  A name-string value
represents a sensor chip name that was being parsed.  It is
initialized using the :name-string initialization argument and read
using the NAME-STRING reader.

---

*Condition Type* **SENSORS-SUBFEATURE-ERROR**

**Class Precedence List:**

sensors-subfeature-error, sensors-error, error, serious-condition, condition, t

**Description:**

An error created when accessing or reading a sensor chip subfeature.

A chip value is a foreign pointer to a SENSORS-CHIP-NAME struct,
initialized using the :chip initalization argument, and accessed using
the CHIP reader function.

A feature value is a foreign pointer to a SENSORS-FEATURE struct, is
initialized using the :feature initialization argument, and accessed
using the FEATURE reader function.

The type value is a subfeature type value that is a member of the
SENSORS-SUBFEATURE-TYPE C enumeration, is initialized using the :type
initialization argument and read using the SUBFEATURE-TYPE reader
function.

---

*Condition Type* **SENSORS-SET-SUBFEATURE-VALUE-ERROR**

**Class Precedence List:**

sensors-set-subfeature-value-error, sensors-subfeature-error,
sensors-error, error, serious-condition, condition, t

**Description:**

An error created when setting a subfeature's value.

A "value" value that holds the new value being set to the subfeature
is initialized using the :value initialization variable and read using
the VALUE reader function.

---

*Macro* **WITH-FOREIGN-FILE-POINTER**

**Syntax:**

with-foreign-file-pointer (pointer filespec &key mode) &body body

=> results

**Arguments and Values:**

pointer -- a variable.

filespec -- a string pathname.

mode -- A string file access mode, suitable to passing to C's fopen.

body -- a body form.

results -- the values returned by body.

**Description:**

with-foreign-file-pointer uses fopen to create a C file pointer by
opening the file named by filespec.  The file pointer to which the
pointer variable is bound has dynamic extent; its extent ends when the
form is exited.  When control leaves the body, either normally or
abnormally the file pointer is closed automatically.

---

*Macro* **WITH-PARSED-CHIP-NAME**

**Syntax:**

with-parsed-chip-name (variable sensors-chip-name &optional name-string) &body body

=> results

**Arguments and Values:**

variable -- a variable.

sensors-chip-name -- a foreign pointer to a SENSORS-CHIP-NAME struct.

name-string -- a string representing a sensor chip or NIL for all chips.

body -- a body form.

results -- the values returned by body.

**Description:**

with-parsed-chip-name binds VARIABLE to the parsed representation of
NAME-STRING.  If NAME-STRING is NIL a foreign null-pointer is bound to
VARIABLE, otherwise the NAME-STRING value is parsed using a foreign
function into SENSORS-CHIP-NAME and the foreign struct is bound to
VARIABLE.   Body is then executed within the dynamic extent of
VARIABLE.  When control leaves the body, either normally or abnormally
any parsed representations of NAME-STRING are freed.

---

*Function* **SENSORS-INIT**

**Syntax:**

sensors-init &optional config-file

=> t

**Arguments and Values**

config-file -- A string pathname to a libsensors configuration file or NIL.

**Description:**

sensors-init initializes the libsensors library by loading the
configuration file specified by CONFIG-FILE, which defaults to
\*LM-SENSORS-CONFIG-FILENAME\* and initializing the detected chips
list.  CONFIG-FILE should be a string pathname or NIL if system
defaults are to be used.  If the sensors fail to initalize then an
error of type SENSORS-INITIALIZATION-ERROR is created.

---

*Function* **SENSORS-CLEANUP**

**Syntax:**

sensors-cleanup

=> no value;

**Description:**

sensors-cleanup resets the state of libsensors and the detected chips
list.  If a new configuration is going to be used, this function is
needed to be called before a new call to SENSORS-INIT.

---

*Function* **SENSORS-GET-CHIP-NAME**

**Syntax:**

sensors-get-chip-name sensors-chip-name

=> chip-namestring

**Arguments and Values:**

sensors-chip-name -- A pointer to a foreign SENSORS-CHIP-NAME struct

chip-namestring -- A string or NIL.

**Description:**

Returns a chip name from its internal representation.  Note that chip
should not contain wildcard values.  This function is the equivalent
of libsensors' snprintf\_chip\_name function. The name is typically in
the form of <hardwarename>-<adapter>-<uniqueID>.

---

*Function* **SENSORS-GET-ADAPTER-NAME**

**Syntax:**

sensors-get-adapter-name sensors-chip-name

=> adapter-namestring

**Arguments and Values:**

sensors-chip-name -- A pointer to a foreign SENSORS-CHIP-NAME struct.

adapter-namestring -- A lisp string.

**Description:**

Returns the adapter name of a bus if it could be found, otherwise it
returns NIL.

---

*Function* **SENSORS-GET-DETECTED-CHIPS**

**Syntax:**

sensors-get-detected-chips &key name-string chip-function feature-function subfeature-function

=> sensor-data-alist

**Arguments and Values:**

name-string -- A string or NIL.

chip-function -- A function accepting two arguments.

feature-function -- A function accepting three arguments.

subfeature-function -- A function accpeting two arguments.

sensor-data-alist -- An alist of sensor values.

**Description**

This function does the primary work of the low level API.  It accepts
a chip name as a keyword argument that it attempts to use or if NIL
will detect all available chips, features, and subfeatures and return
the results as an list of data.  This function is used directly by
the higher level API.

There are several function keyword arguments that default to internal
functions, which create alists of data.  User supplied functions can
modify the default behavior.  Here is an outline of their behavior:

chip-function -- accepts a pointer to a foreign SENSORS-CHIP-NAME
struct and the output of the FEATURE-FUNCTION.  The results of this
function are collected by SENSORS-GET-DETECTED-CHIPS and returned as a
list.

feature-function -- accepts three inputs, a pointer to a foreign
SENSORS-CHIP-NAME struct, a pointer to a foreign FEATURE struct, and
the return value of SUBFEATURE-FUNCTION.  The result of this function
will be passed as the features argument to CHIP-FUNCTION.

subfeature-function -- accepts two inputs, a pointer to a foreign
SENSORS-CHIP-NAME struct, and a pointer to a foreign SUBFEATURE
struct.  It accesses the subfeature's value by calling
SENSORS-GET-VALUE and returns the subfeature data as the third
argument to FEATURE-FUNCTION.

The defaults for these three functions are the internal functions:
CHIP-ALIST, FEATURE-ALIST, and SUBFEATURE-ALIST, with the intention
that users can define their own functions 

---

*Function* **SENSORS-GET-FEATURES**

**Syntax:**

sensors-get-features sensors-chip-name &key feature-function subfeature-function

=> features-list

**Arguments and Values:**

sensors-chip-name -- A foreign pointer to a SENSORS-CHIP-NAME struct.

feature-function -- A function accepting three arguments.

subfeature-function -- A function accepting two arguments.

features-list -- An list of feature data values.

**Description**

Collects a list containing all subfeature data from all features of
SENSORS-CHIP-NAME.  See SENSORS-GET-DETECTED-CHIPS for more
information about FEATURE-FUNCTION and SUBFEATURE-FUNCTION.

---

*Function* **SENSORS-GET-LABEL**

**Syntax:**

sensors-get-label sensors-chip-name feature

=> feature-label

**Arguments and Values:**

sensors-chip-name -- A foreign pointer to a SENSORS-CHIP-NAME struct.

feature -- A foreign pointer to a FEATURE struct.

feature-label -- A string or NIL.

**Description**

Looks up and returns the label which belongs to FEATURE from
SENSORS-CHIP-NAME, which should not be based upon wildcard names.  On
failure NIL is returned.  If no label exists for this feature, its
name is returned.

---

*Function* **SENSORS-GET-ALL-SUBFEATURES**

sensors-get-all-subfeatures sensors-chip-name feature &optional function

=> subfeature-list

**Arguments and Values:**

sensors-chip-name -- A foreign pointer to a SENSORS-CHIP-NAME struct.

feature -- A foreign pointer to a FEATURE struct.

function -- A function accepting two foreign pointer arguments.

subfeature-list -- A list of subfeature data.

**Description**

Collects and returns a list of subfeatures of a given foreign FEATURE struct from
SENSORS-CHIP-NAME.  FUNCTION is a function that accepts a
SENSORS-CHIP-NAME struct pointer and a FEATURE struct pointer and will
package the subfeature data.  Currently this is done using the
internal function SUBFEATURE-ALIST, but it's possible to define custom
methods to handle subfeature data directly.

---

*Function* **SENSORS-GET-SUBFEATURE**

**Syntax:**

sensors-get-subfeature sensors-chip-name feature type &optional function

=> subfeature-data

**Arguments and Values:**

sensors-chip-name -- A foreign pointer to a SENSORS-CHIP-NAME struct.

feature -- A foreign pointer to a FEATURE struct.

type -- An integer related to the foreign SENSORS-SUBFEATURE-TYPE enum.

function -- A function that accepts two foreign pointer arguments.

subfeature-data -- Returned subfeature data from supplied FUNCTION.

**Description:**

Returns subfeature data of the subfeature TYPE from FEATURE of
SENSORS-CHIP-NAME using the function FUNCTION to fetch the subfeature
value and organize the subfeature data.  The default function returns
an alist of data by calling the internal function SUBFEATURE-ALIST.

---

*Function* **SENSORS-GET-VALUE**

**Syntax:**

sensors-get-value sensors-chip-name subfeature-type

=> float value

**Arguments and Values:**

sensors-chip-name -- A foreign pointer to a SENSORS-CHIP-NAME struct.

subfeature-type -- An integer relating to the foreign SENSORS-SUBFEATURE-TYPE enum.

float value -- A float value.

**Description:**

Reads the value of a subfeature of SUBFEATURE-TYPE from
SENSORS-CHIP-NAME and returns a float value, otherwise an error of
type SENSORS-SUBFEATURE-ERROR is created.

---

*Function* **SENSORS-SET-VALUE**

**Syntax:**

sensors-set-value sensors-chip-name subfeature-type value

=> T

**Arguments and Values:**

sensors-chip-name -- A foreign pointer to a SENSORS-CHIP-NAME struct.

subfeature-type -- An integer relating to the foreign SENSORS-SUBFEATURE-TYPE enum.

**Description:**

Sets the value of SUBFEATURE-TYPE of a SENSORS-CHIP-NAME struct to
VALUE.  Returns T or an error of SENSORS-SET-SUBFEATURE-VALUE-ERROR is
created.

---

*Function* **SENSORS-DO-CHIP-SETS**

**Syntax:**

sensors-do-chip-sets &optional sensors-chip-name

=> T

**Arguments and Values:**

sensors-chip-name -- A foreign pointer to a SENSORS-CHIP-NAME struct or NIL.

**Description:**

Executes all set statements for SENSORS-CHIP-NAME.  The chip-name may
contain wildcards. 



#### High level API



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
