# cl-lmsensors
##### _Elliott Johnson <elliott@elliottjohnson.net>_

CL-LMSENSORS: a utility for interacting with 
[lm_sensors](https://hwmon.wiki.kernel.org/) data.

It was built/tested against lm_sensors/libsensors version 3.4.0
with json support.  The FETCH-SENSOR-DATA generic function
can be extended to support other input methods besides
:JSON

### Exported functions and values

----

*Variable* **\*SENSORS-BINARY\***

**Value Type:**

a string or nil

**Initial Value:**

Nil or a pathname, for example "/usr/bin/sensors"

**Description:**

The path used to execute the sensors binary.
It is initially set to the local path by calling
FIND-SENSORS-BINARY.

---

*Variable* **\*DEFAULT-SENSORS-DATA-METHOD\***

**Vaule Type:**

a keyword

**Initial Value:**

:JSON

**Description**

The default method used to fetch data when calling
\*SENSORS-BINARY\*.  The default method is JSON, but
adding :RAW is intended for the future.

---

*Function* **FETCH-SENSOR-DATA**

**Syntax:**

**fetch-sensor-data** *method-name* => *alist*

**Arguments and Values:**

*method-name*::= a symbol (eql :JSON)

*alist*::= an association list of sensor data.

**Description:**

Returns an association list containing the
data returned by a call to *SENSORS-BINARY*.

---

*Function* **FETCH-DEFAULT-SENSOR-DATA**

**Syntax:**

**fetch-default-sensor-data** &optional *method-name* => *alist*

**Arguments and Values:**

*method-name*::= a symbol (eql :JSON)

*alist*::= an association list of sensor data.

**Description:**

Returns an association list containing the
data returned by a call to *SENSORS-BINARY*.

---

*Function* **PARSE-SENSOR-DATA**

**Syntax:**

**parse-sensor-data** *alist* => *list*

**Arguments and Values:**

*alist*::= an association list of sensor data.

*list*::= an list of SENSOR-DATA objects populated
  with data parsed from *ALIST*

**Description:**

A function that accepts an alist of sensor data
and returns a list of HARDWARE objects populated
with data from the supplied alist.

---

*Function* **PARSE-DEFAULT-SENSOR-DATA**

**Syntax:**

**parse-default-sensor-data** *&optional* *method* => *parsed-alist*

**Arguments and Values:**

*method*::= a optional keyword for the fetch method.
  Currently defaults to \*DEFAULT-SENSORS-DATA-METHOD\*

**Description:**

A function that accepts an optional METHOD, defaulting
to \*DEFAULT-SENSORS-DATA-METHOD\* and returns a list
of HARDWARE objects populated with data fetched by
FETCH-DEFAULT-SENSOR-DATA.

### Parsing details.

In an effort to make the data more useable, I have
included some basic parsing for the following hardware that
I have available for testing:

	  coretemp-isa
	  atk0110-acpi

Calling PARSE-SENSOR-DATA with an alist of sensor data, will
return a list of HARDWARE objects, with the SENSORS slot populated
with SENSOR-DATA objects.

### Adding New Hardware

Adding new hardware involves creating a new file under the ```hardware/```
directory and including it into the ASDF system definition.

In this file, define classes of SENSOR-DATA that inherit behavior from
the SENSOR-\*-MIXINS.  These mixins define how to map data description strings
to slots in the SENSOR-DATA object.  New mixins can be defined by defining a
mixin class, defining a SET-SENSOR-DATA method (using the "or"
method combination) and if there are any computed values (%'s, status', etc)
then they can be defined in an :AFTER method to PARSE-HARDWARE-SENSOR-DATA.

Lastly to add new hardware, define a hardware class.  There are hardware mixins
(TEMPERATURE-MIXIN, VOLTAGE-MIXIN, FAN-MIXIN), which all add a slot that holds
a symbol to represent SENSOR-DATA classes defined above.  Then define a
method for the generic function SENSOR-CLASS (using the "or" method combination) which should match a SENSOR-DATA description string and return the contents
of the hardware mixin's slot (ie. the name of the SENSOR-DATA class).

The hardware classes are auto-magically instantiated by mapping the hardware
description string to a class name.  This is accomplished by naming the
hardware class the result of a call to HARDWARE-CLASS with the description
string.  When naming new hardware classes, check that this function will
return the correct class by calling the function with the hardware description
string returned by lm_sensors.

### Adding new input methods

Adding new input methods involves creating a new file under the ```methods/```
directory. In this file, define a new method to FETCH-SENSOR-DATA specializing
on a keyword that fetchs data from lm_sensors and returns the value as an
alist of values.  The resulting alist should be equal to an alist using the
:JSON method.

### Ideas for the future.

1) Add an input method for reading RAW data from lm_sensors.
     * Can we avoid pulling in all the requirements for '''jonathan'''?
2) Expand the supported hardware.
     * Can this be done easily?
     * Is there a need to version the information due to changes in lm_sensors?
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
