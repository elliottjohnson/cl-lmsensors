# cl-lmsensors
##### _Elliott Johnson <elliott@elliottjohnson.net>_

CL-LMSENSORS: a utility for interacting with lm_sensor data.

This library provides a basic means of interacting with
lm_sensors: https://hwmon.wiki.kernel.org/

It was built against lm_sensors/libsensors version 3.4.0
with json support.  The FETCH-SENSORS-DATA generic function
can be extended to support other input methods besides
:JSON

### Exported functions and values
Variable *SENSORS-BINARY*

Value Type: string

Initial Value: A path, for example "/usr/bin/sensors"

Description:
	The path used to execute the sensors binary.
It is initially set to the local path by calling
FIND-SENSORS-BINARY in order to locate it. 



Function FETCH-SENSOR-DATA

Syntax:
fetch-sensor-data method-name => alist

Arguments and Values: 

method-name::= a symbol (eql :JSON)

alist::= an association list of sensor data.

Description:

Returns an association list containing the
data returned by a call to *SENSORS-BINARY*.



Function PARSE-ALIST-DATA

Syntax:
parse-alist-data alist => parsed-alist

Arguments and Values:

alist::= an association list of sensor data.

parsed-alist::= an list of values, %'s, and status
  computed from the sensor data.  The parsing is based
  on hardware type.



Function PARSE-DEFAULT-SENSOR-DATA

Syntax:
parse-default-sensor-data &optional method => parsed-alist

Arguments and Values:

method::= a optional keyword for the fetch method.
  Currently only accpets :JSON.

Description:

In an effort to make the data more useable, I have
included some basic parsing for the following hardware:

	  coretemp-isa
	  atk0110-acpi

Read more in the parsing details below:

### Parsing details.

Calling PARSE-ALIST-DATA with the fetched sensor data, will
currently return the following forms:
    
    (("coretemp-isa-####"
      (<SensorName> <value> <Status> <%ofMax> <%ofCrit> <%ofAlarm>)
      ...)
     ("atk0110-acpi-#"
      (<SensorName> <value> <Status> <%ofMinMax> <%ofCrit>)
      ...))

Where Status equals one of :OK, :WARN, :CRITICAL and %ofMinMax
is equal to ```(* 100 (/ (- value min) (- max min)))```

In the future hopefully I can put more structure to make adding
new methods and hardware very easy.  Hopefully it provides a
starting place for further development.

### Ideas for the future.

1) sensorsd type data collection running in a thread.
  * Data Storage: RRDBS?
2) Data representation
  * RRDBS has it's own tools, is there a lisp method (vecto, etc)
3) StumpWM modeline with temp information.
  * Useful when compiling a lot on a poorly cooled machine.


## License

MIT

Copyright 2018, Elliott Johnson <elliott@elliottjohnson.net>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
