# cl-lmsensors
### _Elliott Johnson <elliott@elliottjohnson.net>_

CL-LMSENSORS: a utility for interacting with lm_sensor data.

This library provides a basic means of interacting with
lm_sensors: https://hwmon.wiki.kernel.org/

It was built against lm_sensors/libsensors version 3.4.0
with json support.  The FETCH-SENSORS-DATA generic function
can have added methods to support other input methods besides
:JSON

Usage:

FETCH-SENSORS-DATA will return an alist of information.

In an effort to make the data more useable, I have
included some basic parsing for the following hardware:

	  coretemp-isa
	  atk0110-acpi

Calling PARSE-ALIST-DATA with the fetched sensor data, will
return the following forms:
    
    (("coretemp-isa-####"
      (<SensorName> <value> <Status> <%ofMax> <%ofCrit> <%ofAlarm>)
      ...)
     ("atk0110-acpi-#"
      (<SensorName> <value> <Status> <%ofMinMax> <%ofCrit>)
      ...))

Where <Status> equals on of :OK, :WARN, :CRITICAL and <%ofMinMax>
is equal to ```(* 100 (/ (- value min) (- max min)))```

As mentioned before this code is very basic and hopefully
provides a starting place for further development.

## License

MIT

Copyright 2018, Elliott Johnson <elliott@elliottjohnson.net>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
