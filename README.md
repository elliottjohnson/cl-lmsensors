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

### Exported functions and values

TODO - update these for the new codebase.

### Ideas for the future.

1) Add an input method for reading RAW data from lm_sensors. *done*
2) Expand the supported hardware. *done*
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
