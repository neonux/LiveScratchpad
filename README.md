# Live Scratchpad #


## Description ##

This add-on adds a live function evaluation feature to the Scratchpad.
You can change the arguments passed to the function and see the result of the
function in real-time, including temporary assignments performed within the
function.
It also shows you the code path taken during execution and provides some nice
debugging features when manipulating regular expressions or HTML canvar.


## Build Instructions ##

From inside the project directory, run:

make

or:

zip -r ../LiveScratchpad.xpi * -x ".git/*"


## Installation ##

Note that the Live Scratchpad add-on requires Firefox 11 or later.
Drag and drop LiveScratchpad.xpi in Firefox, follow instructions to install add-on.


## Testsuite ##

To run the tests, you need to build Mozilla from source.
Set environment variable OBJDIR to point to your tree's OBJDIR, then run :

make test


## Legal ##

Licensed under the tri-license MPL 1.1/GPL 2.0/LGPL 2.1.
See LICENSE.txt for details.

The logo icon.png and derivatives are attributed to the W3C and licensed under
CC Attribution 3.0 Unported <http://creativecommons.org/licenses/by/3.0/>.


## More ##

For documentation, feedback, contributions :
http://wiki.mozilla.org/DevTools/Features/Scratchpad

