This is my emacs configuration.

`init.el` is a rather simple file. It lists all needed packages in the
`my-packages` variable. These are checked to be available and
installed if needed. After that, `after-init.el` is set to be run as
an after-init-hook.

`after-init.el` contains the main configuration.

`lisp/` directory contains any miscellaneous emacs lisp code.

the text-based browser `w3m`, a `markdown` implementation, and the
python package epc (`pip install epc`) need to be present for
everything to work.

`sbcl` and slime (located in `~/source/slime/`) are also needed, but
I'll probably need to fix this and make it more portable later.
