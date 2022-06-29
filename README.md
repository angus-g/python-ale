# python-ale
Interface to MOM6 regridding/remapping in Python

## Installation
MOM6 is bundled as a git submodule, so be sure to run `git submodule
update --init --recursive` before building/installing. At the minimum,
you'll need a version of Python with the development headers, a
Fortran compiler, and the netCDF library, with its Fortran bindings.

With the prerequisites in place, you'll be able to run `pip install
.` to build and install the extension into your current Python
environment. As usual for libraries like this, an isolated virtual
environment is a nice place to do development.

## How it works
MOM6 was built upon FMS (the Flexible Modelling System), and calls out
to that for processes like domain decomposition, diagnostics, and file
I/O. Within the model, there is an *infra* layer, that translates
higher-level requests into the specific FMS routines that implement
that feature. The intention was for the *infra* layer to be pluggable
with different frameworks, so as to not depend explicitly on FMS. In
practice, the abstracted routines still map quite closely onto FMS.

Because we don't need the full FMS to be able to call the ALE routines
from MOM6, we implement our own *infra* layer, consisting of *shims*
for a lot of the parallel- and diagnostic-related functionality. We
can then build an FMS-independent MOM6 shared library, which we link
against a small driver module that initialises the minimum control
structures necessary to access ALE.
