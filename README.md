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

## An example
Here's a minimal example for setting up a domain, loading a restart,
and performing regridding according to that restart. Note that the
parameters should correspond to those in the `MOM_input` that was used
to generate the restart in the first place.

```python
import pyale

params = {
    "NIGLOBAL": 160,
    "NJGLOBAL": 800,
    "GRID_CONFIG": "mercator",
    "ISOTROPIC": True,
    "SOUTHLAT": -70.275597151116,
    "LENLAT": 140.5511943022,
    "LENLON": 40,
    "TOPO_CONFIG": "file",
    "TOPO_VARNAME": "topog",
    "MAXIMUM_DEPTH": 4000.0,
    "NK": 75,
}
cs = pyale.mom_init_cs(params)
pyale.load_mom_restart(cs, "MOM.res.nc")
zstar_cs = pyale.mom_init_regrid(cs, params, "ZSTAR")
h_new = pyale.do_regrid(cs, zstar_cs)
```

## Available regridding schemes
It's possible to create multiple regridding control structures using
`mom_init_regrid`. Here are the possible schemes:

- `ZSTAR`, the *stretch geopotential* coordinate
- `SIGMA`, the *terrain following* coordinate
- `SIGMA_SHELF_ZSTAR`, a hybrid of *ZSTAR* and *SIGMA* (for ice shelf cavities)
- `RHO`, the *continuous isopycnal* coordinate
- `HYCOM1`, the *HyCOM-like* coordinate
- `HYBGEN`, the *HyCOM hybgen* coordinate
- `SLIGHT`, the *SLight* coordinte (stretched coordinate above continuous isopycnal
- `ADAPTIVE`, the *AG* coordinate
