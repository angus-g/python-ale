from pathlib import Path
import pytest

import pyale

FIXTURE_DIR = Path(__file__).parent.resolve() / "data"

def test_displace_sfc():
    params = {
        "INPUTDIR": str(FIXTURE_DIR),
        "NIGLOBAL": 240,
        "NJGLOBAL": 40,
        "GRID_CONFIG": "cartesian",
        "AXIS_UNITS": "k",
        "SOUTHLAT": 0.0,
        "LENLAT": 80.0,
        "WESTLON": 320.0,
        "LENLON": 480.0,
        "TOPO_CONFIG": "ISOMIP",
        "MAXIMUM_DEPTH": 720.0,
        "NK": 25,
        "THICKNESS_CONFIG": "ISOMIP",
        "ICE_PROFILE_CONFIG": "FILE",
        "ICE_THICKNESS_FILE": "Ocean1_3D.nc",
        "ICE_THICKNESS_VARNAME": "thick",
        "ICE_AREA_VARNAME": "area",
    }

    # because the following are all init functions, they *all* need the params
    cs = pyale.mom_init_cs(params)
    pyale.mom_init_shelf(cs, params)
    h = pyale.mom_get_thickness(cs)
    pyale.mom_displace_sfc(cs, params)
    h_new = pyale.mom_get_thickness(cs)

    print(h_new - h)
