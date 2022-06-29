from pathlib import Path
import pytest

import pyale

FIXTURE_DIR = Path(__file__).parent.resolve() / "data"

def test_create_cs():
    params = {
        "NIGLOBAL": 160,
        "NJGLOBAL": 800,
        "GRID_CONFIG": "mercator",
        "ISOTROPIC": True,
        "SOUTHLAT": -70.275597151116,
        "LENLAT": 140.5511943022,
        "LENLON": 40,
        "TOPO_CONFIG": "flat",
    }

    cs = pyale.mom_init_cs(params)

def test_load_restart():
    params = {
        "NIGLOBAL": 2,
        "NJGLOBAL": 2,
        "NIHALO": 2,
        "NJHALO": 2,
        "GRID_CONFIG": "cartesian",
        "SOUTHLAT": 30.0,
        "LENLAT": 1.0,
        "LENLON": 1.0,
        "TOPO_CONFIG": "flat",
        "MINIMUM_DEPTH": 0.5,
        "MAXIMUM_DETPH": 6000.0,
        "NK": 75,
    }

    cs = pyale.mom_init_cs(params)
    pyale.load_mom_restart(cs, str(FIXTURE_DIR / "MOM.res.nc"))
