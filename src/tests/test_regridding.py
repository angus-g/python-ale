from pathlib import Path
import pytest

import pyale

FIXTURE_DIR = Path(__file__).parent.resolve() / "data"

@pytest.fixture(scope="module")
def cs():
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
        "MAXIMUM_DEPTH": 6000.0,
        "NK": 75,
    }

    cs = pyale.mom_init_cs(params)
    pyale.load_mom_restart(cs, str(FIXTURE_DIR / "MOM.res.nc"))

    return cs

schemes = (
    "ZSTAR",
    "SIGMA",
    "RHO",
    "HYCOM1",
    "HYBGEN",
    "SLIGHT",
    "ADAPTIVE",
)

@pytest.mark.parametrize("scheme", schemes)
def test_regrid(cs, scheme):
    params = {}
    regrid_cs = pyale.mom_init_regrid(cs, params, scheme)
    h_new = pyale.do_regrid(cs, regrid_cs)
