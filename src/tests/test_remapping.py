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

def test_remap(cs):
    zstar_cs = pyale.mom_init_regrid(cs, {"ALE_COORDINATE_CONFIG": "UNIFORM"}, "ZSTAR")
    h_new = pyale.do_regrid(cs, zstar_cs)
    (t_new, s_new) = pyale.do_remap(cs, h_new)
