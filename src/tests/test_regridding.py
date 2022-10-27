from pathlib import Path
import pytest

import pyale

FIXTURE_DIR = Path(__file__).parent.resolve() / "data"

@pytest.fixture(scope="module")
def cs():
    params = {
        "NIGLOBAL": 2,
        "NJGLOBAL": 2,
        "NIHALO": 3,
        "NJHALO": 3,
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

schemes = [
    ("ZSTAR", {"ALE_COORDINATE_CONFIG": "UNIFORM"}),
    ("SIGMA", {"ALE_COORDINATE_CONFIG": "UNIFORM"}),
    ("RHO", {"ALE_COORDINATE_CONFIG": "RFNC1:35,999.5,1028,1028.5,8.,1038.,0.0078125"}),
    ("HYCOM1", {"ALE_COORDINATE_CONFIG": "HYBRID:hycom1_75_800m.nc,sigma2,FNC1:2,4000,4.5,.01"}),
    ("HYBGEN", {"ALE_COORDINATE_CONFIG": "HYBRID:hycom1_75_800m.nc,sigma2,FNC1:2,4000,4.5,.01"}),
    ("SLIGHT", {"ALE_COORDINATE_CONFIG": "HYBRID:hycom1_75_800m.nc,sigma2,FNC1:2,4000,4.5,.01"}),
    ("ADAPTIVE", {"ALE_COORDINATE_CONFIG": "UNIFORM"}),
]

@pytest.mark.parametrize("scheme,params", schemes)
def test_regrid(cs, scheme, params):
    params["INPUTDIR"] = str(FIXTURE_DIR)
    regrid_cs = pyale.mom_init_regrid(cs, params, scheme)
    h_new = pyale.do_regrid(cs, regrid_cs)

@pytest.mark.parametrize("scheme,params", schemes)
def test_accelerated_regrid(cs, scheme, params):
    params["INPUTDIR"] = str(FIXTURE_DIR)
    regrid_cs = pyale.mom_init_regrid(cs, params, scheme)
    h_new, t_new, s_new = pyale.accelerate_ale(cs, regrid_cs, 1)

@pytest.mark.parametrize("scheme,params", schemes)
def test_accelerated_resume(cs, scheme, params):
    params["INPUTDIR"] = str(FIXTURE_DIR)
    regrid_cs = pyale.mom_init_regrid(cs, params, scheme)
    state = pyale.accelerate_ale(cs, regrid_cs, 1)
    h_new, t_new, s_new = pyale.resume_ale(cs, regrid_cs, state, 1)