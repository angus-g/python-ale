from pathlib import Path
import pytest

import pyale

FIXTURE_DIR = Path(__file__).parent.resolve() / "data"

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
    "ALE_COORDINATE_CONFIG": "UNIFORM",
}

def test_diag():
    params["INPUTDIR"] = str(FIXTURE_DIR)

    cs = pyale.mom_init_cs(params)
    pyale.load_mom_restart(cs, str(FIXTURE_DIR / "MOM.res.nc"))
    regrid_cs = pyale.mom_init_regrid(cs, params, "ADAPTIVE")

    h_new, diags = pyale.do_regrid(cs, regrid_cs, dt=1.0, diags=["adapt_slope_u"])
    assert "adapt_slope_u" in diags

def test_diag_accelerated():
    params["INPUTDIR"] = str(FIXTURE_DIR)

    cs = pyale.mom_init_cs(params)
    pyale.load_mom_restart(cs, str(FIXTURE_DIR / "MOM.res.nc"))
    regrid_cs = pyale.mom_init_regrid(cs, params, "ADAPTIVE")

    h_new, t_new, s_new, diags = pyale.accelerate_ale(cs, regrid_cs, 1, dt=1.0, diags=["adapt_slope_u"])
    assert "adapt_slope_u" in diags
