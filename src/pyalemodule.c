#define PY_SSIZE_T_CLEAN
#define NPY_NO_DEPRECATED_API NPY_1_7_API_VERSION
#include <Python.h>
#include <numpy/ndarrayobject.h>

#include <stdio.h>
#include <stdbool.h>

extern bool init_mom_state(void*, PyObject*);
extern bool load_mom_restart(void*, char*, int);
extern void init_mom_ale(void*, void*, PyObject*, char*, int);
extern void destroy_mom_state(void*);
extern void destroy_mom_ale(void*);
extern void get_domain_dims(void*, int*, int*, int*);
extern void do_mom_regrid(void*, void*, double*, int, int, int);
extern void clear_mom_error();

static void pyale_destroy_cs(PyObject *capsule) {
  void *cs = PyCapsule_GetPointer(capsule, "MOM6 CS");
  destroy_mom_state(&cs);
}

static void pyale_destroy_regrid(PyObject *capsule) {
  void *cs = PyCapsule_GetPointer(capsule, "MOM6 ALE CS");
  destroy_mom_ale(&cs);
}

static PyObject *pyale_init_cs(PyObject *self, PyObject *args) {
  void *cs;
  int ok;
  PyObject *param_dict;

  // we expect a dict
  ok = PyArg_ParseTuple(args, "O!", &PyDict_Type, &param_dict);
  if (!ok)
    return NULL;

  if (!init_mom_state(&cs, param_dict)) {
    clear_mom_error();
    PyErr_SetString(PyExc_RuntimeError, "Error initialising MOM state");
    return NULL;
  }

  PyObject *cs_ptr = PyCapsule_New(cs, "MOM6 CS", pyale_destroy_cs);
  return cs_ptr;
}

static PyObject *pyale_init_regrid(PyObject *self, PyObject *args) {
  void *cs, *regrid_cs;
  int ok;
  PyObject *param_dict, *cs_ptr, *regrid_ptr;
  char *scheme_buf;
  Py_ssize_t buf_len;

  ok = PyArg_ParseTuple(args, "O!O!s#", &PyCapsule_Type, &cs_ptr, &PyDict_Type, &param_dict,
			&scheme_buf, &buf_len);
  if (!ok)
    return NULL;

  cs = PyCapsule_GetPointer(cs_ptr, "MOM6 CS");
  init_mom_ale(cs, &regrid_cs, param_dict, scheme_buf, (int)buf_len);
  regrid_ptr = PyCapsule_New(regrid_cs, "MOM6 ALE CS", pyale_destroy_regrid);
  return regrid_ptr;
}

static PyObject *pyale_load_restart(PyObject *self, PyObject *args) {
  void *cs;
  int ok;
  PyObject *cs_ptr;
  char *restart_buf;
  Py_ssize_t buf_len;

  ok = PyArg_ParseTuple(args, "O!s#", &PyCapsule_Type, &cs_ptr, &restart_buf, &buf_len);
  if (!ok)
    return NULL;

  cs = PyCapsule_GetPointer(cs_ptr, "MOM6 CS");

  if (!load_mom_restart(cs, restart_buf, (int)buf_len)) {
    clear_mom_error();
    PyErr_SetString(PyExc_RuntimeError, "Error loading restart");
    return NULL;
  }

  Py_RETURN_NONE;
}

static PyObject *pyale_do_regrid(PyObject *self, PyObject *args) {
  void *cs, *regrid_cs;
  int ok;
  PyObject *cs_ptr, *regrid_ptr;
  int ni, nj, nk;
  npy_intp dims[3];

  ok = PyArg_ParseTuple(args, "O!O!", &PyCapsule_Type, &cs_ptr, &PyCapsule_Type, &regrid_ptr);
  if (!ok)
    return NULL;

  cs = PyCapsule_GetPointer(cs_ptr, "MOM6 CS");
  regrid_cs = PyCapsule_GetPointer(regrid_ptr, "MOM6 ALE CS");
  // allocate h_new array of the right size
  get_domain_dims(cs, &ni, &nj, &nk);
  dims[0] = ni; dims[1] = nj; dims[2] = nk;
  PyObject *h_new = PyArray_New(&PyArray_Type, 3, dims, NPY_DOUBLE, NULL, NULL, 0, NPY_ARRAY_FARRAY, NULL);

  do_mom_regrid(cs, regrid_cs, (double*)PyArray_DATA((PyArrayObject*)h_new), ni, nj, nk);

  return h_new;
}

static PyMethodDef PyaleMethods[] = {
  {"mom_init_cs", pyale_init_cs, METH_VARARGS, "Initialise a MOM CS."},
  {"load_mom_restart", pyale_load_restart, METH_VARARGS, "Load a MOM restart."},
  {"mom_init_regrid", pyale_init_regrid, METH_VARARGS, "Initialise MOM regridding."},
  {"do_regrid", pyale_do_regrid, METH_VARARGS, "Perform MOM regridding."},
  {NULL, NULL, 0, NULL},
};

static struct PyModuleDef pyalemodule = {
  PyModuleDef_HEAD_INIT,
  "_pyale",
  "pyale extension module", // module documentation
  -1, // size of per-interpreter state
  PyaleMethods
};

PyMODINIT_FUNC PyInit__pyale(void) {
  import_array();
  return PyModule_Create(&pyalemodule);
}
