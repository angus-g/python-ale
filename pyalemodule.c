#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include <stdio.h>

extern void init_mom_state(void*, PyObject*);
extern void load_mom_restart(void*, char*, int);
extern void init_mom_ale(void*, void*, PyObject*, char*, int);
extern void destroy_mom_state(void*);
extern void destroy_mom_ale(void*);

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

  init_mom_state(&cs, param_dict);
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

  load_mom_restart(cs, restart_buf, (int)buf_len);

  Py_RETURN_NONE;
}

static PyMethodDef PyaleMethods[] = {
  {"mom_init_cs", pyale_init_cs, METH_VARARGS, "Initialise a MOM CS."},
  {"load_mom_restart", pyale_load_restart, METH_VARARGS, "Load a MOM restart."},
  {"mom_init_regrid", pyale_init_regrid, METH_VARARGS, "Initialise MOM regridding."},
  {NULL, NULL, 0, NULL},
};

static struct PyModuleDef pyalemodule = {
  PyModuleDef_HEAD_INIT,
  "pyale",
  NULL, // module documentation
  -1, // size of per-interpreter state
  PyaleMethods
};

PyMODINIT_FUNC PyInit_pyale(void) {
  return PyModule_Create(&pyalemodule);
}