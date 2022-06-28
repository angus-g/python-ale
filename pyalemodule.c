#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include <stdio.h>

extern void init_mom_state(void*, PyObject*);
extern void destroy_mom_state(void*);

static void pyale_destroy_cs(PyObject *capsule) {
  void *cs = PyCapsule_GetPointer(capsule, "MOM6 CS");
  destroy_mom_state(&cs);
}

static PyObject *pyale_init_cs(PyObject *self, PyObject *args) {
  void *cs;
  int ok;
  PyObject *param_dict;

  // we expect a dict
  ok = PyArg_ParseTuple(args, "O", &param_dict);
  if (!ok || !PyDict_Check(param_dict))
    return NULL;

  init_mom_state(&cs, param_dict);
  PyObject *cs_ptr = PyCapsule_New(cs, "MOM6 CS", pyale_destroy_cs);
  return cs_ptr;
}

static PyMethodDef PyaleMethods[] = {
  {"mom_init_cs", pyale_init_cs, METH_VARARGS, "Initialise a MOM CS."},
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
