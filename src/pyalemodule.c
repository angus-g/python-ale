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
extern bool do_mom_regrid(void*, void*, double, double*, int, int, int);
extern bool do_mom_accelerate(void*, void*, int, double, double*, double*, double*, int, int, int);
extern bool do_mom_remap(void*, double*, double*, double*, int, int, int);
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

static char* regrid_keywords[] = {
  "mom_cs", "regrid_cs", "dt", NULL,
};

static PyObject *pyale_do_regrid(PyObject *self, PyObject *args, PyObject *kwargs) {
  void *cs, *regrid_cs;
  int ok;
  PyObject *cs_ptr, *regrid_ptr;
  int ni, nj, nk;
  double dt = 0.0;
  npy_intp dims[3];

  ok = PyArg_ParseTupleAndKeywords(args, kwargs, "O!O!|$d", regrid_keywords,
				   &PyCapsule_Type, &cs_ptr,
				   &PyCapsule_Type, &regrid_ptr,
				   &dt);
  if (!ok)
    return NULL;

  cs = PyCapsule_GetPointer(cs_ptr, "MOM6 CS");
  regrid_cs = PyCapsule_GetPointer(regrid_ptr, "MOM6 ALE CS");
  // allocate h_new array of the right size
  get_domain_dims(cs, &ni, &nj, &nk);
  dims[0] = ni; dims[1] = nj; dims[2] = nk;
  PyObject *h_new = PyArray_New(&PyArray_Type, 3, dims, NPY_DOUBLE, NULL, NULL, 0, NPY_ARRAY_FARRAY, NULL);

  if (!do_mom_regrid(cs, regrid_cs, dt, (double*)PyArray_DATA((PyArrayObject*)h_new), ni, nj, nk)) {
    clear_mom_error();
    PyErr_SetString(PyExc_RuntimeError, "Error running regridding");
    Py_DECREF(h_new);

    return NULL;
  }

  return h_new;
}

static char* accelerate_keywords[] = {
  "mom_cs", "regrid_cs", "iter", "dt", NULL,
};

static PyObject *pyale_accelerate_ale(PyObject *self, PyObject *args, PyObject *kwargs) {
  void *cs, *regrid_cs;
  int ok;
  PyObject *cs_ptr, *regrid_ptr;
  int ni, nj, nk, iter;
  double dt = 0.0;
  npy_intp dims[3];

  ok = PyArg_ParseTupleAndKeywords(args, kwargs, "O!O!i|$d", accelerate_keywords,
				   &PyCapsule_Type, &cs_ptr,
				   &PyCapsule_Type, &regrid_ptr,
				   &iter, &dt);
  if (!ok)
    return NULL;

  cs = PyCapsule_GetPointer(cs_ptr, "MOM6 CS");
  regrid_cs = PyCapsule_GetPointer(regrid_ptr, "MOM6 ALE CS");
  get_domain_dims(cs, &ni, &nj, &nk);
  dims[0] = ni; dims[1] = nj; dims[2] = nk;
  PyObject *h_new = PyArray_New(&PyArray_Type, 3, dims, NPY_DOUBLE, NULL, NULL, 0, NPY_ARRAY_FARRAY, NULL);
  PyObject *t_new = PyArray_New(&PyArray_Type, 3, dims, NPY_DOUBLE, NULL, NULL, 0, NPY_ARRAY_FARRAY, NULL);
  PyObject *s_new = PyArray_New(&PyArray_Type, 3, dims, NPY_DOUBLE, NULL, NULL, 0, NPY_ARRAY_FARRAY, NULL);

  if (!do_mom_accelerate(cs, regrid_cs, iter, dt,
			 (double*)PyArray_DATA((PyArrayObject*)h_new),
			 (double*)PyArray_DATA((PyArrayObject*)t_new),
			 (double*)PyArray_DATA((PyArrayObject*)s_new),
			 ni, nj, nk)) {
    clear_mom_error();
    PyErr_SetString(PyExc_RuntimeError, "Error running accelerated ALE");
    Py_DECREF(h_new);
    Py_DECREF(t_new);
    Py_DECREF(s_new);

    return NULL;
  }

  return Py_BuildValue("(NNN)", h_new, t_new, s_new);
}

static PyObject *pyale_do_remap(PyObject *self, PyObject *args) {
  void *cs;
  int ok;
  PyObject *cs_ptr, *t_new, *s_new, *h_arg, *h_new;
  int ni, nj, nk;
  npy_intp dims[3], *in_dims;

  // take h_new array
  ok = PyArg_ParseTuple(args, "O!O", &PyCapsule_Type, &cs_ptr, &h_arg);
  if (!ok)
    return NULL;

  cs = PyCapsule_GetPointer(cs_ptr, "MOM6 CS");
  get_domain_dims(cs, &ni, &nj, &nk);
  dims[0] = ni; dims[1] = nj; dims[2] = nk;

  h_new = PyArray_FROM_OTF(h_arg, NPY_DOUBLE, NPY_ARRAY_IN_FARRAY);
  if (!h_new) return NULL;

  // check that h_new is the right size for the domain
  in_dims = PyArray_DIMS((PyArrayObject*)h_new);
  if (PyArray_NDIM((PyArrayObject*)h_new) != 3 || in_dims[0] != dims[0] || in_dims[1] != dims[1] || in_dims[2] != dims[2]) {
    PyErr_SetString(PyExc_ValueError, "h_new dims don't conform to domain");
    Py_DECREF(h_new);
    return NULL;
  }

  t_new = PyArray_New(&PyArray_Type, 3, dims, NPY_DOUBLE, NULL, NULL, 0, NPY_ARRAY_FARRAY, NULL);
  s_new = PyArray_New(&PyArray_Type, 3, dims, NPY_DOUBLE, NULL, NULL, 0, NPY_ARRAY_FARRAY, NULL);
  if (!t_new || !s_new) {
    Py_DECREF(h_new);
    return NULL;
  }

  if (!do_mom_remap(cs,
		    (double*)PyArray_DATA((PyArrayObject*)h_new),
		    (double*)PyArray_DATA((PyArrayObject*)t_new),
		    (double*)PyArray_DATA((PyArrayObject*)s_new),
		    ni, nj, nk)) {
    clear_mom_error();
    PyErr_SetString(PyExc_RuntimeError, "Error running remapping");
    Py_DECREF(h_new);
    Py_DECREF(t_new);
    Py_DECREF(s_new);
    return NULL;
  }

  // return state tuple
  Py_DECREF(h_new);
  return Py_BuildValue("(NN)", t_new, s_new);
}

static PyMethodDef PyaleMethods[] = {
  {"mom_init_cs", pyale_init_cs, METH_VARARGS, "Initialise a MOM CS."},
  {"load_mom_restart", pyale_load_restart, METH_VARARGS, "Load a MOM restart."},
  {"mom_init_regrid", pyale_init_regrid, METH_VARARGS, "Initialise MOM regridding."},
  {"do_regrid", (PyCFunction) pyale_do_regrid, METH_VARARGS | METH_KEYWORDS, "Perform MOM regridding."},
  {"accelerate_ale", (PyCFunction) pyale_accelerate_ale, METH_VARARGS | METH_KEYWORDS, "Perform iterated MOM regridding/remapping. Returns a (H, T, S) tuple."},
  {"do_remap", pyale_do_remap, METH_VARARGS, "Perform MOM remapping. Returns a (T, S) tuple."},
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
