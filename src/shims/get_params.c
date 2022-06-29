#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stdbool.h>
#include <stdio.h>

bool get_dict_int(PyObject *params, char *name, int namelen, int *val) {
  char cname[namelen+1];
  PyObject *item;

  strncpy(cname, name, namelen);
  cname[namelen] = '\0';

  item = PyDict_GetItemString(params, cname);
  if (!item || !PyLong_Check(item)) return 1;

  *val = (int)PyLong_AsLong(item);
  return 0;
}

bool get_dict_int_array(PyObject *params, char *name, int namelen, int **val, int len) {
  char cname[namelen+1];
  PyObject *item;

  strncpy(cname, name, namelen);
  cname[namelen] = '\0';

  item = PyDict_GetItemString(params, cname);
  if (!item || !PySequence_Check(item) || PySequence_Length(item) < len) return 1;

  for (int i = 0; i < len; i++) {
    PyObject *x = PySequence_ITEM(item, i);
    if (!PyLong_Check(x)) return 1;
    (*val)[i] = (int)PyLong_AsLong(x);

    Py_DECREF(x);
  }

  return 0;
}

bool get_dict_real(PyObject *params, char *name, int namelen, double *val) {
  char cname[namelen+1];
  PyObject *item, *fitem;

  strncpy(cname, name, namelen);
  cname[namelen] = '\0';

  item = PyDict_GetItemString(params, cname);
  if (!item || !PyNumber_Check(item)) return 1;

  fitem = PyNumber_Float(item);
  *val = PyFloat_AS_DOUBLE(fitem);
  Py_DECREF(fitem);

  return 0;
}

bool get_dict_real_array(PyObject *params, char *name, int namelen, double **val, int len) {
  char cname[namelen+1];
  PyObject *item;

  strncpy(cname, name, namelen);
  cname[namelen] = '\0';

  fprintf(stderr, "get_dict_real_array not implemented! %s\n", cname);
  return 1;
}

bool get_dict_logical(PyObject *params, char *name, int namelen, bool *val) {
  char cname[namelen+1];
  PyObject *item;

  strncpy(cname, name, namelen);
  cname[namelen] = '\0';

  item = PyDict_GetItemString(params, cname);
  if (!item || !PyBool_Check(item)) return 1;

  *val = (item == Py_True);
  return 0;
}

bool get_dict_char(PyObject *params, char *name, int namelen, char *val, int *vallen) {
  char cname[namelen+1];
  PyObject *item;
  char *buf;
  Py_ssize_t len;

  strncpy(cname, name, namelen);
  cname[namelen] = '\0';

  item = PyDict_GetItemString(params, cname);
  if (!item) return 1;

  // only accept unicode or byte strings
  if (PyUnicode_Check(item)) {
    buf = (char*)PyUnicode_AsUTF8AndSize(item, &len);
  } else if (PyBytes_Check(item)) {
    PyBytes_AsStringAndSize(item, &buf, &len);
  } else {
    return 1;
  }

  *vallen = (int)len;
  strncpy(val, buf, *vallen);

  return 0;
}
