pub mod emit_py;

use koatl_core::{
    format_errs, linecol::LineColCache, parser::lexer::is_valid_ident,
    transpile as transpile_to_source, transpile_to_py_ast, TranspileOptions,
};
use pyo3::{
    exceptions::PyBaseException,
    ffi::{self},
    prelude::*,
    types::{PyDict, PyList, PyRange, PySlice, PyString},
};

#[pyfunction(signature=(src, mode="module", filename="<string>", sourcemap=false))]
fn transpile(src: &str, mode: &str, filename: &str, sourcemap: bool) -> PyResult<PyObject> {
    let options = match mode {
        "module" => TranspileOptions::module(),
        "prelude" => TranspileOptions::prelude(),
        "interactive" => TranspileOptions::interactive(),
        "script" => TranspileOptions::script(),
        _ => {
            return Err(PyErr::new::<pyo3::exceptions::PyValueError, _>(
                "Invalid mode. Use 'module' or 'prelude' or 'interactive' or 'script'.",
            ))
        }
    };

    if sourcemap {
        let ctx = transpile_to_source(src, options).map_err(|e| {
            PyErr::new::<pyo3::exceptions::PySyntaxError, _>(format_errs(&e, filename, src))
        })?;

        let line_cache = LineColCache::new(src);

        let retval = Python::with_gil(|py| -> PyResult<PyObject> {
            let pydict = PyDict::new(py);

            for (line, span) in ctx.source_line_map {
                pydict.set_item(line, line_cache.linecol(span.start).0)?;
            }

            let ret_list = PyList::empty(py);
            ret_list.append(ctx.source)?;
            ret_list.append(pydict)?;

            Ok(ret_list.unbind().into_any())
        })?;

        Ok(retval)
    } else {
        let py_ast = transpile_to_py_ast(src, options).map_err(|e| {
            PyErr::new::<pyo3::exceptions::PySyntaxError, _>(format_errs(&e, filename, src))
        })?;

        let py_ast_obj = emit_py::emit_py(&py_ast, src).map_err(|e| {
            PyErr::new::<pyo3::exceptions::PyException, _>(format!("Emission error: {}", e.message))
        })?;
        Ok(py_ast_obj)
    }
}

#[pyclass(extends=PyDict)]
#[derive(Default)]
pub struct Record {}

impl Record {
    pub fn from_items<'py>(
        py: Python<'py>,
        items: &Vec<(PyObject, PyObject)>,
    ) -> PyResult<Bound<'py, Self>> {
        let rec = Py::new(py, Self::default())?.into_bound(py);
        let dict = rec.downcast::<PyDict>()?;

        for (item_key, item_value) in items {
            dict.set_item(item_key, item_value)?;
        }

        Ok(rec)
    }
}

#[pymethods]
impl Record {
    #[new]
    #[pyo3(signature = (*args, **kwargs))]
    #[allow(unused_variables)]
    fn new(args: &Bound<'_, PyAny>, kwargs: Option<&Bound<'_, PyAny>>) -> Self {
        Record::default()
    }

    fn __getattr__(slf: &Bound<'_, Self>, name: &str) -> PyResult<PyObject> {
        let dict = slf.downcast::<PyDict>()?;

        if let Some(value) = dict.get_item(name)? {
            Ok(value.unbind())
        } else {
            Err(PyErr::new::<pyo3::exceptions::PyKeyError, _>(format!(
                "'Record' object has no key '{}'",
                name
            )))
        }
    }

    fn __setattr__(slf: &Bound<'_, Self>, name: &str, value: PyObject) -> PyResult<()> {
        let dict = slf.downcast::<PyDict>()?;
        dict.set_item(name, value)?;
        Ok(())
    }

    fn __repr__(slf: &Bound<'_, Self>) -> PyResult<String> {
        let dict = slf.downcast::<PyDict>().unwrap();
        let mut s = String::new();
        let py = slf.py();

        if dict.is_empty() {
            return Ok("Record()".to_string());
        }

        for (i, (key, value)) in dict.iter().enumerate() {
            if i > 0 {
                s.push_str(", ");
            }

            let key_repr = if key.is_none() {
                PyString::new(py, "None")
            } else if let Ok(key_int) = key.downcast::<pyo3::types::PyInt>() {
                key_int.repr()?
            } else if let Ok(key_bool) = key.downcast::<pyo3::types::PyBool>() {
                key_bool.repr()?
            } else if let Ok(key_float) = key.downcast::<pyo3::types::PyFloat>() {
                key_float.repr()?
            } else if let Ok(key_str) = key.downcast::<pyo3::types::PyString>() {
                let s = key_str.to_string_lossy().to_string();
                if is_valid_ident(&s) {
                    PyString::new(py, &s)
                } else {
                    PyString::new(py, &format!("({})", key_str.repr()?))
                }
            } else {
                PyString::new(py, &format!("({})", key.repr()?))
            };

            s.push_str(&format!("{}: {}", key_repr, value.repr()?,));
        }

        Ok(format!("{{{}}}", s))
    }
}

enum VGetResult<'py, 'ptr> {
    Value(Bound<'py, PyAny>),
    NeedsBind(Bound<'py, PyAny>),
    SliceIter(&'ptr Bound<'py, PySlice>),
    None,
}

fn get_virtual<'py, 'ptr>(
    obj: &'ptr Bound<'py, PyAny>,
    name: &'ptr Bound<'py, PyString>,
) -> PyResult<VGetResult<'py, 'ptr>> {
    if let Ok(attr) = obj.getattr(name) {
        return Ok(VGetResult::Value(attr));
    }

    if name.to_string() == "iter" {
        if let Ok(items) = obj.getattr("items") {
            return Ok(VGetResult::Value(items.call0()?.into()));
        }

        if let Ok(iter) = obj.try_iter() {
            return Ok(VGetResult::Value(iter.into_any()));
        }

        if let Ok(sl) = obj.downcast::<PySlice>() {
            return Ok(VGetResult::SliceIter(sl));
        }
    }

    let module = PyModule::import(obj.py(), "koatl._rs")?;
    let types_vtbl = module.getattr("types_vtbl")?.downcast_into::<PyDict>()?;

    if let Ok(Some(types)) = types_vtbl.get_item(name) {
        let types = types.downcast::<PyDict>()?;

        let mro = obj.get_type().mro();
        for typ in mro {
            if let Some(attr) = types.get_item(typ)? {
                return Ok(VGetResult::NeedsBind(attr.into()));
            }
        }
    }

    let traits_vtbl = module.getattr("traits_vtbl")?.downcast_into::<PyDict>()?;

    if let Ok(Some(traits)) = traits_vtbl.get_item(name) {
        let types = traits.downcast::<PyDict>()?;

        for (typ, attr) in types.iter() {
            if obj.is_instance(&typ)? {
                return Ok(VGetResult::NeedsBind(attr.into()));
            }
        }
    }

    Ok(VGetResult::None)
}

#[pyfunction(signature=(obj, name))]
fn vcheck(obj: &Bound<'_, PyAny>, name: &Bound<'_, PyString>) -> PyResult<bool> {
    match get_virtual(obj, name)? {
        VGetResult::Value(_) => Ok(true),
        VGetResult::NeedsBind(_) => Ok(true),
        VGetResult::SliceIter(_) => Ok(true),
        VGetResult::None => Ok(false),
    }
}

#[pyfunction(signature=(obj, name))]
fn vget(obj: &Bound<'_, PyAny>, name: &Bound<'_, PyString>) -> PyResult<PyObject> {
    fn bind_attr<'py>(obj: &Bound<'py, PyAny>, attr: &Bound<'py, PyAny>) -> PyResult<PyObject> {
        if attr.hasattr("ext_prop")? {
            return Ok(attr.call1((obj,))?.unbind());
        }

        let g = PyModule::import(obj.py(), "functools")?;
        Ok(g.getattr("partial")?.call1((attr, obj))?.unbind())
    }

    let got = get_virtual(obj, name)?;
    let py = obj.py();

    match got {
        VGetResult::Value(value) => return Ok(value.into()),
        VGetResult::NeedsBind(attr) => return bind_attr(obj, &attr),
        VGetResult::SliceIter(slice) => unsafe {
            let slice_obj = slice.as_ptr() as *mut ffi::PySliceObject;
            let start = (*slice_obj).start;
            let stop = (*slice_obj).stop;
            let step = (*slice_obj).step;

            let step = if step == ffi::Py_None() {
                1
            } else if ffi::PyLong_Check(step) == 0 {
                return Err(PyErr::new::<pyo3::exceptions::PyTypeError, _>(
                    "Slice step must be an integer or None to iterate".to_string(),
                ));
            } else {
                ffi::PyLong_AsSsize_t(step)
            };

            let start = if start == ffi::Py_None() {
                0
            } else if ffi::PyLong_Check(start) == 0 {
                return Err(PyErr::new::<pyo3::exceptions::PyTypeError, _>(
                    "Slice start must be an integer or None to iterate".to_string(),
                ));
            } else {
                ffi::PyLong_AsSsize_t(start)
            };

            if stop == ffi::Py_None() {
                let itertools = py.import("itertools")?;
                Ok(itertools
                    .getattr("count")?
                    .call1((start, step))?
                    .unbind()
                    .into_any())
            } else if ffi::PyLong_Check(stop) == 0 {
                return Err(PyErr::new::<pyo3::exceptions::PyTypeError, _>(
                    "Slice stop must be an integer or None to iterate".to_string(),
                ));
            } else {
                let stop = ffi::PyLong_AsSsize_t(stop);
                Ok(PyRange::new_with_step(py, start, stop, step)?
                    .unbind()
                    .into_any())
            }
        },
        VGetResult::None => Err(PyErr::new::<pyo3::exceptions::PyAttributeError, _>(
            format!(
                "'{}' object has no v-attribute '{}'",
                obj.get_type().name()?,
                name
            ),
        )),
    }
}

#[pyfunction(signature=(obj,))]
fn ok(obj: &Bound<'_, PyAny>) -> PyResult<bool> {
    return Ok(if obj.is_none() {
        false
    } else if obj.is_instance_of::<PyBaseException>() {
        false
    } else {
        true
    });
}

#[pymodule(name = "_rs")]
fn py_module(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add("types_vtbl", PyDict::new(m.py()))?;
    m.add("traits_vtbl", PyDict::new(m.py()))?;
    m.add_class::<Record>()?;
    m.add_function(wrap_pyfunction!(transpile, m)?)?;
    m.add_function(wrap_pyfunction!(vget, m)?)?;
    m.add_function(wrap_pyfunction!(vcheck, m)?)?;
    m.add_function(wrap_pyfunction!(ok, m)?)?;
    Ok(())
}
