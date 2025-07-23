pub mod emit_py;

use koatl_core::{
    format_errs, linecol::LineColCache, parser::lexer::is_valid_ident,
    transpile as transpile_to_source, transpile_to_py_ast, TranspileOptions,
};
use pyo3::{
    prelude::*,
    types::{PyDict, PyList, PySlice, PyString},
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
struct Record {}

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

        Ok(format!("[{}]", s))
    }
}

#[pyfunction(signature=(obj, name))]
fn vget(obj: &Bound<'_, PyAny>, name: &Bound<'_, PyString>) -> PyResult<PyObject> {
    if name.to_string() == "iter" {
        if let Ok(slice) = obj.downcast::<PySlice>() {
            // TODO move this into rs
            let module = PyModule::import(obj.py(), "koatl.runtime")?;
            return Ok(module.getattr("_slice_iter")?.call1((slice,))?.unbind());
        }

        if let Ok(items) = obj.getattr("items") {
            return Ok(items.call0()?.unbind());
        }

        if let Ok(iter) = obj.try_iter() {
            return Ok(iter.unbind().into_any());
        }
    }

    if let Ok(attr) = obj.getattr(name) {
        return Ok(attr.unbind());
    }

    // TODO add trait-based lookup?

    let module = PyModule::import(obj.py(), "koatl._rs")?;
    let tbl = module.getattr("vtbl")?.downcast_into::<PyDict>()?;

    if let Ok(Some(types)) = tbl.get_item(name) {
        let types = types.downcast::<PyDict>()?;
        let obj_type = obj.get_type();
        for typ in obj_type.mro() {
            if let Ok(Some(value)) = types.get_item(typ) {
                return Ok(value.unbind());
            }
        }
    }

    Err(PyErr::new::<pyo3::exceptions::PyAttributeError, _>(
        format!(
            "'{}' object has no v-attribute '{}'",
            obj.get_type().name()?,
            name
        ),
    ))
}

#[pymodule(name = "_rs")]
fn py_module(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(transpile, m)?)?;
    m.add_class::<Record>()?;
    m.add("vtbl", PyDict::new(m.py()))?;
    m.add_function(wrap_pyfunction!(vget, m)?)?;
    Ok(())
}
