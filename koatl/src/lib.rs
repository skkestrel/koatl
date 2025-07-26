pub mod emit_py;

use koatl_core::{
    format_errs, linecol::LineColCache, transpile_to_py_ast, transpile_to_source, TranspileOptions,
};
use pyo3::{
    prelude::*,
    types::{PyDict, PyList},
};

fn get_option(mode: &str) -> PyResult<TranspileOptions> {
    Ok(match mode {
        "module" => TranspileOptions::module(),
        "prelude" => TranspileOptions::prelude(),
        "interactive" => TranspileOptions::interactive(),
        "script" => TranspileOptions::script(),
        _ => {
            return Err(PyErr::new::<pyo3::exceptions::PyValueError, _>(
                "Invalid transpilation mode.",
            ))
        }
    })
}

#[pyfunction(signature=(src, mode="script", filename="<string>"))]
fn transpile(src: &str, mode: &str, filename: &str) -> PyResult<PyObject> {
    let options = get_option(mode)?;

    let py_ast = transpile_to_py_ast(src, options).map_err(|e| {
        PyErr::new::<pyo3::exceptions::PySyntaxError, _>(format_errs(&e, filename, src))
    })?;

    let py_ast_obj = emit_py::emit_py(&py_ast, src).map_err(|e| {
        PyErr::new::<pyo3::exceptions::PyException, _>(format!("Emission error: {}", e.message))
    })?;
    Ok(py_ast_obj)
}

#[pyfunction(signature=(src, mode="script", filename="<string>"))]
fn transpile_raw(src: &str, mode: &str, filename: &str) -> PyResult<PyObject> {
    let options = get_option(mode)?;

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
}
#[pymodule(name = "_rs")]
fn py_module(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(transpile, m)?)?;
    m.add_function(wrap_pyfunction!(transpile_raw, m)?)?;
    Ok(())
}
