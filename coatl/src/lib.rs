pub mod emit_py;

use coatl_core::{
    format_errs, linecol::LineColCache, transpile as transpile_to_source, transpile_to_py_ast,
    TranspileOptions,
};
use pyo3::{
    prelude::*,
    types::{PyDict, PyList},
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

#[pymodule(name = "_rs")]
fn py_module(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(transpile, m)?)?;
    Ok(())
}
