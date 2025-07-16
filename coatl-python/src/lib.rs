pub mod emit_py;

use coatl::TranspileOptions;
use pyo3::prelude::*;

#[pyfunction(signature=(src, mode="module"))]
fn transpile(src: &str, mode: &str) -> PyResult<PyObject> {
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

    let py_ast = coatl::transpile_to_py_ast(src, options).map_err(|e| {
        PyErr::new::<pyo3::exceptions::PyException, _>(format!(
            "Transpilation error: {}",
            e.iter()
                .map(|err| err.message.clone())
                .collect::<Vec<_>>()
                .join(", ")
        ))
    })?;

    let py_ast_obj = emit_py::emit_py(&py_ast, src).map_err(|e| {
        PyErr::new::<pyo3::exceptions::PyException, _>(format!("Emission error: {}", e.message))
    })?;

    Ok(py_ast_obj)
}

#[pymodule(name = "_rs")]
fn py_module(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(transpile, m)?)?;
    Ok(())
}
