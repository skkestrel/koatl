pub mod emit;

use pyo3::prelude::*;

#[pyfunction]
fn transpile(src: &str) -> PyResult<PyObject> {
    let py_ast = coatl::transpile_to_py_ast(src).map_err(|e| {
        PyErr::new::<pyo3::exceptions::PyException, _>(format!(
            "Transpilation error: {}",
            e.iter()
                .map(|err| err.message.clone())
                .collect::<Vec<_>>()
                .join(", ")
        ))
    })?;

    let py_ast_obj = emit::emit_py(&py_ast, src).map_err(|e| {
        PyErr::new::<pyo3::exceptions::PyException, _>(format!("Emission error: {}", e.message))
    })?;

    Ok(py_ast_obj)
}

#[pymodule(name = "_rs")]
fn py_module(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(transpile, m)?)?;
    Ok(())
}
