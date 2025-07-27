pub mod emit_py;

use once_cell::sync::Lazy;
use std::{collections::HashMap, sync::Mutex};

use koatl_core::{
    format_errs, linecol::LineColCache, transpile_to_py_ast, transpile_to_source, TranspileOptions,
};
use pyo3::{
    prelude::*,
    types::{PyDict, PyList, PyString, PyType},
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

static VTBL: Lazy<Mutex<HashMap<String, HashMap<usize, PyObject>>>> =
    Lazy::new(|| Mutex::new(HashMap::new()));

#[allow(dead_code)]
struct TraitAttr {
    name: String,
    requirements: Vec<String>,
    value: PyObject,
}

static VTBL2: Lazy<Mutex<HashMap<String, Vec<TraitAttr>>>> =
    Lazy::new(|| Mutex::new(HashMap::new()));

#[pyfunction(signature=(obj, name))]
fn fast_vget<'py, 'ptr>(
    obj: &'ptr Bound<'py, PyAny>,
    name: &'ptr Bound<'py, PyString>,
) -> PyResult<PyObject> {
    let name = name.to_string();

    let py = obj.py();

    let vtbl = VTBL.lock().unwrap();

    if let Some(types) = vtbl.get(&name) {
        let mro = obj.get_type().mro();

        for typ in mro {
            if let Some(attr) = types.get(&(typ.as_ptr() as usize)) {
                return Ok((*attr).clone_ref(py));
            }
        }
    }

    let vtbl2 = VTBL2.lock().unwrap();

    if let Some(traits) = vtbl2.get(&name) {
        let tlmod = py.import("koatl.runtime.virtual")?;
        let vget = tlmod.getattr("vget")?;

        for t in traits {
            let mut ok = true;
            for r in t.requirements.iter() {
                if vget.call1((obj, r)).is_err() {
                    ok = false;
                    break;
                }
            }

            if ok {
                return Ok(t.value.clone_ref(py));
            }
        }
    }

    Ok(py.None().into_any())
}

#[pyfunction(signature=(typ, name, value))]
fn fast_vset<'py, 'ptr>(
    typ: &'ptr Bound<'py, PyType>,
    name: &'ptr Bound<'py, PyString>,
    value: &'ptr Bound<'py, PyAny>,
) -> PyResult<()> {
    let name = name.to_string();

    let mut vtbl = VTBL.lock().unwrap();
    if !vtbl.contains_key(&name) {
        vtbl.insert(name.clone(), HashMap::new());
    }

    vtbl.get_mut(&name)
        .unwrap()
        .insert(typ.as_ptr() as usize, value.clone().unbind());

    Ok(())
}

#[pyfunction(signature=(trait_name, reqs, name, value))]
fn fast_vset_trait<'py, 'ptr>(
    trait_name: &'ptr Bound<'py, PyString>,
    reqs: &'ptr Bound<'py, PyList>,
    name: &'ptr Bound<'py, PyString>,
    value: &'ptr Bound<'py, PyAny>,
) -> PyResult<()> {
    let name = name.to_string();
    let reqs = reqs
        .iter()
        .map(|r| r.extract::<String>())
        .collect::<PyResult<Vec<_>>>()?;

    let mut vtbl = VTBL2.lock().unwrap();

    if !vtbl.contains_key(&name) {
        vtbl.insert(name.clone(), Vec::new());
    }

    vtbl.get_mut(&name).unwrap().push(TraitAttr {
        name: trait_name.to_string(),
        requirements: reqs,
        value: value.clone().unbind(),
    });

    Ok(())
}

#[pymodule(name = "_rs")]
fn py_module(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(transpile, m)?)?;
    m.add_function(wrap_pyfunction!(transpile_raw, m)?)?;

    m.add_function(wrap_pyfunction!(fast_vget, m)?)?;
    m.add_function(wrap_pyfunction!(fast_vset, m)?)?;
    m.add_function(wrap_pyfunction!(fast_vset_trait, m)?)?;
    Ok(())
}
