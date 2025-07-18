# Before everything

https://github.com/pyenv/pyenv-installer

```
cd coatl
pyenv virtualenv pyo3
pyenv local pyo3
pip install -r requirements.txt

maturin develop -m ./Coatl.toml
# maturin build --release -m coatl/Cargo.toml

pytest
coatl ../sample/sunset_timer.tl
```

# Installing the kernel

```
cd coatl-kernel
pip install .
coatl
```

# Building cli tool

```
cargo test
cargo build
target/debug/coatl trans sample/hello_world.tl
```
