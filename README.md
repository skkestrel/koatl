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
python3 -m coatl ../sample/hello_world.tl
```

# Building cli tool

```
cargo test
cargo build
target/debug/coatl trans sample/hello_world.tl
```

# Installing the kernel

```
cd coatl-kernel
pip install .
```
