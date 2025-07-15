# Before everything

https://github.com/pyenv/pyenv-installer

```
pyenv virtualenv pyo3
pyenv local pyo3
pip install maturin
```

# Building cli tool

```
cargo test
cargo build
target/debug/coatl trans sample/hello_world.tl
```

# Running e2e tests

```
cd quetzal
npm install
npm test
```

# Installing python package

```
maturin develop -m coatl-python/Cargo.toml
maturin build --release -m coatl-python/Cargo.toml
```

# Installing the kernel

```
cd coatl-kernel
pip install .
```
