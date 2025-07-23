# Installation

```
pip install koatl koatl-kernel
```

Language provider in vscode: "quetzal-koatl"

# Development

https://github.com/pyenv/pyenv-installer

```
cd koatl
pyenv virtualenv pyo3
pyenv local pyo3
pip install -r requirements.txt

maturin develop -m ./Koatl.toml
# maturin build --release -m koatl/Cargo.toml

pytest
koatl ../sample/sunset_timer.tl
```

# Installing the kernel

```
cd koatl-kernel
pip install -e .
koatl
```

# Serving docs locally

```
cd docs
npx docsify init
```

# Building cli tool

```
cargo test
cargo build
target/debug/koatl trans sample/hello_world.tl
```
