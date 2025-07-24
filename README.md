# Homepage and docs

https://koatl.org/

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

pytest
koatl ../sample/sunset_timer.tl
```

## Deploying

Bump version number in Cargo.toml, then

```
maturin publish
```

# Installing the kernel

```
cd koatl-kernel
pip install -e .
koatl
```

## Deploying kernel

Version number is in `koatl_kernel/__init__.py`

```
python3 -m build
python3 -m twine upload --repository pypi dist/*
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
