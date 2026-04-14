# Koatl Development

Read `language-quickref.md` for a description of the Koatl (.tl) language.

## Testing

1. Build the native Python extension:

    ```
    cd koatl && pyenv activate pyo3 && maturin develop
    ```

2. Run tests:
    ```
    cd koatl && pytest
    ```
