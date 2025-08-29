# Ohtli - Koatl Code Formatter

Ohtli is a code formatter for the Koatl programming language. It automatically formats Koatl source code according to configurable style guidelines.

## Features

-   **Automatic code formatting**: Formats Koatl source files with consistent style
-   **Configurable**: Customizable formatting options including indentation, line length, and spacing
-   **Check mode**: Verify if files are properly formatted without modifying them
-   **Diff mode**: Show what changes would be made without applying them
-   **Fast and reliable**: Built on the official Koatl parser

## Installation

Build from source:

```bash
cargo build --release
```

The binary will be available at `target/release/ohtli`.

## Usage

### Format a file in-place

```bash
ohtli example.tl
```

### Check if a file is formatted correctly

```bash
ohtli --check example.tl
```

### Show formatting differences

```bash
ohtli --diff example.tl
```

## Configuration

Ohtli supports various configuration options:

-   `indent_width`: Number of spaces per indentation level (default: 4)
-   `max_line_length`: Maximum line length before wrapping (default: 88)
-   `trailing_commas`: Whether to use trailing commas in multi-line contexts (default: true)
-   `spaces_around_operators`: Whether to put spaces around binary operators (default: true)

Configuration file support is planned for future releases.

## Examples

### Before formatting:

```koatl
x=1+2*3
def foo(a,b,c):
return a+b+c
```

### After formatting:

```koatl
x = 1 + 2 * 3

def foo(a, b, c):
    return a + b + c
```

## Development

Ohtli is built using the Koatl parser infrastructure:

-   `koatl-parser`: Lexical analysis and parsing
-   `koatl-core`: Core language constructs

The formatter works by:

1. Tokenizing the source code using the Koatl lexer
2. Parsing tokens into a Concrete Syntax Tree (CST)
3. Traversing the CST and emitting formatted code

## Contributing

Contributions are welcome! Please see the main Koatl repository for contribution guidelines.

## License

Licensed under either of

-   Apache License, Version 2.0
-   MIT License

at your option.
