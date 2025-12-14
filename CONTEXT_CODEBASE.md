# Koatl Codebase - Architecture & Module Reference

## Project Structure Overview

```
koatl/          PyO3 binding and Rust crate; Python wheel sources live in python/koatl/
koatl-core/     Core pipeline (parse → scope → infer → transform → emit)
koatl-parser/   Lexer and parser library
ohtli/          Formatter CLI (Rust)
quetzal/        VS Code extension (TypeScript + wasm formatter)
docs/           Documentation site
sample/         Example Koatl programs
```

## Data Flow: Koatl Source → Python Code

```
koatl source (.tl file)
           ↓
    [LEXING]  (lexer.rs)
           ↓
  Token Stream with Trivia
           ↓
    [PARSING]  (parser.rs)
           ↓
  Concrete Syntax Tree (CST)
           ↓
  [CST LIFTING]  (lift_cst.rs)
           ↓
Abstract Syntax Tree (AST)
           ↓
 [SCOPE RESOLUTION]  (resolve_scopes.rs)
           ↓
 AST + Name Binding Info
           ↓
  [TYPE INFERENCE]  (inference.rs)
           ↓
   AST + Type Info
           ↓
  [TRANSFORMATION]  (transform.rs)
           ↓
Python Abstract Syntax Tree (PyAST)
           ↓
[PYTHON CODE EMISSION]  (emit_py.rs / py/emit.rs)
           ↓
  Python source code
           ↓
  [PYTHON EXECUTION]
           ↓
        Output
```

## Module Responsibilities

### koatl-parser

The foundation of the compiler pipeline, implementing lexical analysis and parsing.

**Lexer** (`lexer.rs`):

-   Tokenizes Koatl source into a stream of tokens with full trivia preservation
-   Trivia (whitespace, comments, newlines) is attached to tokens as leading/trailing trivia rather than discarded
-   Supports indentation-based syntax via `Indent`/`Dedent` tokens (Python-like)
-   Handles string interpolation (f-strings) with nested formatting specifications
-   Zero-copy design: tokens contain string slices (`&'src str`) referencing the original source

**Parser** (`parser.rs`):

-   Pratt parser-based recursive descent parser that builds a Concrete Syntax Tree (CST)
-   Error recovery strategy: tracks the furthest parsing position that failed, allowing the parser to continue collecting errors rather than stopping at the first error
-   Uses backtracking combinators (`first_of!`, `optional!`) to try multiple parse paths
-   Preserves all syntactic information including delimiters, separators, and newlines for accurate formatter reconstruction
-   The parser context (`ParseCtx`) maintains:
    -   Current position in token stream
    -   Error accumulation: stores errors in `cur_error` and only commits them when certain they're valid
    -   Backtracking state via `save()`/`rewind()` for speculative parsing

**Error Handling Approach**:
The parser uses a "furthest failure" heuristic: when multiple parse paths fail, it reports the error from the path that consumed the most tokens, as this is likely closest to the user's intent. Errors are accumulated in `ParseCtx.errors` rather than immediately aborting, enabling comprehensive error reporting in a single pass.

**CST** (`cst.rs`):

-   Generic tree representation via the `Tree` trait, allowing different backends (syntax highlighting, formatting, semantic analysis)
-   Preserves all tokens including structural elements (parentheses, commas, colons)
-   Maintains exact correspondence to source text for perfect round-tripping

### koatl-core

The semantic analysis and code generation pipeline, transforming Koatl AST to Python AST.

**CST Lifting** (`lift_cst.rs`):

-   Converts the concrete syntax tree (CST) into an abstract syntax tree (AST)
-   Discards purely syntactic elements (parentheses, separators) while preserving semantic content
-   Lifts string interpolation (f-strings) into structured `Expr::Fstr` nodes
-   Maintains span information for error reporting

**Scope Resolution** (`resolve_scopes.rs`):

-   Implements name binding and lexical scoping analysis
-   Builds a scope tree tracking variable declarations, function definitions, and imports
-   Resolves identifier references to their declarations using a `HashMap<RefHash, DeclarationKey>`
-   Detects scope errors: undefined variables, duplicate declarations, invalid references
-   Tracks special constructs:
    -   Function metadata (`FnInfo`): parameter info, async/generator status, memoization
    -   Pattern bindings (`PatternInfo`): destructuring assignments, match patterns
    -   Export tracking: module-level exports for Python interop
-   Uses `SlotMap` for efficient scope and declaration storage with stable keys
-   Maintains a stack of active scopes (`scope_stack`) and function contexts (`fn_stack`) during traversal

**Type Inference** (`inference.rs`):

-   Basic type inference for optimization hints (currently limited)
-   Tracks expression types using `HashMap<RefHash, Type>`
-   Distinguishes between `Type::NoReturn` (statements), `Type::Bottom` (never returns), and `Type::Any`
-   Foundation for future type-based optimizations

**Transformation** (`transform.rs`):

-   Transforms Koatl AST to Python AST
-   Python keyword escaping: renames identifiers that conflict with Python keywords (e.g., `class` → `class_`)
-   Translates Koatl-specific features to Python equivalents:
    -   Pipeline operators → function composition
    -   Pattern matching → if/elif chains or match statements (Python 3.10+)
    -   Memoization decorators → `@memo` function wrappers
    -   Optional chaining (`?.`) → conditional attribute access with guards
    -   Spread operators in various contexts
-   Maintains source mapping for debugging and error messages
-   Generates unique identifiers for compiler-introduced temporaries

**Python Code Emission** (`py/emit.rs`):

-   Converts Python AST to Python source code strings
-   Precedence-aware expression printing to minimize parentheses
-   Handles Python-specific string escaping for both regular strings and f-strings
-   Maintains line mapping from generated Python back to original Koatl source (`source_line_map`)
-   Tracks indentation state for proper code formatting

### koatl

PyO3-based Python bindings exposing the compiler to Python.

**Main API** (`lib.rs`):

-   `transpile()`: Full compilation to Python AST objects (usable by Python `compile()`)
-   `transpile_raw()`: Compilation to Python source string with source map
-   `fast_vget()`: Runtime support for virtual method dispatch (trait system implementation)
-   Manages transpilation modes:
    -   `module`: Full module with prelude and runtime imports
    -   `script`: Standalone script without module exports
    -   `interactive`: REPL mode with top-level await support
    -   `no_prelude`: For compiling the prelude itself (bootstrap mode)

**Python AST Emission** (`emit_py.rs`):

-   Converts Koatl's Python AST representation into Python's native AST objects
-   Uses PyO3 to construct Python `ast.Module`, `ast.Expr`, `ast.Stmt`, etc.
-   Enables seamless integration with Python's compilation pipeline

### ohtli

Sophisticated code formatter for Koatl with a three-stage architecture.

**Architecture Overview**:

1. **CST → Element Tree**:

    - Converts parsed CST into an intermediate representation of layout "elements"
    - Elements are atomic units: `Atom` (tokens), `LineBreak`, `Listing` (comma-separated items), `Parens` (delimited groups), `Block` (indented statements)
    - Trivia (comments, whitespace) is extracted from tokens and converted to elements
    - Attachment flags (`attach_before`, `attach_after`) indicate whether spaces are needed around elements

2. **Layout Calculation** (`LayoutCalculator`):

    - Currently a pass-through stage (opportunity for future optimizations)
    - Could implement line-length aware decisions (inline vs. block layout selection)
    - Recursively processes element tree, potentially rewriting inline/block choices
    - Placeholder for "pretty printing" algorithms (e.g., Wadler-Leijen)

3. **Layout Writing** (`LayoutWriter`):
    - Converts element tree to final formatted string
    - Manages indentation state via a stack of `WriterLineInfo` (tracks whether line has content, was just broken, needs continuation indent)
    - **Continuation indentation heuristic**: If multiple atoms appear on separate lines within a single logical line, subsequent atoms get extra indentation (e.g., chained method calls)
    - Respects attachment flags: omits spaces when `attach_next` is true
    - Handles inline vs. block layout:
        - Inline listings: rendered on single line with spaces
        - Block listings: each item on separate line with increased indentation
        - Inline blocks: semicolon-separated statements
        - Block blocks: standard indented statements
    - Special handling for comments: preserves them but doesn't affect indentation continuation logic

**Error Handling in Formatting**:
When the source has parse errors, the formatter preserves the unparseable regions as `Stmt::Error { raw }` nodes. These are tokenized to extract trivia, then all tokens and trivia are converted to tightly-attached elements, preserving the exact original formatting for the error region while formatting the rest of the file.

**Config** (`config.rs`):

-   Formatter configuration (currently just `indent_width`)
-   Designed for extension with line length limits, style preferences, etc.

### quetzal

VS Code extension integrating Koatl tooling into the editor.

**Extension** (`extension.ts`):

-   Loads formatter as WebAssembly module compiled from Rust
-   Registers document formatting provider for `.tl` files
-   Uses Wasm Component Model for TypeScript ↔ Rust interop
-   Converts diff hunks from formatter into VS Code `TextEdit` operations
-   Applies incremental edits rather than replacing entire file (preserves undo history)

**Formatter Bridge** (`formatter.ts`, generated):

-   WIT-based interface definitions for calling Rust formatter from TypeScript
-   Exposes `formatDiff()` function returning line-based diff hunks

**Rust Formatter** (`lib.rs`):

-   Wasm-compatible Rust implementation wrapping `ohtli::Formatter`
-   Generates unified-diff-style output for efficient editor updates

### profiling

Minimal CLI tool for performance profiling of the compiler pipeline.

**Purpose**: Benchmark transpilation end-to-end with various input files to identify bottlenecks.

## Testing

### Architecture

-   Pytest (under `koatl/tests/`) drives the PyO3 module. `conftest.py` adds a custom collector so any `test_*.tl` file can be transpiled and executed as a pytest module.
-   End-to-end harness: `test_e2e.py` parametrizes over `.tl` fixtures in `tests/e2e/base` and `tests/e2e/prelude`, running them through `koatl.transpile_raw` and the CLI.
-   Parser coverage: `test_parse.py` walks `tests/parse/*.tl` to ensure parsing succeeds; `test_parse_fail.py` walks `tests/fail-parse/*.tl` to assert parsing fails.
-   Additional `.tl` samples (for example `test_iterable.tl`, `test_re.tl`) are collected by pytest via the custom collector.
-   Rust crates (`koatl-core`, `koatl-parser`, `ohtli`, the Rust portion of `quetzal`) use the standard cargo test workflow.

### Running Tests

-   Pytest for the PyO3 module:

```bash
cd koatl
pyenv activate pyo3   # if using pyenv
maturin develop        # build the extension in-place
pytest                 # runs e2e, parse, fail-parse, and collected .tl modules
```

-   Cargo tests for Rust crates from the workspace root:

```bash
cargo test
cargo test -p koatl-core   # run a single crate
```

---

## Important Design Decisions

1. **Lifetime Parameters**: Extensive use of `'src` and `'tok` lifetimes for zero-copy processing of source text
2. **Trivia Preservation**: Whitespace and comments preserved through lexing to enable accurate error reporting
3. **Two-stage Scoping**: CST parsed first, then scope resolution prevents re-parsing during semantic analysis
4. **Immutable Data**: Extensive use of immutable data structures for safety and parallelizability
5. **Error Accumulation**: Collecting all errors before failing improves user experience
6. **Modular AST**: Tree trait allows flexibility in AST implementations (Koatl AST, Python AST)

---

## Build & Development Workflow

```bash
cargo run -p koatl-core trans file_to_transpile.tl
```

## Related Documentation

-   **Language Reference**: See CONTEXT_LANGUAGE.md
-   **Example Code**: See sample/ directory
-   **User Documentation**: See docs/ directory
