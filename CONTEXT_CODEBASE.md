# Koatl Codebase - Architecture & Module Reference

## Project Structure Overview

```
koatl/                          # Main Python binding & emission
  ├── src/emit_py.rs           # Python AST emission
  ├── src/lib.rs               # PyO3 Python interface
  └── Cargo.toml

koatl-parser/                   # Lexing and parsing
  ├── src/lexer.rs             # Tokenization with trivia preservation
  ├── src/cst.rs               # Concrete Syntax Tree definitions
  ├── src/parser.rs            # Recursive descent parser
  ├── src/simple_fmt.rs        # Formatting utilities
  └── Cargo.toml

koatl-core/                     # Core language compilation
  ├── src/lib.rs               # Main transpile entry points
  ├── src/ast.rs               # Abstract Syntax Tree definitions
  ├── src/ast_builder.rs       # AST construction helpers
  ├── src/lift_cst.rs          # CST to AST conversion
  ├── src/transform.rs         # AST to Python AST transformation
  ├── src/resolve_scopes.rs    # Name resolution & scope tracking
  ├── src/inference.rs         # Type inference (referenced, not impl shown)
  ├── src/types.rs             # Type system definitions
  ├── src/util.rs              # Error handling & utilities
  ├── src/main.rs              # CLI transpiler tool
  └── src/py/                  # Python AST and emission
      ├── ast.rs               # Python AST structures
      ├── ast_builder.rs       # Python AST helpers
      ├── emit.rs              # Python code generation
      └── ...
  └── Cargo.toml

ohtli/                          # Code formatter
  ├── src/main.rs              # CLI formatter entry point
  ├── src/formatter.rs         # Formatting implementation
  ├── src/config.rs            # Formatter configuration
  └── Cargo.toml

quetzal/                        # VSCode extension
  ├── src/extension.ts         # VSCode extension host
  ├── src/formatter.ts         # Formatter integration
  ├── syntaxes/tl.tmLanguage.json  # TextMate grammar
  └── package.json

docs/                           # Documentation
  ├── intro.md
  ├── match.md
  ├── modules.md
  ├── monads.md
  ├── operators.md
  ├── prelude.md
  ├── containers.md
  ├── extensions.md
  ├── formatting.md
  └── index.html

sample/                         # Example programs
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

### **koatl-parser** - Lexical Analysis & Parsing

#### `lexer.rs` (~2100 lines)

**Responsibility**: Convert Koatl source text into tokens with trivia preservation

**Key Structures**:

-   `Token<'src>` - Enumeration of token types:
    -   `Ident`, `Kw`, `Symbol` - Keywords and identifiers
    -   `Int`, `IntHex`, `IntOct`, `IntBin`, `Float` - Numeric literals
    -   `Str`, `FstrBegin`, `FstrEnd`, etc. - String and f-string tokens
    -   `Indent`, `Dedent`, `Eol` - Whitespace-sensitive tokens
-   `Span` - Source location (start byte, end byte)
-   `SToken<'src>` - Token with span and associated trivia
-   `TrivialToken<'src>` - Whitespace, comments, newlines (preserves formatting)
-   `TokenList<'src>` - Vector of tokens for parser input

**Key Functions**:

-   `tokenize()` - Main lexer entry point, produces TokenList
-   Handles multi-line strings, f-strings, nested block comments (`#- -#`)
-   Tracks indentation for Python-like scoping
-   Preserves all non-semantic whitespace and comments

**Design Notes**:

-   Uses `CharIndices` iterator for efficient character-by-character scanning
-   Preserves trivia (comments, whitespace) for accurate error messages and formatting
-   Supports both `.tl` file syntax and interactive notebook mode

#### `cst.rs`

**Responsibility**: Define the Concrete Syntax Tree (preserves syntax structure exactly as written)

**Key Structures**:

-   `SStmts<'src, 'tok>` - Block of statements with trivia
-   `STree<'src, 'tok>` - Either a statement or expression
-   `Stmt<'src, 'tok>` - Statement types (declarations, assignments, expressions, etc.)
-   `Expr<'src, 'tok>` - Expression types (literals, identifiers, function calls, pattern matching, etc.)
-   `Pattern<'src, 'tok>` - Pattern matching constructs
-   `CallItem<'src, 'tok>` - Function call arguments (positional, keyword, unpacking)
-   `ListItem<'src, 'tok>` - List elements (values, unpacking)
-   `MappingItem<'src, 'tok>` - Record/dict entries
-   `FmtSpec<'src, 'tok>` - Format specifiers in f-strings

**Design Notes**:

-   Mirrors Python's grammar but captures Koatl-specific syntax
-   Fully preserves formatting and comments in trivia
-   Intermediate representation between parsing and transformation

#### `parser.rs` (~2559 lines)

**Responsibility**: Parse token stream into CST using recursive descent with error recovery

**Key Functions**:

-   `parse_tokens(tokens) -> (CST, Errors)` - Main parser entry
-   `parse_expr()` - Parse expressions with operator precedence
-   `parse_stmt()` - Parse statements
-   `parse_pattern()` - Parse pattern matching syntax
-   `parse_call()` - Parse function call arguments

**Parser Techniques**:

-   Recursive descent with lookahead
-   Operator precedence climbing for expressions
-   Error recovery to collect multiple errors in one pass
-   Macros: `first_of!` for choice points, `optional!` for optional constructs
-   `ExprPrec` enum for precedence levels

**Key Parsing Features**:

-   Handles Python-like indentation
-   Supports Koatl-specific syntax:
    -   Arrow functions (`=>`)
    -   Pattern matching (`match`)
    -   Pipe operator (`|`)
    -   Check expressions
    -   Placeholder variables (`$`)
    -   Import syntax simplifications

#### `simple_fmt.rs`

**Responsibility**: Utility functions for formatting and manipulating parsed code

---

### **koatl-core** - Semantic Analysis & Transformation

#### `lib.rs` (~291 lines)

**Responsibility**: Main transpilation API and orchestration

**Key Functions**:

-   `transpile_to_py_ast(src, filename, options) -> PyBlock<'src>` - Main entry point

    -   Parses Koatl source
    -   Resolves names and scopes
    -   Infers types
    -   Transforms to Python AST
    -   Handles TranspileOptions (prelude injection, exports, etc.)

-   `parse_tl(src) -> (Option<AST>, Errors)` - Top-level parse
-   `TranspileOptions` - Control transpilation behavior:
    -   `inject_prelude` - Include prelude imports
    -   `inject_runtime` - Include runtime support
    -   `set_exports` - Auto-set `__all__`
    -   `allow_await` - Allow async/await
    -   `interactive` - Interactive mode (REPL)
    -   `target_version` - Python version target

**Error Handling**:

-   Collects errors from each phase
-   Uses ariadne for formatted error reporting with source context

#### `ast.rs` (~307 lines)

**Responsibility**: Define Abstract Syntax Tree (semantic structure)

**Key Type Aliases**:

-   `Indirect<T>` = `Box<T>` - Boxing for recursive types
-   `SIdent<'a>` = `Spanned<Ident<'a>>` - Identifier with span
-   `SLiteral<'a>` = `Spanned<Literal<'a>>` - Literal with span
-   `SExpr<'a>` = `Spanned<Expr<'a, STree<'a>>>` - Expression with span
-   `SStmt<'a>` = `Spanned<Stmt<'a, STree<'a>>>` - Statement with span
-   `SPattern<'a>` = `Spanned<Pattern<'a, STree<'a>>>` - Pattern with span

**Key Enums**:

-   `Ident<'a>` - Identifier name
-   `Literal<'a>` - Numeric, string, bool, None constants
-   `DeclType` - Declaration modifiers (Let, Const, Global, Export)
-   `Stmt<'a, TTree>` - Statement variants:
    -   `Decl` - Variable declarations
    -   `Assign` - Assignments
    -   `PatternAssign` - Pattern-based assignments
    -   `Expr` - Expression statements
    -   `Return`, `While`, etc.
-   `Expr<'a, TTree>` - Expression variants:
    -   `Literal` - Constants
    -   `Ident` - Variable references
    -   `Call` - Function calls
    -   `Fn` - Function definitions
    -   `If`, `Match`, `Try` - Control flow
    -   `BinaryOp`, `UnaryOp` - Operations
    -   `Fstr` - F-strings
    -   `Check` - Error handling
    -   And many more...
-   `Pattern<'a, TTree>` - Pattern variants:
    -   `Capture` - Bind to variable
    -   `Literal`, `Class`, `Sequence`, `Mapping` - Matching constructs
    -   `MatchAs`, `MatchOr` - Pattern combinators

**Design Notes**:

-   Generic `TTree: Tree` trait for flexibility
-   `STree<'src>` concrete implementation
-   Fully typed and structured (unlike CST which preserves formatting)
-   Ready for semantic analysis

#### `lift_cst.rs` (~660 lines)

**Responsibility**: Convert CST to AST (strip formatting, extract structure)

**Key Functions**:

-   `lift_cst(cst) -> Indirect<SExpr>` - Main CST-to-AST conversion
-   `lift_fstr()` - Convert f-string CST to AST
-   `STokenExt` trait methods:
    -   `lift_as_ident()` - Convert token to identifier
    -   `lift_as_literal()` - Convert token to literal value
    -   `lift_as_decl_modifier()` - Map keywords to DeclType

**Transformations**:

-   Removes all trivia (comments, whitespace)
-   Converts parenthesized expressions to explicit AST structure
-   Handles special cases:
    -   F-strings with format specifiers
    -   Import statement simplification
    -   Pattern matching constructs

#### `resolve_scopes.rs`

**Responsibility**: Name resolution and scope tracking

**Key Structures** (inferred from usage in transform.rs):

-   `ResolveState` - Output of name resolution:
    -   `functions` - Map of function definitions to FnInfo
    -   `patterns` - Map of patterns to PatternInfo
    -   `resolutions` - Map of expression refs to declarations
    -   `memo_fninfo`, `mapped_fninfo`, `coal_fninfo` - Special function tracking
    -   `scopes` - Scope information
    -   `declarations` - All variable declarations with metadata

**Responsibilities**:

-   Builds scope hierarchy
-   Tracks variable binding and usage
-   Detects shadowing
-   Records function information (parameters, closures)
-   Handles Koatl scoping rules (unlike Python, no nonlocal needed)

#### `inference.rs`

**Responsibility**: Type inference system (not fully shown)

**Inferred from context**:

-   `InferenceCtx` - Stores type information
    -   `.types` - Map of expressions to their inferred types
-   `Type` enum - Type representations
-   Validates:
    -   Pattern match feasibility
    -   Monad operation compatibility
    -   Result type handling

#### `types.rs`

**Responsibility**: Type system definitions

**Key Type Enum**:
Represents the Koatl type system for analysis and optimization

#### `util.rs`

**Responsibility**: Error handling, caching, and utilities

**Key Structures**:

-   `LineColCache` - Cache line/column positions for error reporting
    -   `linecol(byte_offset) -> (line, col)`
-   `RefHash` - Reference-based hashing for AST nodes
-   `TlErr` - Single error with span, message, and context
    -   `.kind` - TlErrKind (Tokenize, Parse, Transform, Emit, Unknown)
-   `TlErrs` - Collection of errors
-   `TlErrBuilder` - Fluent error construction

**Error Handling**:

-   Collects multiple errors before failing
-   Provides detailed error context with source location
-   Used throughout compilation pipeline

#### `transform.rs` (~2577 lines)

**Responsibility**: Transform Koatl AST to Python AST (core transpilation logic)

**Key Structures**:

-   `TlCtx<'src, 'ast>` - Transformation context:
    -   Source code and filename
    -   Name resolution state
    -   Type information
    -   Python identifier mappings
    -   Monad function tracking
-   `TransformOptions` - Transformation settings:

    -   `interactive` - Interactive/REPL mode
    -   `target_version` - Python version (3.7, 3.8, etc.)

-   `TransformOutput<'src>` - Transpilation result:
    -   `source` - Generated Python source code
    -   `source_line_map` - Map from Python line to original Koatl byte offset
    -   `module_star_exports` - Modules with `import *`

**Key Functions** (inferred from code):

-   `transform_ast()` - Main transformation entry
-   `transform_expr()` - Transform expressions
-   `transform_stmt()` - Transform statements
-   `transform_pattern()` - Transform patterns
-   Expression handlers for:
    -   Arrow functions (create Python lambda or nested function)
    -   Pattern matching (convert to Python match statement)
    -   Monads (`@` operator - convert to generator-based bind)
    -   Pipe operations (convert to function calls)
    -   Check expressions (wrap in try-except returning Result)
    -   Placeholder variables (extract and create lambdas)

**Transformation Strategy**:

-   Rewrite Koatl-specific constructs to Python equivalents
-   Generate helper functions for complex operations
-   Mangle variable names to avoid conflicts (using `let_` prefix)
-   Track memo dependencies for Memo monad
-   Handle monad semantics (Result auto-unwrapping, Async wrapping, etc.)

**Helper Functions**:

-   `decl_py_ident()` - Generate safe Python identifiers for Koatl variables
-   `fn_info()`, `pattern_info()` - Retrieve semantic analysis results
-   `create_aux_var()` - Create temporary variables
-   Operator mapping (Koatl to Python)

#### `ast_builder.rs`

**Responsibility**: Fluent AST construction helpers

**Key Methods**:

-   `AstBuilder::new(span)` - Create builder
-   `ident()`, `expr()`, `assign()`, `return_()`, `while_()`, `for_()` - Build statements
-   Convenience methods for common patterns

#### `main.rs` (~100 lines)

**Responsibility**: CLI tool for transpilation

**Commands**:

-   `trans <filename>` - Transpile to Python and print
-   `run <filename>` - Transpile and execute with Python

**Features**:

-   Error formatting with ariadne
-   File I/O
-   Process spawning for Python execution

#### `py/` directory - Python AST Generation

**`py/ast.rs`** - Python AST structures

-   `PyBlock<'src>` - Python module body
-   `PyStmt`, `PyExpr` - Python statement and expression types
-   Mirrors Python's AST module structure
-   `PyToken<'src>`, `PyLiteral<'src>` - Python primitives

**`py/ast_builder.rs`** - Python AST construction helpers

-   `PyAstBuilder` - Fluent interface for building Python AST
-   Convenience methods for common patterns

**`py/emit.rs`** - Code generation

-   Walks Python AST and generates Python source code
-   Preserves line information for error mapping

---

### **koatl** - Python Bindings (PyO3)

#### `src/lib.rs` (~230 lines)

**Responsibility**: PyO3 Python module interface

**Exported Functions**:

-   `transpile(src, mode, filename, target_version) -> PyObject`

    -   Main entry point for Python code
    -   Calls `transpile_to_py_ast()` in koatl-core
    -   Emits Python AST as PyObject
    -   Modes: "module", "script", "interactive", "no_prelude"

-   `transpile_raw(src, mode, filename, target_version) -> (source, linemap)`
    -   Returns Python source code as string
    -   Provides line number mapping to original Koatl source

**Virtual Method Tables** (Runtime feature):

-   `VTBL` - Type-based method lookup cache
-   `VTBL2` - Trait method lookup cache
-   `fast_vget()` - Fast virtual method resolution for extension attributes

**Caching**:

-   Uses `once_cell::sync::Lazy` for thread-safe initialization
-   Caches extension method implementations at runtime

#### `src/emit_py.rs` (~896 lines)

**Responsibility**: Emit Python AST objects to PyObjects for execution

**Key Structures**:

-   `PyTlErr` - Error representation for PyO3
-   `PyCtx<'py, 'src>` - Emission context:
    -   Python interpreter reference
    -   AST module access
    -   Source code and line caching

**Key Functions**:

-   `emit_py(py_block) -> PyObject`
    -   Walks Python AST
    -   Creates Python `ast` module objects
    -   Returns executable PyAST

**Emission Strategies**:

-   `ast_node()` - Create AST node with location info (lineno, col_offset)
-   Maps Python AST node types to `ast` module constructors
-   Preserves source location information for error reporting

---

### **ohtli** - Code Formatter

#### `src/main.rs` (~50 lines)

**Responsibility**: CLI formatter tool

**Commands**:

-   Format file in-place
-   `--check` mode to validate formatting without modification

**Features**:

-   Integrable with editor LSP
-   Used by quetzal VSCode extension

#### `src/formatter.rs`

**Responsibility**: Koatl code formatting implementation

**Features**:

-   Standardizes indentation
-   Consistent spacing
-   Proper alignment
-   Comment preservation (via trivia from lexer)

#### `src/config.rs`

**Responsibility**: Formatter configuration

---

### **quetzal** - VSCode Extension

#### `src/extension.ts`

**Responsibility**: VSCode integration

**Features**:

-   Syntax highlighting via TextMate grammar
-   Language definition
-   Formatter integration (calls ohtli)

#### `src/formatter.ts`

**Responsibility**: Formatter provider for VSCode

#### `syntaxes/tl.tmLanguage.json`

**Responsibility**: TextMate grammar for syntax highlighting

---

## Key Algorithms & Patterns

### Variable Name Mangling

Koatl's `let` scoping requires variable name mangling in Python (which has function scope, not block scope). Variables are suffixed with `_<count>` to create unique Python names while preserving Koatl semantics.

### Extension Attribute Resolution

Extension methods/properties are resolved at runtime using virtual method tables (VTBL) that cache lookups by type and name. The resolution order is:

1. Instance `__getattr__` if defined
2. Type's extension method table
3. Trait extension table
4. Raise AttributeError

---

## Error Handling Pipeline

1. **Lexer**: Tokenization errors collected
2. **Parser**: Syntax errors with recovery (multiple errors in one pass)
3. **Lift CST**: Structural conversion errors
4. **Resolve Scopes**: Name resolution errors
5. **Inference**: Type compatibility errors
6. **Transform**: Semantic validation errors
7. **Emit**: Code generation errors

All errors are collected and reported together with source location context via ariadne's formatted output.

---

## Testing Architecture

**Test types**:

-   `e2e/` - End-to-end tests (Koatl source → Python execution)
-   `fail-parse/` - Tests for parse failures
-   `parse/` - Tests for successful parsing
-   Integration tests verify full pipeline

Tests in `koatl/tests/` directory use pytest to validate transpilation and execution.

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
