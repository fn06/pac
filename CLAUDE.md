# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

### Building
- `dune build` - Build the entire project
- `dune build bin/main.exe` - Build just the executable
- `dune build lib/` - Build just the library

### Testing
- `dune runtest` - Run all tests
- `dune test test/test_pac.ml` - Run a specific test file

### Running
- `dune exec bin/main.exe -- <args>` - Run the pac CLI tool
- `dune exec pac -- <args>` - Alternative way to run (using public name)

### Development
- `dune clean` - Clean build artifacts
- `dune build --watch` - Build continuously on file changes

### CLI Usage Examples
- `dune exec pac -- parse -f examples/example.pac` - Parse a dependency file
- `dune exec pac -- check -f examples/example.pac -q 'A 1' -r 'A 1,B 1,C 1,D 2' -c core` - Check resolution validity
- `dune exec pac -- check -f examples/example.pac -q 'A 1' -r 'A 1,B 1,C 1,D 2' -c pubgrub` - Validate with PubGrub
- `dune exec pac -- solve -f examples/example.pac -q 'A 1'` - Automatically solve using PubGrub
- `dune exec pac -- reduce -f examples/concurrent.pac -g major --from concurrent --to core` - Reduce dependencies between calculi

## Architecture Overview

**Pac** is an OCaml implementation of the Package Calculus, a formal model for package dependency resolution with support for both core and concurrent resolution semantics.

### Core Components

#### Library (`lib/`)
- **`ast.ml`** - Defines the abstract syntax tree for dependency expressions, packages, and versions
- **`lexer.mll`** - OCamllex lexer for parsing dependency files (.pac format)  
- **`parser.mly`** - Menhir parser grammar for dependency syntax
- **`pac.ml`** - Main library module containing:
  - Core resolution logic (`Resolution` module)
  - Concurrent resolution with granularity functions (`Concurrent` and `ConcurrentResolution` modules)
  - AST conversion utilities
- **`pubgrub.ml`** - Complete PubGrub algorithm implementation:
  - Advanced dependency resolution with conflict-driven learning
  - Automatic solution finding (no pre-computed resolution needed)
  - Superior error reporting with incompatibility tracking

#### Binary (`bin/`)
- **`main.ml`** - CLI application using Cmdliner with four main commands:
  - `parse` - Parse and display dependency files
  - `check` - Validate resolutions against dependency constraints (supports core, concurrent, pubgrub)
  - `solve` - Automatically find solutions using PubGrub algorithm
  - `reduce` - Transform dependencies between calculi (concurrent → core)

### Key Concepts

**Dependency File Format (.pac)**
```
A 1.0.0 ( B ( 1.0.0 ) C ( 1.0.0 ) )
B 1.0.0 ( D ( 1.0.0 2.0.0 ) )
```
Each line represents: `package version ( target1 ( allowed_versions... ) target2 ( versions... ) )`

**Resolution Types**
- **Core Resolution**: Standard dependency resolution with version uniqueness constraints
- **Concurrent Resolution**: Enhanced resolution allowing multiple versions of the same package with granularity functions (e.g., major version compatibility)
- **PubGrub Resolution**: Advanced algorithm with conflict-driven learning, backtracking, and automatic solution finding

**Granularity Functions**: Currently supports "major" granularity (extracts major version from semver strings)

### Project Structure
- Uses Dune build system with Menhir parser generator
- Dependencies: `ocaml`, `menhir`, `cmdliner` for CLI
- Examples in `examples/` directory demonstrate both simple and concurrent dependency scenarios