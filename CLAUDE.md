# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Agent Guidelines

@agents/repl-driven-development.md
@agents/common-lisp-expert.md

## Project Overview

cl-tensor-decomposition is a Common Lisp library for non-negative tensor decomposition on sparse tensors. It uses Multiplicative Update (MU) algorithm for minimization of KL divergence.

## Build & Development Commands

```bash
# Load the library into a REPL
ros run -l cl-tensor-decomposition.asd -e '(asdf:load-system :cl-tensor-decomposition)' -q

# Run tests (Rove harness)
ros run -l cl-tensor-decomposition-test.asd -e '(asdf:test-system :cl-tensor-decomposition)' -q

# Run simple example
ros run -l examples/simple-example.lisp -q

# From within a REPL
(asdf:load-system :cl-tensor-decomposition)
(asdf:test-system :cl-tensor-decomposition)
```

## Architecture

### Core Modules (src/)

- **core.lisp**: Main decomposition algorithm and public API. Exports the `cltd` package with key functions:
  - `decomposition`: Main entry point for tensor decomposition
  - `sparse-kl-divergence`: KL divergence computation
  - `sdot`: Reconstruct sparse observations from factor matrices
  - `ranking`: Sort labels by factor weights

- **reporting.lisp**: Factor card generation and report artifacts for interpreting decomposition results. Generates structured alists for JSON export and markdown reports.

- **model-selection.lisp**: Cross-validation utilities for rank selection (`select-rank`, `cross-validate-rank`, `make-fold-splits`).

### ASDF System Structure

- `cl-tensor-decomposition.asd`: Main system with no external dependencies
- `cl-tensor-decomposition-test.asd`: Test system depending on Rove

### Data Representation

Sparse tensors are represented as:
- `X-shape`: List of tensor dimensions per mode
- `X-indices-matrix`: Fixnum matrix where each row is the coordinate of a non-zero element
- `X-value-vector`: Double-float vector of observed counts aligned with indices

## Coding Conventions

- Two-space indentation, `loop` clauses on new lines
- Hyphenated lowercase names (`initialize-random-matrix`)
- Globals use earmuffs (`*epsilon*`)
- Double-float literals use `d0` suffix (`1.0d0`)
- Exported symbols in `src/core.lisp` package form with explicit type declarations
- Verbose logging gated by `:verbose t` keyword arguments

## Testing

Tests use Rove with `deftest` forms and `ok` assertions. Test file: `tests/cl-tensor-decomposition.lisp`. Set random seeds when adding randomized tests. Run tests before PRs.

## Python Usage

When Python commands are needed, use the system Python: `/usr/bin/python3`
