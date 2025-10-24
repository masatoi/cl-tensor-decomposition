# Repository Guidelines

## Project Structure & Module Organization
The ASDF system `cl-tensor-decomposition` loads its public API from `src/core.lisp`; keep exported entry points there and wire any new source files through `cl-tensor-decomposition.asd`. Supporting utilities live beside it (e.g., `src/sparse-tensor.lisp`, `src/workspace.lisp`). Tests sit in `tests/cl-tensor-decomposition.lisp` under the Rove harness. Usage walkthroughs and datasets belong in `examples/`, while documentation assets remain under `docs/images/`. Update both `.asd` definitions whenever you add modules, test files, or rename packages.

## Build, Test, and Development Commands
Use Roswell or SBCL with ASDF. Common flows:
- `ros run -l cl-tensor-decomposition.asd -e '(asdf:load-system :cl-tensor-decomposition)' -q` loads the library into a REPL.
- `ros run -l cl-tensor-decomposition-test.asd -e '(asdf:test-system :cl-tensor-decomposition)' -q` runs the Rove suite headlessly.
- `ros run -l examples/simple-example.lisp -q` executes the minimal factorization demo.
Keep incremental scripts under `examples/` and prefer Roswell shebangs for longer notebooks or reports.

## Coding Style & Naming Conventions
Follow idiomatic Common Lisp formatting: two-space indentation, `loop` clauses on new lines, and hyphenated lowercase names (`initialize-random-matrix`). Exported symbols belong in `src/core.lisp`’s package form and should retain explicit type declarations. Globals stay earmuffed (`*epsilon*`), double-float literals use the `d0` suffix, and verbose logging is gated by keyword arguments (`:verbose t`). Do not commit editor backups such as `*.lisp~` files.

## Testing Guidelines
Tests use Rove with `deftest` forms and `ok` assertions; mirror the naming pattern `tests/<system>.lisp` and define fixtures near the top of each file. New features should add targeted assertions plus coverage around `decomposition`. Run `asdf:test-system :cl-tensor-decomposition` before opening a PR, and document any stochastic behavior (set random seeds when introducing new randomized steps).

## Commit & Pull Request Guidelines
Recent history favors short, imperative commit subjects (“Add examples”, “Fix README”); keep body text only when extra context is required. Pull requests should summarize the change, list the Roswell commands you executed, note new tests, and attach logs or screenshots when enabling verbose cycles. Link related issues and call out follow-up tasks when altering algorithmic behavior or tensor formats.

## Rule to use system Python command
When Python commands are needed, use the system standard Python from the absolute path below.
/usr/bin/python3
