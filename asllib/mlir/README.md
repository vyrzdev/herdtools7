# TypedAST

This prototype represents selected guarantees established by ASLRef’s type checker directly in the OCaml types. It includes a TypedAST representation and normalisation from successfully typed `AST.t`.

## Scope

- **Expressions:** integer literals, addition, local references and `ARBITRARY`.
- **Types:** unconstrained integers, singleton constraints and ranges.
- **Statements:** pass, sequence, local `let` and `var` declarations, and return.
- **Subprograms:** functions and procedures with ASL bodies, without parameters or arguments.

Terms also support multiplication introduced by symbolic normalisation.

## Implementation

`typedAST/representation/AST.ml` defines the model. Expressions and terms are indexed by kind; source types and typing judgements have distinct annotation structures. Statement indices carry the enclosing subprogram’s return requirement.

Within `typedAST/normalisation/`:

- `Term.ml` handles terms and typing judgements.
- `Expression.ml` handles expressions and source types.
- `Statement.ml` and `Declaration.ml` handle statements and subprograms.
- `Environment.ml` records the kinds of local bindings.

`Pipeline.ml` connects declaration normalisation to the executable.

## Running

With ASLRef’s build dependencies installed in the active opam switch, run from `asllib/`:

```sh
opam exec -- dune build --root . mlir/aslmlir.exe
opam exec -- dune exec --root . mlir/aslmlir.exe -- /path/to/program.asl
```

The executable parses ASLv1, typechecks, and constructs the TypedAST. Multiple files are combined in command-line order before typing.

Success is silent and exits with status zero. Failures produce a diagnostic and a nonzero exit status.

## Diagnostics

- **`Unimplemented`** — a form outside the supported subset.
- **`Violation #n`** — a failed assumption of the normaliser, numbered for tracking.
- **`PrecisionLoss` / `ArbitraryInAnnotation`** — deferred handling of typing annotations with lost precision or containing `ARBITRARY`.
