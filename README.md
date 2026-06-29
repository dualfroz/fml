# fml

A small ML-family language implemented from scratch in F#.

## Overview

`fml` is a statically typed, functional mini-ML. It has full
Hindley-Milner type inference (Algorithm W) with let-polymorphism,
algebraic data types and records, exhaustive pattern matching, first-class
functions and a small module system. Programs run on either of two
back ends that share the same runtime values: a tree-walking interpreter
and a bytecode compiler paired with a stack virtual machine. The test suite
uses the two engines as an oracle for each other — every program must
evaluate to the same value on both.

## Features

- **Lexer** — a hand-written tokenizer with nested block comments, string
  escapes and type-variable syntax.
- **Parser** — a recursive-descent parser with precedence climbing for
  expressions, patterns, type declarations (variants and records), and
  `struct ... end` modules with `open`.
- **Type inference** — Hindley-Milner with unification over mutable type
  cells, the level-based generalization trick (Rémy / OCaml), the occurs
  check, and Algorithm W with full let-polymorphism.
- **Algebraic data types and records** — user-declared parametric variants
  and record types, checked and inferred throughout.
- **Pattern matching** — normalization to a uniform constructor form,
  Maranget's usefulness algorithm for exhaustiveness and redundancy
  (with a rendered counterexample), and a decision-tree compiler.
- **Two back ends** — a tree-walking interpreter and a stack-machine
  compiler with a de Bruijn environment, closures, tail calls and
  recursive knot-tying.
- **Standard library** — `Option`, `Result`, `List` and `Map` modules
  written in `fml` itself and loaded on demand.
- **Tooling** — a REPL and a file runner.

## Usage

```sh
# start the REPL
dotnet run --project src/Fml.Cli

# run a source file
dotnet run --project src/Fml.Cli examples/demo.fml
```

In the REPL, enter an expression or a definition; each result is printed as
`val <name> : <type> = <value>`. Press Ctrl-D to quit.

```
fml> let rec fact = fun n -> if n <= 1 then 1 else n * fact (n - 1) ;; fact 5
val fact : int -> int = <fun>
val it : int = 120
```

## Tests

```sh
dotnet test
```

The tests cover inference of polymorphic terms, type-error detection,
evaluation, exhaustiveness and redundancy checking, the standard library,
and an oracle that asserts the interpreter and the bytecode VM agree on a
range of programs (recursion, higher-order functions, ADTs, records and
modules).

## Project structure

```
src/Fml.Core   the language: lexer, parser, types, inference, backends
src/Fml.Cli    the REPL and file runner
tests          xUnit test suite
examples       sample fml programs
```

## Requirements

- .NET 6 SDK (F#)
