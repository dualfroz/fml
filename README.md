# fml

A small ML-family language implemented from scratch in F#.

## Overview

`fml` is a statically typed, functional mini-ML. It has Hindley-Milner type
inference with let-polymorphism, algebraic data types and records, pattern
matching with exhaustiveness and redundancy checking, first-class functions
and a small module system. Programs run on either of two back ends that share
the same runtime values: a tree-walking interpreter and a bytecode compiler
paired with a stack virtual machine. The test suite uses the two engines as
an oracle for each other: the oracle tests and every program in `examples/`
must produce the same output on both.

## Features

- **Lexer**: a hand-written tokenizer with nested `(* ... *)` comments,
  string escapes and type-variable syntax.
- **Parser**: a recursive-descent parser with one function per precedence
  level, covering expressions, patterns, type declarations (variants and
  records) and `module ... = struct ... end` modules with `open`.
  Multi-parameter functions are desugared into nested one-parameter lambdas.
- **Type inference**: Hindley-Milner with destructive unification over
  mutable type variables, the occurs check, and level-based generalization
  (the technique used by OCaml), giving full let-polymorphism for top-level
  and local definitions, including mutually recursive `let rec ... and ...`
  groups.
- **Algebraic data types and records**: user-declared parametric variants
  (`type ('a, 'b) result = Ok of 'a | Error of 'b`) and record types
  (`type point = { x : int; y : int }`), checked and inferred throughout,
  with field access `p.x` and record patterns.
- **Pattern matching**: patterns are normalized to a uniform constructor form
  and analyzed with Maranget's usefulness algorithm. A clause that can never
  match is rejected as unreachable; a match that misses a case is rejected
  with a rendered counterexample, for example
  `non-exhaustive match: no clause covers ([], _ :: _)`.
- **Two back ends**: a tree-walking interpreter, and a compiler to a stack
  machine with a de Bruijn environment, closures, tail calls, recursive
  knot-tying for `let rec` groups and matches lowered to decision trees.
  VM call frames live on the heap, so deep non-tail recursion does not
  exhaust the .NET stack.
- **Standard library**: built-in primitives plus `Option`, `Result`, `List`
  and `Map` modules written in `fml` itself (see below).
- **Tooling**: a REPL and a file runner.

## Build and run

Requires the .NET 6 SDK.

```sh
dotnet build

# start the REPL
dotnet run --project src/Fml.Cli

# run a source file
dotnet run --project src/Fml.Cli examples/demo.fml

# run a source file on the bytecode VM
dotnet run --project src/Fml.Cli -- --vm examples/demo.fml
```

The file runner prints one line per definition or expression,
`val <name> : <type> = <value>`, where an expression is bound to `it`. On the
first lex, parse, type, match or runtime error it prints the error to stderr
and exits with code 1.

In the REPL, enter one line at a time; a line may hold several phrases
separated by `;;`. Errors are reported and the session continues. Press
Ctrl-D to quit.

```
fml> let rec fact = fun n -> if n <= 1 then 1 else n * fact (n - 1) ;; fact 5
val fact : int -> int = <fun>
val it : int = 120
fml> fun x -> x x
Type error: occurs check failed: infinite type
```

Both the REPL and the file runner load the standard library first and use the
tree-walking interpreter by default. With `--vm` they run every phrase on the
bytecode VM instead: `fml --vm` starts a VM-backed REPL and
`fml --vm file.fml` runs a file on the VM. The output is the same on both
back ends.

## Examples

Every file in `examples/` has a matching `.out` file with the exact output of
`dotnet run --project src/Fml.Cli examples/<name>.fml`. A test runs each
example on both back ends and compares the result with that file.

| File | What it shows |
|---|---|
| `demo.fml` | recursion, `map`, `compose`, let-polymorphism |
| `inference.fml` | inferred types of polymorphic combinators, Church numerals |
| `datatypes.fml` | variants, a polymorphic binary search tree, records |
| `exhaustiveness.fml` | accepted matches, then a rejected non-exhaustive one |
| `interpreter.fml` | an interpreter and simplifier for a small lambda language |
| `lists.fml` | a list library built on the prelude, sorting, primes |

`inference.fml` prints the most general types, for example:

```
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c = <fun>
val zip : 'a list -> 'b list -> ('a * 'b) list = <fun>
val succ : (('a -> 'b) -> 'c -> 'a) -> ('a -> 'b) -> 'c -> 'b = <fun>
val it : int * string * bool = (7, "seven", true)
```

`exhaustiveness.fml` ends with a function that forgets one case, so the run
stops there with exit code 1:

```
let rec zipExact = fun xs ys ->
  match (xs, ys) with
  | ([], []) -> []
  | (x :: xt, y :: yt) -> (x, y) :: zipExact xt yt
  | (_ :: _, []) -> [] ;;
```

```
Match error: non-exhaustive match: no clause covers ([], _ :: _)
```

`interpreter.fml` evaluates terms of an untyped lambda language to
`(value, string) result`, and computes a factorial through a fixed-point
combinator written in that language:

```
val eval : (string * value) list -> term -> (value, string) result = <fun>
val it : string = "3628800"
val it : string = "error: unbound variable w"
```

## Language

### Lexical structure

- Comments are `(* ... *)` and nest.
- Integer literals are decimal, 32-bit. Negative numbers use unary minus.
  A literal above 2147483647 is a lex error that reports its line and column.
- String literals use double quotes with the escapes `\n`, `\t`, `\r`, `\\`
  and `\"`.
- Identifiers start with a letter or `_` and continue with letters, digits,
  `_` or `'`. An identifier starting with an uppercase letter is a
  constructor or a module name.
- Type variables are written `'a`, `'b`, ...
- Keywords: `let rec in fun if then else match with type of and module
  struct end open mod true false`.

### Grammar

Informal EBNF; `{ x }` is repetition, `[ x ]` is optional. `lid` is a
lowercase identifier, `Uid` an uppercase one.

```
program   = [ phrase { ";;" phrase } ] [ ";;" ]
phrase    = "let" [ "rec" ] binding { "and" binding }            definition
          | "type" typedecl { "and" typedecl }
          | "module" Uid "=" "struct" { phrase [ ";;" ] } "end"
          | "open" Uid
          | expr                                                  bound to it

binding   = lid { lid | "_" } "=" expr

expr      = "let" [ "rec" ] binding { "and" binding } "in" expr
          | "fun" ( lid | "_" ) { lid | "_" } "->" expr
          | "if" expr "then" expr "else" expr
          | "match" expr "with" [ "|" ] case { "|" case }
          | binary
case      = pattern "->" expr

binary    = operators below, from lowest to highest precedence
unary     = "-" unary | app
app       = Uid [ postfix ]                    constructor application
          | postfix { postfix }                function application
postfix   = atom { "." lid }                   field access or Module.member
atom      = int | string | "true" | "false" | lid | Uid
          | "()" | "(" expr ")" | "(" expr "," expr { "," expr } ")"
          | "[" [ expr { ";" expr } ] "]"
          | "{" lid "=" expr { ";" lid "=" expr } [ ";" ] "}"

pattern   = patapp [ "::" pattern ]
patapp    = Uid [ patatom ] | patatom
patatom   = int | string | "true" | "false" | "_" | lid | Uid
          | "()" | "(" pattern ")" | "(" pattern "," pattern { "," pattern } ")"
          | "[" [ pattern { ";" pattern } ] "]"
          | "{" lid "=" pattern { ";" lid "=" pattern } [ ";" ] "}"

typedecl  = [ tparams ] lid "=" ( variants | recorddef )
tparams   = tyvar | "(" tyvar { "," tyvar } ")"
variants  = [ "|" ] Uid [ "of" type ] { "|" Uid [ "of" type ] }
recorddef = "{" lid ":" type { ";" lid ":" type } [ ";" ] "}"

type      = tuplety [ "->" type ]
tuplety   = appty { "*" appty }
appty     = atomty { lid }                     postfix: int list, 'a tree
atomty    = tyvar | lid | "(" type ")" | "(" type { "," type } ")" lid
```

Binary operators, from lowest to highest precedence:

| Operators | Associativity |
|---|---|
| `\|\|` | left, short-circuit |
| `&&` | left, short-circuit |
| `=` `<>` `<` `<=` `>` `>=` | left |
| `::` | right |
| `+` `-` `^` | left |
| `*` `/` `mod` | left |
| unary `-` | prefix |
| application | left |
| `.` | postfix |

`let`, `fun`, `if` and `match` extend as far to the right as possible and
start an expression only at the beginning of `expr`; as an operand or an
argument they need parentheses. A `match` inside a match arm that is not the
last arm also needs parentheses, otherwise it takes the following arms.

A constructor declared with `of t1 * t2` takes two arguments, written as a
tuple: `Node (l, x, r)`. A constructor with a single field takes a
parenthesized tuple as that one field, so `Some (1, 2)` is an
`(int * int) option`, in expressions and in patterns. Several definitions or type declarations joined by
`and` form one group; with `rec` the definitions may refer to each other.

### Typing

- Arithmetic (`+ - * / mod`) and ordering (`< <= > >=`) work on `int`.
- `=` and `<>` are structural and polymorphic; comparing functions is a
  runtime error.
- `^` concatenates strings, `&&` and `||` take `bool`.
- The type of a record expression or record pattern is decided by the first
  field named in it, and a record expression must set exactly the declared
  fields.

### Standard library

Built-in functions: `head`, `tail`, `isEmpty`, `fst`, `snd`, `not`, `print`
(prints any value), `stringLength`, `intToString`, `boolToString`.

The prelude, written in `fml`, declares `type 'a option = None | Some of 'a`
and `type ('a, 'b) result = Ok of 'a | Error of 'b`, and the modules:

- `Option`: `isSome`, `getOr`, `map`, `bind`
- `Result`: `isOk`, `map`, `mapError`, `getOr`
- `List`: `length`, `map`, `filter`, `foldl`, `foldr`, `append`, `rev`,
  `mem`, `range`, `sum`
- `Map`: an association list with `empty`, `add`, `find`, `contains`,
  `size`, `remove`

Members are reached as `List.map` or brought into scope with `open List`.

## Library API

`Fml.Core.Interpreter` exposes `Session(Interp)` and `Session(Vm)`, a
persistent top-level environment with `Feed`, `RunPhrase` and `LoadPrelude`.
Each phrase returns its name, pretty-printed type and value. Helpers such as
`typeOf`, `tryTypeOf`, `evalOn` and `tryEval` run a whole program in one
call.

## Tests

```sh
dotnet test
```

The xUnit suite covers inference of polymorphic terms, type-error detection,
evaluation, exhaustiveness and redundancy checking, the standard library,
an oracle that asserts the interpreter and the bytecode VM agree on a range
of programs (recursion, higher-order functions, ADTs, records, modules), and
one test per example program that checks both back ends against the
documented `.out` file.

## Project layout

```
src/Fml.Core/
  Ast.fs           syntax tree
  Lexer.fs         tokenizer
  Parser.fs        recursive-descent parser
  Types.fs         types, unification, generalization, type printing
  Decls.fs         registry of declared types, constructors and fields
  Match.fs         exhaustiveness and redundancy analysis, decision trees
  Infer.fs         type inference
  Value.fs         runtime values, equality, value printing
  Eval.fs          tree-walking interpreter
  Prelude.fs       built-in primitives and the fml standard library
  Bytecode.fs      instruction set and compiler
  Vm.fs            stack virtual machine
  Interpreter.fs   sessions, modules and the top-level API
src/Fml.Cli/       REPL and file runner
tests/Fml.Tests/   xUnit test suite
examples/          sample programs with their expected output
```

## Limitations

- There are no type annotations in expressions; type syntax appears only in
  `type` declarations.
- Function parameters are names or `_`; destructuring goes through `match`.
- Patterns have no guards, or-patterns, `as` bindings or negative literals.
- Constructors are not first-class: they must be applied to all their
  arguments where they appear.
- Constructor and record field names are global. A later declaration that
  reuses a name replaces the earlier one, also across modules.
- Modules cannot be nested and have no signatures; types declared inside a
  module are global. `open` copies the members that exist at that point.
- Ordering operators work only on integers. Integers are 32-bit and wrap on
  overflow.
- There is no mutable state, no exceptions and no I/O besides `print`.
- A type name used in a declaration is not checked to exist; an unknown name
  becomes an opaque type.
- The tree-walking interpreter uses the .NET stack, so deep non-tail
  recursion (around a million calls) overflows it. The VM does not have this
  limit.
- The REPL reads one line per input; multi-line definitions need a file.

## License

MIT, see [LICENSE](LICENSE).
