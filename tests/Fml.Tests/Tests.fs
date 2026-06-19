module Fml.Tests.Tests

open Xunit
open Fml.Core.Value
open Fml.Core.Interpreter

// Keep the shared mutable inference state single-threaded across facts.
[<assembly: CollectionBehavior(DisableTestParallelization = true)>]
do ()

// --- Type inference of well-known polymorphic terms ---

[<Fact>]
let ``identity infers 'a -> 'a`` () =
    Assert.Equal("'a -> 'a", typeOf "fun x -> x")

[<Fact>]
let ``const infers 'a -> 'b -> 'a`` () =
    Assert.Equal("'a -> 'b -> 'a", typeOf "fun x -> fun y -> x")

[<Fact>]
let ``compose infers the expected arrow type`` () =
    Assert.Equal("('a -> 'b) -> ('c -> 'a) -> 'c -> 'b", typeOf "fun f -> fun g -> fun x -> f (g x)")

[<Fact>]
let ``map infers (('a -> 'b) -> 'a list -> 'b list)`` () =
    let src = "let rec map = fun f -> fun xs -> match xs with [] -> [] | h :: t -> f h :: map f t"
    Assert.Equal("('a -> 'b) -> 'a list -> 'b list", typeOf src)

[<Fact>]
let ``empty list infers 'a list`` () =
    Assert.Equal("'a list", typeOf "[]")

[<Fact>]
let ``cons of ints infers int list`` () =
    Assert.Equal("int list", typeOf "1 :: 2 :: []")

[<Fact>]
let ``tuple infers a product type`` () =
    Assert.Equal("int * bool * string", typeOf "(1, true, \"hi\")")

[<Fact>]
let ``print infers 'a -> unit`` () =
    Assert.Equal("'a -> unit", typeOf "print")

[<Fact>]
let ``let polymorphism reuses id at two types`` () =
    Assert.Equal("int * bool", typeOf "let id = fun x -> x in (id 1, id true)")

// --- Type error detection ---

[<Fact>]
let ``adding an int and a bool is a type error`` () =
    match tryTypeOf "1 + true" with
    | Error msg -> Assert.Contains("unify", msg)
    | Ok t -> Assert.True(false, sprintf "expected a type error, inferred %s" t)

[<Fact>]
let ``if branches must agree`` () =
    match tryTypeOf "if true then 1 else false" with
    | Error msg -> Assert.Contains("unify", msg)
    | Ok t -> Assert.True(false, sprintf "expected a type error, inferred %s" t)

[<Fact>]
let ``applying a non-function is a type error`` () =
    match tryTypeOf "1 2" with
    | Error _ -> Assert.True(true)
    | Ok t -> Assert.True(false, sprintf "expected a type error, inferred %s" t)

[<Fact>]
let ``self application triggers the occurs check`` () =
    match tryTypeOf "fun x -> x x" with
    | Error msg -> Assert.Contains("occurs", msg)
    | Ok t -> Assert.True(false, sprintf "expected an occurs-check error, inferred %s" t)

// --- Evaluation ---

[<Fact>]
let ``factorial via let rec`` () =
    let src = "let rec fact = fun n -> if n <= 1 then 1 else n * fact (n - 1) ;; fact 5"
    Assert.Equal("120", evalToString src)

[<Fact>]
let ``fibonacci via let rec`` () =
    let src = "let rec fib = fun n -> if n < 2 then n else fib (n - 1) + fib (n - 2) ;; fib 10"
    Assert.Equal("55", evalToString src)

[<Fact>]
let ``list length via recursion`` () =
    let src = "let rec length = fun xs -> match xs with [] -> 0 | h :: t -> 1 + length t ;; length [10; 20; 30; 40]"
    Assert.Equal("4", evalToString src)

[<Fact>]
let ``map squares a list`` () =
    let src =
        "let rec map = fun f -> fun xs -> match xs with [] -> [] | h :: t -> f h :: map f t ;; map (fun x -> x * x) [1; 2; 3; 4]"
    Assert.Equal("[1; 4; 9; 16]", evalToString src)

[<Fact>]
let ``tuple pattern binds both components`` () =
    Assert.Equal("3", evalToString "match (1, 2) with (a, b) -> a + b")

[<Fact>]
let ``let polymorphism evaluates to a mixed tuple`` () =
    Assert.Equal("(1, true)", evalToString "let id = fun x -> x in (id 1, id true)")

[<Fact>]
let ``builtins head and tail work`` () =
    Assert.Equal("[2; 3]", evalToString "tail [1; 2; 3]")
    Assert.Equal("1", evalToString "head [1; 2; 3]")

[<Fact>]
let ``boolean short circuit avoids evaluating the tail`` () =
    Assert.Equal("false", evalToString "false && (1 / 0 = 0)")

[<Fact>]
let ``polymorphic equality on lists`` () =
    Assert.Equal("true", evalToString "[1; 2; 3] = [1; 2; 3]")
    Assert.Equal("false", evalToString "[1; 2] = [1; 2; 3]")
