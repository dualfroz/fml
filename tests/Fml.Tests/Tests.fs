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

// --- Oracle: interpreter and bytecode VM must agree, bit for bit ---

let private bothBackends (src: string) : string =
    let fromInterp = prettyValue (evalInterp src)
    let fromVm = prettyValue (evalVm src)
    Assert.Equal(fromInterp, fromVm)
    fromInterp

let private bothWithPrelude (src: string) : string =
    let fromInterp = prettyValue (evalOn Interp true src)
    let fromVm = prettyValue (evalOn Vm true src)
    Assert.Equal(fromInterp, fromVm)
    fromInterp

[<Fact>]
let ``oracle fib matches on both engines`` () =
    Assert.Equal("610", bothBackends "let rec fib = fun n -> if n < 2 then n else fib (n - 1) + fib (n - 2) ;; fib 15")

[<Fact>]
let ``oracle ackermann matches on both engines`` () =
    let src =
        "let rec ack = fun m n -> if m = 0 then n + 1 else if n = 0 then ack (m - 1) 1 else ack (m - 1) (ack m (n - 1)) ;; ack 3 3"
    Assert.Equal("61", bothBackends src)

[<Fact>]
let ``oracle quicksort matches on both engines`` () =
    let src =
        "let rec qsort = fun xs -> match xs with [] -> [] | p :: rest -> let smaller = List.filter (fun y -> y < p) rest in let larger = List.filter (fun y -> y >= p) rest in List.append (qsort smaller) (p :: qsort larger) ;; qsort [3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5]"
    Assert.Equal("[1; 1; 2; 3; 3; 4; 5; 5; 5; 6; 9]", bothWithPrelude src)

[<Fact>]
let ``oracle tree adt match matches on both engines`` () =
    let src =
        "type tree = Leaf | Node of tree * int * tree ;; let rec sum = fun t -> match t with Leaf -> 0 | Node (l, x, r) -> sum l + x + sum r ;; sum (Node (Node (Leaf, 1, Node (Leaf, 2, Leaf)), 3, Node (Leaf, 4, Leaf)))"
    Assert.Equal("10", bothBackends src)

[<Fact>]
let ``oracle records match on both engines`` () =
    let src =
        "type point = { x : int; y : int } ;; let move = fun p d -> { x = p.x + d; y = p.y + d } in let q = move { x = 1; y = 2 } 10 in q.x + q.y"
    Assert.Equal("23", bothBackends src)

[<Fact>]
let ``oracle higher order functions match on both engines`` () =
    let src =
        "let compose = fun f g x -> f (g x) in let rec map = fun f xs -> match xs with [] -> [] | h :: t -> f h :: map f t in map (compose (fun x -> x + 1) (fun x -> x * x)) [1; 2; 3; 4]"
    Assert.Equal("[2; 5; 10; 17]", bothBackends src)

[<Fact>]
let ``oracle deep tail recursion agrees`` () =
    Assert.Equal("500500", bothBackends "let rec go = fun acc n -> if n = 0 then acc else go (acc + n) (n - 1) ;; go 0 1000")

[<Fact>]
let ``oracle mutually recursive parity agrees`` () =
    let src =
        "let rec isEven = fun n -> if n = 0 then true else isOdd (n - 1) and isOdd = fun n -> if n = 0 then false else isEven (n - 1) in (isEven 10, isOdd 10)"
    Assert.Equal("(true, false)", bothBackends src)

[<Fact>]
let ``oracle nested constructor patterns agree`` () =
    let src =
        "type 'a option = None | Some of 'a ;; let f = fun o -> match o with None -> 0 | Some None -> 1 | Some (Some x) -> x in (f None, f (Some None), f (Some (Some 7)))"
    Assert.Equal("(0, 1, 7)", bothBackends src)

[<Fact>]
let ``oracle string literal matching agrees`` () =
    let src = "let classify = fun s -> match s with \"yes\" -> 1 | \"no\" -> 0 | _ -> -1 in (classify \"yes\", classify \"no\", classify \"maybe\")"
    Assert.Equal("(1, 0, -1)", bothBackends src)

[<Fact>]
let ``oracle record with unsorted fields projects correctly`` () =
    let src = "type pt = { y : int; x : int } ;; match { y = 1; x = 2 } with { x = a; y = b } -> (a, b)"
    Assert.Equal("(2, 1)", bothBackends src)

[<Fact>]
let ``oracle let and parallel binding agrees`` () =
    Assert.Equal("(1, 2)", bothBackends "let a = 1 and b = 2 in (a, b)")

// --- Algebraic data types: inference ---

[<Fact>]
let ``option constructor infers option type`` () =
    Assert.Equal("int option", typeOf "type 'a option = None | Some of 'a ;; Some 3")

[<Fact>]
let ``tree constructor infers tree type`` () =
    let src = "type tree = Leaf | Node of tree * int * tree ;; Node (Leaf, 5, Leaf)"
    Assert.Equal("tree", typeOf src)

[<Fact>]
let ``result constructor infers polymorphic result`` () =
    Assert.Equal("(int, 'a) result", typeOf "type ('a, 'b) result = Ok of 'a | Error of 'b ;; Ok 1")

[<Fact>]
let ``constructor arity mismatch is a type error`` () =
    match tryTypeOf "type 'a option = None | Some of 'a ;; Some" with
    | Error _ -> Assert.True(true)
    | Ok t -> Assert.True(false, sprintf "expected an arity error, inferred %s" t)

[<Fact>]
let ``record field access infers the field type`` () =
    Assert.Equal("string", typeOf "type person = { name : string; age : int } ;; let p = { name = \"a\"; age = 1 } in p.name")

[<Fact>]
let ``record with a missing field is rejected`` () =
    match tryTypeOf "type pt = { x : int; y : int } ;; { x = 1 }" with
    | Error _ -> Assert.True(true)
    | Ok t -> Assert.True(false, sprintf "expected a missing-field error, inferred %s" t)

// --- Exhaustiveness and reachability ---

[<Fact>]
let ``non-exhaustive option match is rejected`` () =
    match tryTypeOf "type 'a option = None | Some of 'a ;; fun o -> match o with Some x -> x" with
    | Error msg -> Assert.Contains("exhaustive", msg)
    | Ok _ -> Assert.True(false, "expected the missing None case to be reported")

[<Fact>]
let ``non-exhaustive list match is rejected`` () =
    match tryTypeOf "fun xs -> match xs with h :: t -> h" with
    | Error msg -> Assert.Contains("exhaustive", msg)
    | Ok _ -> Assert.True(false, "expected the missing empty-list case to be reported")

[<Fact>]
let ``non-exhaustive bool match is rejected`` () =
    match tryTypeOf "fun b -> match b with true -> 1" with
    | Error msg -> Assert.Contains("exhaustive", msg)
    | Ok _ -> Assert.True(false, "expected the missing false case to be reported")

[<Fact>]
let ``non-exhaustive constructor match is rejected`` () =
    match tryTypeOf "type color = Red | Green | Blue ;; fun c -> match c with Red -> 1 | Green -> 2" with
    | Error msg -> Assert.Contains("exhaustive", msg)
    | Ok _ -> Assert.True(false, "expected the missing Blue case to be reported")

[<Fact>]
let ``a complete match is accepted`` () =
    Assert.Equal("color -> int", typeOf "type color = Red | Green | Blue ;; fun c -> match c with Red -> 1 | Green -> 2 | Blue -> 3")

[<Fact>]
let ``a redundant wildcard-first clause is rejected`` () =
    match tryTypeOf "fun n -> match n with x -> 0 | 5 -> 1" with
    | Error msg -> Assert.Contains("unreachable", msg)
    | Ok _ -> Assert.True(false, "expected the shadowed 5 clause to be reported")

[<Fact>]
let ``a duplicated constructor clause is rejected`` () =
    match tryTypeOf "type color = Red | Green | Blue ;; fun c -> match c with Red -> 1 | Green -> 2 | Blue -> 3 | Red -> 4" with
    | Error msg -> Assert.Contains("unreachable", msg)
    | Ok _ -> Assert.True(false, "expected the duplicate Red clause to be reported")

// --- Standard library implemented in fml ---

[<Fact>]
let ``prelude List map and length agree`` () =
    Assert.Equal("[2; 4; 6]", bothWithPrelude "List.map (fun x -> x * 2) [1; 2; 3]")
    Assert.Equal("4", bothWithPrelude "List.length [9; 8; 7; 6]")

[<Fact>]
let ``prelude List foldl sums a range`` () =
    Assert.Equal("5050", bothWithPrelude "List.foldl (fun a x -> a + x) 0 (List.range 1 101)")

[<Fact>]
let ``prelude List rev and append agree`` () =
    Assert.Equal("[3; 2; 1; 4; 5]", bothWithPrelude "List.append (List.rev [1; 2; 3]) [4; 5]")

[<Fact>]
let ``prelude Option combinators agree`` () =
    Assert.Equal("Some 42", bothWithPrelude "Option.map (fun x -> x * 2) (Some 21)")
    Assert.Equal("99", bothWithPrelude "Option.getOr 99 None")

[<Fact>]
let ``prelude Result combinators agree`` () =
    Assert.Equal("Ok 10", bothWithPrelude "Result.map (fun x -> x + 1) (Ok 9)")
    Assert.Equal("true", bothWithPrelude "Result.isOk (Ok 1)")

[<Fact>]
let ``prelude Map add find and size agree`` () =
    let src = "let m = Map.add 1 100 (Map.add 2 200 (Map.add 1 111 Map.empty)) in (Map.find 1 m, Map.find 2 m, Map.size m)"
    Assert.Equal("(Some 100, Some 200, 2)", bothWithPrelude src)

[<Fact>]
let ``opening a module brings members into scope`` () =
    Assert.Equal("[1; 4; 9]", bothWithPrelude "open List ;; map (fun x -> x * x) [1; 2; 3]")

[<Fact>]
let ``qualified access reaches across modules`` () =
    Assert.Equal("3", bothWithPrelude "Map.size (Map.add 1 1 (Map.add 2 2 (Map.add 3 3 Map.empty)))")

// --- Records and constructors at runtime ---

[<Fact>]
let ``record field update produces a fresh record`` () =
    let src = "type pt = { x : int; y : int } ;; let p = { x = 1; y = 2 } in let q = { x = p.x + 5; y = p.y } in (p.x, q.x)"
    Assert.Equal("(1, 6)", bothBackends src)

[<Fact>]
let ``constructor carrying a tuple destructures in a match`` () =
    let src = "type box = Box of int * bool ;; match Box (7, true) with Box (n, flag) -> if flag then n else 0"
    Assert.Equal("7", bothBackends src)

[<Fact>]
let ``pretty printing of a nested constructor value`` () =
    let src = "type 'a option = None | Some of 'a ;; Some (Some 5)"
    Assert.Equal("Some (Some 5)", bothBackends src)
