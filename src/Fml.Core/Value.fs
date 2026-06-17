module Fml.Core.Value

open Fml.Core.Ast

// Runtime values shared by the tree-walking interpreter and the bytecode VM.
// Interpreter closures carry a mutable environment cell so let rec can tie the
// recursive knot. VM closures carry a code-object id plus a captured de Bruijn
// environment cell for the same reason.

type Value =
    | VInt of int
    | VBool of bool
    | VString of string
    | VUnit
    | VTuple of Value list
    | VList of Value list
    | VConstruct of string * Value list
    | VRecord of Map<string, Value>
    | VClosure of string * Expr * Env ref
    | VCode of int * Value list ref
    | VBuiltin of string * (Value -> Value)

and Env = Map<string, Value>

exception RuntimeError of string

let rec valueEquals (a: Value) (b: Value) : bool =
    match a, b with
    | VInt x, VInt y -> x = y
    | VBool x, VBool y -> x = y
    | VString x, VString y -> x = y
    | VUnit, VUnit -> true
    | VTuple xs, VTuple ys -> List.length xs = List.length ys && List.forall2 valueEquals xs ys
    | VList xs, VList ys -> List.length xs = List.length ys && List.forall2 valueEquals xs ys
    | VConstruct(t1, xs), VConstruct(t2, ys) ->
        t1 = t2 && List.length xs = List.length ys && List.forall2 valueEquals xs ys
    | VRecord m1, VRecord m2 ->
        Map.count m1 = Map.count m2
        && m1 |> Map.forall (fun k v -> match Map.tryFind k m2 with
                                        | Some w -> valueEquals v w
                                        | None -> false)
    | (VClosure _ | VBuiltin _ | VCode _), _
    | _, (VClosure _ | VBuiltin _ | VCode _) -> raise (RuntimeError "cannot compare functions for equality")
    | _ -> false

let private escapeString (s: string) =
    let sb = System.Text.StringBuilder()
    for ch in s do
        match ch with
        | '"' -> sb.Append("\\\"") |> ignore
        | '\\' -> sb.Append("\\\\") |> ignore
        | '\n' -> sb.Append("\\n") |> ignore
        | '\t' -> sb.Append("\\t") |> ignore
        | '\r' -> sb.Append("\\r") |> ignore
        | c -> sb.Append(c) |> ignore
    sb.ToString()

let rec prettyValue (v: Value) : string =
    match v with
    | VInt n -> string n
    | VBool b -> if b then "true" else "false"
    | VString s -> "\"" + escapeString s + "\""
    | VUnit -> "()"
    | VTuple parts -> "(" + (parts |> List.map prettyValue |> String.concat ", ") + ")"
    | VList items -> "[" + (items |> List.map prettyValue |> String.concat "; ") + "]"
    | VConstruct(name, []) -> name
    | VConstruct(name, [ single ]) -> name + " " + prettyAtom single
    | VConstruct(name, args) -> name + " (" + (args |> List.map prettyValue |> String.concat ", ") + ")"
    | VRecord fields ->
        let body =
            fields
            |> Map.toList
            |> List.map (fun (k, fv) -> k + " = " + prettyValue fv)
            |> String.concat "; "
        "{ " + body + " }"
    | VClosure _ -> "<fun>"
    | VCode _ -> "<fun>"
    | VBuiltin _ -> "<fun>"

and prettyAtom (v: Value) : string =
    match v with
    | VConstruct(_, _ :: _) -> "(" + prettyValue v + ")"
    | _ -> prettyValue v
