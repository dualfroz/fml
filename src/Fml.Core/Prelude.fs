module Fml.Core.Prelude

open Fml.Core.Types
open Fml.Core.Value

let private genVar () = TVar(ref (Generic(newVarId ())))

let initialTypeEnv: Map<string, Ty> =
    Map.ofList
        [ "head", (let a = genVar () in TArrow(TList a, a))
          "tail", (let a = genVar () in TArrow(TList a, TList a))
          "isEmpty", (let a = genVar () in TArrow(TList a, TBool))
          "not", TArrow(TBool, TBool)
          "print", (let a = genVar () in TArrow(a, TUnit))
          "fst", (let a = genVar () in let b = genVar () in TArrow(TTuple [ a; b ], a))
          "snd", (let a = genVar () in let b = genVar () in TArrow(TTuple [ a; b ], b))
          "stringLength", TArrow(TString, TInt)
          "intToString", TArrow(TInt, TString)
          "boolToString", TArrow(TBool, TString) ]

let initialValueEnv: Env =
    Map.ofList
        [ "head",
          VBuiltin(
              "head",
              fun v ->
                  match v with
                  | VList(x :: _) -> x
                  | VList [] -> raise (RuntimeError "head of an empty list")
                  | _ -> raise (RuntimeError "head expects a list")
          )
          "tail",
          VBuiltin(
              "tail",
              fun v ->
                  match v with
                  | VList(_ :: xs) -> VList xs
                  | VList [] -> raise (RuntimeError "tail of an empty list")
                  | _ -> raise (RuntimeError "tail expects a list")
          )
          "isEmpty",
          VBuiltin(
              "isEmpty",
              fun v ->
                  match v with
                  | VList xs -> VBool(List.isEmpty xs)
                  | _ -> raise (RuntimeError "isEmpty expects a list")
          )
          "not",
          VBuiltin(
              "not",
              fun v ->
                  match v with
                  | VBool b -> VBool(not b)
                  | _ -> raise (RuntimeError "not expects a boolean")
          )
          "print",
          VBuiltin(
              "print",
              fun v ->
                  printfn "%s" (prettyValue v)
                  VUnit
          )
          "fst",
          VBuiltin(
              "fst",
              fun v ->
                  match v with
                  | VTuple(a :: _) -> a
                  | _ -> raise (RuntimeError "fst expects a pair")
          )
          "snd",
          VBuiltin(
              "snd",
              fun v ->
                  match v with
                  | VTuple(_ :: b :: _) -> b
                  | _ -> raise (RuntimeError "snd expects a pair")
          )
          "stringLength",
          VBuiltin(
              "stringLength",
              fun v ->
                  match v with
                  | VString s -> VInt s.Length
                  | _ -> raise (RuntimeError "stringLength expects a string")
          )
          "intToString",
          VBuiltin(
              "intToString",
              fun v ->
                  match v with
                  | VInt n -> VString(string n)
                  | _ -> raise (RuntimeError "intToString expects an int")
          )
          "boolToString",
          VBuiltin(
              "boolToString",
              fun v ->
                  match v with
                  | VBool b -> VString(if b then "true" else "false")
                  | _ -> raise (RuntimeError "boolToString expects a bool")
          ) ]

let preludeSource =
    """
type 'a option = None | Some of 'a ;;
type ('a, 'b) result = Ok of 'a | Error of 'b ;;

module Option = struct
  let isSome = fun o -> match o with None -> false | Some _ -> true ;;
  let getOr = fun d o -> match o with None -> d | Some x -> x ;;
  let map = fun f o -> match o with None -> None | Some x -> Some (f x) ;;
  let bind = fun o f -> match o with None -> None | Some x -> f x ;;
end ;;

module Result = struct
  let isOk = fun r -> match r with Ok _ -> true | Error _ -> false ;;
  let map = fun f r -> match r with Ok x -> Ok (f x) | Error e -> Error e ;;
  let mapError = fun f r -> match r with Ok x -> Ok x | Error e -> Error (f e) ;;
  let getOr = fun d r -> match r with Ok x -> x | Error _ -> d ;;
end ;;

module List = struct
  let rec length = fun xs -> match xs with [] -> 0 | _ :: t -> 1 + length t ;;
  let rec map = fun f xs -> match xs with [] -> [] | h :: t -> f h :: map f t ;;
  let rec filter = fun p xs ->
    match xs with
    | [] -> []
    | h :: t -> if p h then h :: filter p t else filter p t ;;
  let rec foldl = fun f acc xs ->
    match xs with
    | [] -> acc
    | h :: t -> foldl f (f acc h) t ;;
  let rec foldr = fun f acc xs ->
    match xs with
    | [] -> acc
    | h :: t -> f h (foldr f acc t) ;;
  let rec append = fun xs ys ->
    match xs with
    | [] -> ys
    | h :: t -> h :: append t ys ;;
  let rec rev = fun xs ->
    match xs with
    | [] -> []
    | h :: t -> append (rev t) (h :: []) ;;
  let rec mem = fun x xs ->
    match xs with
    | [] -> false
    | h :: t -> if h = x then true else mem x t ;;
  let rec range = fun lo hi -> if lo >= hi then [] else lo :: range (lo + 1) hi ;;
  let sum = fun xs -> foldl (fun acc x -> acc + x) 0 xs ;;
end ;;

module Map = struct
  let empty = [] ;;
  let rec add = fun k v m ->
    match m with
    | [] -> (k, v) :: []
    | pair :: rest -> if fst pair = k then (k, v) :: rest else pair :: add k v rest ;;
  let rec find = fun k m ->
    match m with
    | [] -> None
    | pair :: rest -> if fst pair = k then Some (snd pair) else find k rest ;;
  let contains = fun k m -> Option.isSome (find k m) ;;
  let size = fun m -> List.length m ;;
  let rec remove = fun k m ->
    match m with
    | [] -> []
    | pair :: rest -> if fst pair = k then rest else pair :: remove k rest ;;
end ;;
"""
