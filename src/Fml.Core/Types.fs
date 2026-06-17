module Fml.Core.Types

// Types for the Hindley-Milner core. Type variables are mutable cells so
// unification can link them in place. Generalization uses the level trick
// (Remy / OCaml): a variable is generalizable when its level is deeper than
// the level of the let binding it escapes from.

type Ty =
    | TInt
    | TBool
    | TString
    | TUnit
    | TArrow of Ty * Ty
    | TTuple of Ty list
    | TList of Ty
    | TCon of string * Ty list
    | TVar of TypeVar ref

and TypeVar =
    | Unbound of int * int
    | Link of Ty
    | Generic of int

exception TypeError of string

let mutable private counter = 0
let mutable currentLevel = 0

let newVarId () =
    counter <- counter + 1
    counter

let newVar () = TVar(ref (Unbound(newVarId (), currentLevel)))
let enterLevel () = currentLevel <- currentLevel + 1
let leaveLevel () = currentLevel <- currentLevel - 1

let rec repr (t: Ty) : Ty =
    match t with
    | TVar r ->
        match r.Value with
        | Link inner ->
            let resolved = repr inner
            r.Value <- Link resolved
            resolved
        | _ -> t
    | _ -> t

let prettyTy (t: Ty) : string =
    let names = System.Collections.Generic.Dictionary<int, string>()
    let mutable next = 0
    let nameFor id =
        match names.TryGetValue id with
        | true, nm -> nm
        | _ ->
            let letter = char (int 'a' + next % 26)
            let suffix = if next / 26 = 0 then "" else string (next / 26)
            let nm = "'" + string letter + suffix
            next <- next + 1
            names.[id] <- nm
            nm
    let rec go prec (ty: Ty) =
        match repr ty with
        | TInt -> "int"
        | TBool -> "bool"
        | TString -> "string"
        | TUnit -> "unit"
        | TVar r ->
            match r.Value with
            | Unbound(id, _) -> nameFor id
            | Generic id -> nameFor id
            | Link inner -> go prec inner
        | TList elem -> go 2 elem + " list"
        | TCon(name, []) -> name
        | TCon(name, [ single ]) -> go 2 single + " " + name
        | TCon(name, args) ->
            "(" + (args |> List.map (go 0) |> String.concat ", ") + ") " + name
        | TTuple parts ->
            let body = parts |> List.map (go 2) |> String.concat " * "
            if prec > 1 then "(" + body + ")" else body
        | TArrow(a, b) ->
            let body = go 1 a + " -> " + go 0 b
            if prec > 0 then "(" + body + ")" else body
    go 0 t

let rec generalize (t: Ty) =
    match repr t with
    | TVar r ->
        match r.Value with
        | Unbound(id, level) when level > currentLevel -> r.Value <- Generic id
        | _ -> ()
    | TArrow(a, b) -> generalize a; generalize b
    | TList elem -> generalize elem
    | TCon(_, args) -> List.iter generalize args
    | TTuple parts -> List.iter generalize parts
    | TInt | TBool | TString | TUnit -> ()

let instantiate (t: Ty) : Ty =
    let mapping = System.Collections.Generic.Dictionary<int, Ty>()
    let rec go ty =
        match repr ty with
        | TVar r ->
            match r.Value with
            | Generic id ->
                match mapping.TryGetValue id with
                | true, fresh -> fresh
                | _ ->
                    let fresh = newVar ()
                    mapping.[id] <- fresh
                    fresh
            | _ -> ty
        | TArrow(a, b) -> TArrow(go a, go b)
        | TList elem -> TList(go elem)
        | TCon(name, args) -> TCon(name, List.map go args)
        | TTuple parts -> TTuple(List.map go parts)
        | (TInt | TBool | TString | TUnit) as concrete -> concrete
    go t

let private occursAndAdjust (id: int) (level: int) (ty: Ty) =
    let rec go ty =
        match repr ty with
        | TVar r ->
            match r.Value with
            | Unbound(id2, lvl2) ->
                if id2 = id then raise (TypeError "occurs check failed: infinite type")
                elif lvl2 > level then r.Value <- Unbound(id2, level)
            | Generic _ -> ()
            | Link inner -> go inner
        | TArrow(a, b) -> go a; go b
        | TList elem -> go elem
        | TCon(_, args) -> List.iter go args
        | TTuple parts -> List.iter go parts
        | TInt | TBool | TString | TUnit -> ()
    go ty

let private failUnify a b =
    raise (TypeError(sprintf "cannot unify %s with %s" (prettyTy a) (prettyTy b)))

let rec unify (t1: Ty) (t2: Ty) =
    let a = repr t1
    let b = repr t2
    match a, b with
    | TVar r1, TVar r2 when System.Object.ReferenceEquals(r1, r2) -> ()
    | TVar r, other ->
        match r.Value with
        | Unbound(id, level) ->
            occursAndAdjust id level other
            r.Value <- Link other
        | _ -> failUnify a b
    | other, TVar r ->
        match r.Value with
        | Unbound(id, level) ->
            occursAndAdjust id level other
            r.Value <- Link other
        | _ -> failUnify a b
    | TInt, TInt | TBool, TBool | TString, TString | TUnit, TUnit -> ()
    | TArrow(a1, a2), TArrow(b1, b2) ->
        unify a1 b1
        unify a2 b2
    | TList e1, TList e2 -> unify e1 e2
    | TCon(n1, a1), TCon(n2, a2) ->
        if n1 <> n2 || List.length a1 <> List.length a2 then failUnify a b
        else List.iter2 unify a1 a2
    | TTuple xs, TTuple ys ->
        if List.length xs <> List.length ys then failUnify a b
        else List.iter2 unify xs ys
    | _ -> failUnify a b
