module Fml.Core.Match

open Fml.Core.Ast
open Fml.Core.Decls

// Pattern-match analysis and compilation. Patterns are normalized to a uniform
// constructor form (CPat). On that form we run Maranget's usefulness algorithm
// for exhaustiveness and redundancy, and a matrix compiler that lowers a match
// into a decision tree the bytecode backend can walk.

exception MatchError of string

type CPat =
    | CWild
    | CCtor of string * int * CPat list

type Signature =
    | Finite of (string * int) list
    | Infinite

// A path addresses a sub-value of the scrutinee by successive positional
// projections: tuple element, cons head(0)/tail(1), constructor argument, or
// record field in sorted-name order.
type Path = int list

type SwitchTest =
    | SInt of int
    | SStr of string
    | SBool of bool
    | SUnit
    | SNil
    | SCons
    | SCtor of string
    | STuple
    | SRecord

type DecisionTree =
    | Leaf of int
    | FailNode
    | SwitchNode of Path * (SwitchTest * DecisionTree) list * DecisionTree option

let private recordSortedFields (typeName: string) : string list =
    match lookupType typeName with
    | Some ti -> List.sort ti.Fields
    | None -> []

let private recordOrderedPatterns (fields: (string * Pattern) list) : Pattern list =
    match fields with
    | [] -> []
    | (firstName, _) :: _ ->
        let typeName =
            match lookupField firstName with
            | Some info -> info.TypeName
            | None -> ""
        let provided = Map.ofList fields
        recordSortedFields typeName
        |> List.map (fun name ->
            match Map.tryFind name provided with
            | Some p -> p
            | None -> PWildcard)

let rec toCPat (p: Pattern) : CPat =
    match p with
    | PWildcard | PVar _ -> CWild
    | PInt n -> CCtor("#int:" + string n, 0, [])
    | PString s -> CCtor("#str:" + s, 0, [])
    | PBool b -> CCtor("#bool:" + (if b then "true" else "false"), 0, [])
    | PUnit -> CCtor("#unit", 0, [])
    | PNil -> CCtor("#nil", 0, [])
    | PCons(h, t) -> CCtor("#cons", 2, [ toCPat h; toCPat t ])
    | PTuple ps -> CCtor("#tuple:" + string (List.length ps), List.length ps, List.map toCPat ps)
    | PConstruct(name, ps) -> CCtor(name, List.length ps, List.map toCPat ps)
    | PRecord fields ->
        let ordered = recordOrderedPatterns fields
        let typeName =
            match fields with
            | (firstName, _) :: _ ->
                match lookupField firstName with
                | Some info -> info.TypeName
                | None -> ""
            | [] -> ""
        CCtor("#record:" + typeName, List.length ordered, List.map toCPat ordered)

let private signatureOf (tag: string) : Signature =
    if tag.StartsWith "#int:" || tag.StartsWith "#str:" then Infinite
    elif tag = "#bool:true" || tag = "#bool:false" then Finite [ "#bool:true", 0; "#bool:false", 0 ]
    elif tag = "#unit" then Finite [ "#unit", 0 ]
    elif tag = "#nil" || tag = "#cons" then Finite [ "#nil", 0; "#cons", 2 ]
    elif tag.StartsWith "#tuple:" then
        let arity = int (tag.Substring 7)
        Finite [ tag, arity ]
    elif tag.StartsWith "#record:" then
        let typeName = tag.Substring 8
        match lookupType typeName with
        | Some ti -> Finite [ tag, List.length ti.Fields ]
        | None -> Finite [ tag, 0 ]
    else
        match lookupCtor tag with
        | Some ci ->
            match lookupType ci.TypeName with
            | Some ti -> Finite(ti.Ctors |> List.map (fun c -> c, ctorArity c))
            | None -> Finite [ tag, List.length ci.ArgTypes ]
        | None -> Finite [ tag, 0 ]

let private completeSignature (presentTags: string list) : (string * int) list option =
    match presentTags with
    | [] -> None
    | first :: _ ->
        match signatureOf first with
        | Infinite -> None
        | Finite fullSig ->
            let fullTagSet = fullSig |> List.map fst |> Set.ofList
            let presentSet = Set.ofList presentTags
            if Set.isSubset fullTagSet presentSet then Some fullSig else None

let private isWild p =
    match p with
    | CWild -> true
    | _ -> false

let private specializeRow (tag: string) (arity: int) (row: CPat list) : CPat list option =
    match row with
    | head :: rest ->
        match head with
        | CWild -> Some(List.replicate arity CWild @ rest)
        | CCtor(t, _, args) -> if t = tag then Some(args @ rest) else None
    | [] -> None

let private specialize tag arity matrix = matrix |> List.choose (specializeRow tag arity)

let private defaultRow (row: CPat list) : CPat list option =
    match row with
    | CWild :: rest -> Some rest
    | _ -> None

let private defaultMatrix matrix = matrix |> List.choose defaultRow

let private headTags matrix =
    matrix
    |> List.choose (fun row ->
        match row with
        | CCtor(t, a, _) :: _ -> Some(t, a)
        | _ -> None)
    |> List.distinctBy fst

let rec private useful (matrix: CPat list list) (q: CPat list) : bool =
    match q with
    | [] -> List.isEmpty matrix
    | q1 :: qs ->
        match q1 with
        | CCtor(tag, arity, args) -> useful (specialize tag arity matrix) (args @ qs)
        | CWild ->
            let present = headTags matrix |> List.map fst
            match completeSignature present with
            | Some fullSig ->
                fullSig
                |> List.exists (fun (tag, arity) ->
                    useful (specialize tag arity matrix) (List.replicate arity CWild @ qs))
            | None -> useful (defaultMatrix matrix) qs

let private describeCPat (p: CPat) : string =
    match p with
    | CWild -> "_"
    | CCtor(tag, _, _) ->
        if tag.StartsWith "#int:" then tag.Substring 5
        elif tag.StartsWith "#str:" then "a string"
        elif tag.StartsWith "#bool:" then tag.Substring 6
        elif tag = "#unit" then "()"
        elif tag = "#nil" then "[]"
        elif tag = "#cons" then "_ :: _"
        elif tag.StartsWith "#tuple:" then "a tuple"
        elif tag.StartsWith "#record:" then "a record"
        else tag

// Renders a constructor pattern back into surface-like syntax so an
// exhaustiveness counterexample reads the way a user would have written it.
let rec private renderCPat (p: CPat) : string =
    match p with
    | CWild -> "_"
    | CCtor(tag, _, args) ->
        if tag.StartsWith "#int:" then tag.Substring 5
        elif tag.StartsWith "#str:" then "\"" + tag.Substring 5 + "\""
        elif tag.StartsWith "#bool:" then tag.Substring 6
        elif tag = "#unit" then "()"
        elif tag = "#nil" then "[]"
        elif tag = "#cons" then
            match args with
            | [ h; t ] -> renderConsArg h + " :: " + renderCPat t
            | _ -> "_ :: _"
        elif tag.StartsWith "#tuple:" then
            "(" + (args |> List.map renderCPat |> String.concat ", ") + ")"
        elif tag.StartsWith "#record:" then "{ ... }"
        elif List.isEmpty args then tag
        else tag + " " + (args |> List.map renderCtorArg |> String.concat " ")

and private renderCtorArg (p: CPat) : string =
    match p with
    | CCtor(tag, _, _ :: _) when not (tag.StartsWith "#") -> "(" + renderCPat p + ")"
    | CCtor("#cons", _, _) -> "(" + renderCPat p + ")"
    | _ -> renderCPat p

and private renderConsArg (p: CPat) : string =
    match p with
    | CCtor("#cons", _, _) -> "(" + renderCPat p + ")"
    | _ -> renderCPat p

// Picks a head constructor (or literal value) that no clause in the column
// mentions, so the witness names a genuinely missing case.
let private missingHead (presentTags: string list) : CPat option =
    match presentTags with
    | [] -> None
    | first :: _ ->
        match signatureOf first with
        | Infinite ->
            if first.StartsWith "#int:" then
                let used =
                    presentTags
                    |> List.choose (fun t -> if t.StartsWith "#int:" then Some(int (t.Substring 5)) else None)
                    |> Set.ofList
                let mutable v = 0
                while Set.contains v used do v <- v + 1
                Some(CCtor("#int:" + string v, 0, []))
            elif first.StartsWith "#str:" then
                let used = presentTags |> List.filter (fun t -> t.StartsWith "#str:") |> Set.ofList
                let mutable v = "#str:x"
                while Set.contains v used do v <- v + "x"
                Some(CCtor(v, 0, []))
            else None
        | Finite fullSig ->
            match fullSig |> List.filter (fun (t, _) -> not (List.contains t presentTags)) with
            | (tag, arity) :: _ -> Some(CCtor(tag, arity, List.replicate arity CWild))
            | [] -> None

// Maranget's witness-producing usefulness algorithm. Returns a counterexample
// row of width n that no row of the matrix matches, or None when the matrix is
// exhaustive over n columns.
let rec private witnessRows (matrix: CPat list list) (n: int) : CPat list option =
    if n = 0 then
        if List.isEmpty matrix then Some [] else None
    else
        let present = headTags matrix
        let presentTags = present |> List.map fst
        match completeSignature presentTags with
        | Some fullSig ->
            fullSig
            |> List.tryPick (fun (tag, arity) ->
                match witnessRows (specialize tag arity matrix) (arity + n - 1) with
                | Some w ->
                    let args = w |> List.take arity
                    let rest = w |> List.skip arity
                    Some(CCtor(tag, arity, args) :: rest)
                | None -> None)
        | None ->
            match witnessRows (defaultMatrix matrix) (n - 1) with
            | Some w ->
                let head =
                    match missingHead presentTags with
                    | Some cpat -> cpat
                    | None -> CWild
                Some(head :: w)
            | None -> None

// Runs redundancy and exhaustiveness analysis, raising MatchError on failure.
let checkMatch (patterns: Pattern list) : unit =
    let rows = patterns |> List.map (fun p -> [ toCPat p ])
    rows
    |> List.iteri (fun i row ->
        let earlier = rows |> List.take i
        if not (useful earlier row) then
            raise (MatchError(sprintf "unreachable match case: %s" (describeCPat row.Head))))
    match witnessRows rows 1 with
    | Some(witness :: _) ->
        raise (MatchError(sprintf "non-exhaustive match: no clause covers %s" (renderCPat witness)))
    | _ -> ()
