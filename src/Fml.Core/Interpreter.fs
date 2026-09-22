module Fml.Core.Interpreter

open Fml.Core.Ast
open Fml.Core.Lexer
open Fml.Core.Parser
open Fml.Core.Types
open Fml.Core.Decls
open Fml.Core.Match
open Fml.Core.Infer
open Fml.Core.Value
open Fml.Core.Eval
open Fml.Core.Prelude
open Fml.Core.Bytecode

type Backend =
    | Interp
    | Vm

type PhraseResult =
    { Name: string
      TypeText: string
      Value: Value
      ValueText: string }

let parsePhrases (source: string) : Phrase list = source |> tokenize |> parse

let rec private patternVars (pat: Pattern) : Set<string> =
    match pat with
    | PVar x -> Set.singleton x
    | PWildcard | PInt _ | PBool _ | PString _ | PUnit | PNil -> Set.empty
    | PTuple ps -> ps |> List.map patternVars |> Set.unionMany
    | PCons(h, t) -> Set.union (patternVars h) (patternVars t)
    | PConstruct(_, ps) -> ps |> List.map patternVars |> Set.unionMany
    | PRecord fields -> fields |> List.map (snd >> patternVars) |> Set.unionMany

let rec renameFreeVars (mapping: Map<string, string>) (bound: Set<string>) (e: Expr) : Expr =
    let recur b = renameFreeVars mapping b
    match e with
    | IntLit _ | BoolLit _ | StringLit _ | UnitLit | Nil -> e
    | Var x ->
        if Set.contains x bound then e
        else
            match Map.tryFind x mapping with
            | Some qualified -> Var qualified
            | None -> e
    | Lambda(p, body) -> Lambda(p, recur (Set.add p bound) body)
    | App(fn, arg) -> App(recur bound fn, recur bound arg)
    | Let(false, name, rhs, body) -> Let(false, name, recur bound rhs, recur (Set.add name bound) body)
    | Let(true, name, rhs, body) ->
        let inner = Set.add name bound
        Let(true, name, recur inner rhs, recur inner body)
    | LetGroup(isRec, bindings, body) ->
        let names = bindings |> List.map fst |> Set.ofList
        let rhsBound = if isRec then Set.union bound names else bound
        let renamed = bindings |> List.map (fun (n, r) -> n, recur rhsBound r)
        LetGroup(isRec, renamed, recur (Set.union bound names) body)
    | If(c, t, f) -> If(recur bound c, recur bound t, recur bound f)
    | BinOp(op, l, r) -> BinOp(op, recur bound l, recur bound r)
    | Neg inner -> Neg(recur bound inner)
    | Tuple items -> Tuple(items |> List.map (recur bound))
    | Cons(h, t) -> Cons(recur bound h, recur bound t)
    | Construct(name, args) -> Construct(name, args |> List.map (recur bound))
    | Record fields -> Record(fields |> List.map (fun (n, v) -> n, recur bound v))
    | Field(record, name) -> Field(recur bound record, name)
    | Match(scrut, cases) ->
        let cases' =
            cases
            |> List.map (fun (pat, body) -> pat, recur (Set.union bound (patternVars pat)) body)
        Match(recur bound scrut, cases')

let private takesSingleField (name: string) : bool =
    match lookupCtor name with
    | Some info -> List.length info.ArgTypes = 1
    | None -> false

let rec private groupCtorPatternArgs (pat: Pattern) : Pattern =
    match pat with
    | PVar _ | PWildcard | PInt _ | PBool _ | PString _ | PUnit | PNil -> pat
    | PTuple ps -> PTuple(List.map groupCtorPatternArgs ps)
    | PCons(h, t) -> PCons(groupCtorPatternArgs h, groupCtorPatternArgs t)
    | PConstruct(name, ps) ->
        let args = List.map groupCtorPatternArgs ps
        if List.length args > 1 && takesSingleField name then PConstruct(name, [ PTuple args ])
        else PConstruct(name, args)
    | PRecord fields -> PRecord(fields |> List.map (fun (n, p) -> n, groupCtorPatternArgs p))

let rec private groupCtorArgs (e: Expr) : Expr =
    match e with
    | IntLit _ | BoolLit _ | StringLit _ | UnitLit | Nil | Var _ -> e
    | Lambda(p, body) -> Lambda(p, groupCtorArgs body)
    | App(fn, arg) -> App(groupCtorArgs fn, groupCtorArgs arg)
    | Let(isRec, name, rhs, body) -> Let(isRec, name, groupCtorArgs rhs, groupCtorArgs body)
    | LetGroup(isRec, bindings, body) ->
        LetGroup(isRec, bindings |> List.map (fun (n, r) -> n, groupCtorArgs r), groupCtorArgs body)
    | If(c, t, f) -> If(groupCtorArgs c, groupCtorArgs t, groupCtorArgs f)
    | BinOp(op, l, r) -> BinOp(op, groupCtorArgs l, groupCtorArgs r)
    | Neg inner -> Neg(groupCtorArgs inner)
    | Tuple items -> Tuple(List.map groupCtorArgs items)
    | Cons(h, t) -> Cons(groupCtorArgs h, groupCtorArgs t)
    | Construct(name, args) ->
        let grouped = List.map groupCtorArgs args
        if List.length grouped > 1 && takesSingleField name then Construct(name, [ Tuple grouped ])
        else Construct(name, grouped)
    | Record fields -> Record(fields |> List.map (fun (n, v) -> n, groupCtorArgs v))
    | Field(record, name) -> Field(groupCtorArgs record, name)
    | Match(scrut, cases) ->
        Match(groupCtorArgs scrut, cases |> List.map (fun (pat, body) -> groupCtorPatternArgs pat, groupCtorArgs body))

type Session(backend: Backend) =
    do resetDecls ()
    let mutable typeEnv = initialTypeEnv
    let mutable runtimeEnv: Env = initialValueEnv
    let functions = ResizeArray<Instr []>()
    let mutable moduleMembers: Map<string, string list> = Map.empty

    let globalCells (pending: (string * Value ref) list) : Map<string, Value ref> =
        pending |> List.fold (fun cells (n, cell) -> Map.add n cell cells) (runtimeEnv |> Map.map (fun _ v -> ref v))

    let evalExpr (e: Expr) : Value =
        match backend with
        | Interp -> eval runtimeEnv e
        | Vm -> Vm.compileAndRun functions (globalCells []) e

    let inferTop (e: Expr) : Ty =
        currentLevel <- 0
        enterLevel ()
        let ty = infer typeEnv e
        leaveLevel ()
        generalize ty
        ty

    let processExpr (parsed: Expr) : PhraseResult =
        let e = groupCtorArgs parsed
        let ty = inferTop e
        let v = evalExpr e
        typeEnv <- Map.add "it" ty typeEnv
        runtimeEnv <- Map.add "it" v runtimeEnv
        { Name = "it"
          TypeText = prettyTy ty
          Value = v
          ValueText = prettyValue v }

    let processLet (isRec: bool) (parsed: (string * Expr) list) : PhraseResult list =
        let bindings = parsed |> List.map (fun (n, rhs) -> n, groupCtorArgs rhs)
        currentLevel <- 0
        let newTypeEnv = inferLetGroup typeEnv isRec bindings
        let values =
            match backend with
            | Interp ->
                let newEnv = evalLetGroup runtimeEnv isRec bindings
                runtimeEnv <- newEnv
                bindings |> List.map (fun (n, _) -> n, Map.find n newEnv)
            | Vm ->
                let cells = bindings |> List.map (fun (n, _) -> n, ref VUnit)
                let globals = globalCells (if isRec then cells else [])
                let vs = bindings |> List.map (fun (n, rhs) -> n, Vm.compileAndRun functions globals rhs)
                List.iter2 (fun (_, cell: Value ref) (_, v) -> cell.Value <- v) cells vs
                runtimeEnv <- vs |> List.fold (fun e (n, v) -> Map.add n v e) runtimeEnv
                vs
        typeEnv <- newTypeEnv
        bindings
        |> List.map (fun (n, _) ->
            let ty = Map.find n newTypeEnv
            let v = values |> List.find (fun (m, _) -> m = n) |> snd
            { Name = n
              TypeText = prettyTy ty
              Value = v
              ValueText = prettyValue v })

    let processOpen (name: string) =
        match Map.tryFind name moduleMembers with
        | Some members ->
            for m in members do
                let qualified = name + "." + m
                typeEnv <- Map.add m (Map.find qualified typeEnv) typeEnv
                runtimeEnv <- Map.add m (Map.find qualified runtimeEnv) runtimeEnv
        | None -> raise (TypeError(sprintf "unknown module '%s'" name))

    let rec processModule (name: string) (inner: Phrase list) =
        let memberNames =
            inner
            |> List.collect (fun ph ->
                match ph with
                | PhraseLet(_, bindings) -> bindings |> List.map fst
                | _ -> [])
        let mapping = memberNames |> List.map (fun n -> n, name + "." + n) |> Map.ofList
        for ph in inner do
            match ph with
            | PhraseType decls -> decls |> List.iter registerDecl
            | PhraseOpen m -> processOpen m
            | PhraseLet(isRec, bindings) ->
                let qualified =
                    bindings |> List.map (fun (n, rhs) -> name + "." + n, renameFreeVars mapping Set.empty rhs)
                processLet isRec qualified |> ignore
            | PhraseExpr e -> processExpr (renameFreeVars mapping Set.empty e) |> ignore
            | PhraseModule _ -> raise (ParseError "nested modules are not supported")
        moduleMembers <- Map.add name memberNames moduleMembers

    let processPhrase (ph: Phrase) : PhraseResult list =
        match ph with
        | PhraseExpr e -> [ processExpr e ]
        | PhraseLet(isRec, bindings) -> processLet isRec bindings
        | PhraseType decls -> decls |> List.iter registerDecl; []
        | PhraseOpen name -> processOpen name; []
        | PhraseModule(name, inner) -> processModule name inner; []

    new() = Session(Interp)

    member _.TypeEnv = typeEnv
    member _.ValueEnv = runtimeEnv
    member _.RunPhrase(ph: Phrase) : PhraseResult list = processPhrase ph

    member this.Feed(source: string) : PhraseResult list =
        source |> parsePhrases |> List.collect this.RunPhrase

    member this.LoadPrelude() : unit = this.Feed preludeSource |> ignore

let private lastResult (results: PhraseResult list) : PhraseResult =
    match List.tryLast results with
    | Some r -> r
    | None -> raise (RuntimeError "program produced no result")

let runProgram (source: string) : PhraseResult list =
    let session = Session(Interp)
    session.Feed source

let typeOf (source: string) : string = (lastResult (runProgram source)).TypeText

let evalString (source: string) : Value = (lastResult (runProgram source)).Value

let evalToString (source: string) : string = prettyValue (evalString source)

let tryTypeOf (source: string) : Result<string, string> =
    try
        Ok(typeOf source)
    with
    | TypeError m -> Error m
    | MatchError m -> Error m
    | ParseError m -> Error m
    | LexError m -> Error m

let evalOn (backend: Backend) (loadPrelude: bool) (source: string) : Value =
    let session = Session(backend)
    if loadPrelude then session.LoadPrelude()
    lastResult(session.Feed source).Value

let evalInterp (source: string) : Value = evalOn Interp false source
let evalVm (source: string) : Value = evalOn Vm false source

let tryEval (source: string) : Result<Value, string> =
    try
        Ok(evalInterp source)
    with
    | TypeError m -> Error m
    | MatchError m -> Error m
    | RuntimeError m -> Error m
    | ParseError m -> Error m
    | LexError m -> Error m
