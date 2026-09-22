module Fml.Core.Bytecode

open System.Collections.Generic
open Fml.Core.Ast
open Fml.Core.Value
open Fml.Core.Match

type Instr =
    | Push of Value
    | LoadLocal of int
    | LoadGlobal of Value ref
    | MakeClosure of int
    | MakeRec of int list
    | Bind
    | PopEnv of int
    | Pop
    | Call
    | TailCall
    | Ret
    | Jump of int
    | JumpIfFalse of int
    | MakeTuple of int
    | NilList
    | ConsList
    | MakeConstruct of string * int
    | MakeRecord of string list
    | GetField of string
    | Proj of int
    | NegInt
    | Prim of string
    | TestInt of int
    | TestStr of string
    | TestBool of bool
    | TestNil
    | TestCons
    | TestCtor of string
    | MatchFail

let private isTrivialTest (test: SwitchTest) : bool =
    match test with
    | SUnit | STuple | SRecord -> true
    | _ -> false

let private testInstr (test: SwitchTest) : Instr =
    match test with
    | SInt n -> TestInt n
    | SStr s -> TestStr s
    | SBool b -> TestBool b
    | SNil -> TestNil
    | SCons -> TestCons
    | SCtor tag -> TestCtor tag
    | SUnit | STuple | SRecord -> failwith "trivial tests are not emitted"

let rec compileBlock (functions: ResizeArray<Instr []>) (globals: Map<string, Value ref>) (rootScope: string list) (rootTail: bool) (expr: Expr) : Instr [] =
    let code = ResizeArray<Instr>()
    let labels = Dictionary<int, int>()
    let fixups = ResizeArray<int * int>()
    let mutable nextLabel = 0
    let newLabel () =
        let l = nextLabel
        nextLabel <- nextLabel + 1
        l
    let emit i = code.Add i
    let emitJump lbl =
        fixups.Add(code.Count, lbl)
        code.Add(Jump -1)
    let emitJumpIfFalse lbl =
        fixups.Add(code.Count, lbl)
        code.Add(JumpIfFalse -1)
    let mark lbl = labels.[lbl] <- code.Count

    let localIndex (scope: string list) (name: string) : int option =
        scope |> List.tryFindIndex ((=) name)

    let compileFunction (scope: string list) (param: string) (body: Expr) : int =
        let id = functions.Count
        functions.Add [||]
        let block = compileBlock functions globals (param :: scope) true body
        functions.[id] <- block
        id

    let compileRecGroup (scope: string list) (bindings: (string * Expr) list) =
        let groupScope = (bindings |> List.map fst) @ scope
        let ids =
            bindings
            |> List.map (fun (_, rhs) ->
                match rhs with
                | Lambda(param, body) ->
                    let id = functions.Count
                    functions.Add [||]
                    let block = compileBlock functions globals (param :: groupScope) true body
                    functions.[id] <- block
                    id
                | _ -> raise (RuntimeError "let rec binding must be a function"))
        emit (MakeRec ids)

    let rec go (scope: string list) (tail: bool) (e: Expr) =
        match e with
        | IntLit n -> emit (Push(VInt n))
        | BoolLit b -> emit (Push(VBool b))
        | StringLit s -> emit (Push(VString s))
        | UnitLit -> emit (Push VUnit)
        | Var x ->
            match localIndex scope x with
            | Some idx -> emit (LoadLocal idx)
            | None ->
                match Map.tryFind x globals with
                | Some cell -> emit (LoadGlobal cell)
                | None -> raise (RuntimeError(sprintf "unbound variable '%s'" x))
        | Lambda(param, body) ->
            let id = compileFunction scope param body
            emit (MakeClosure id)
        | App(fn, arg) ->
            go scope false fn
            go scope false arg
            emit (if tail then TailCall else Call)
        | Let(false, name, rhs, body) ->
            go scope false rhs
            emit Bind
            go (name :: scope) tail body
            if not tail then emit (PopEnv 1)
        | Let(true, name, rhs, body) ->
            compileRecGroup scope [ name, rhs ]
            go (name :: scope) tail body
            if not tail then emit (PopEnv 1)
        | LetGroup(true, bindings, body) ->
            compileRecGroup scope bindings
            let scope' = (bindings |> List.map fst) @ scope
            go scope' tail body
            if not tail then emit (PopEnv(List.length bindings))
        | LetGroup(false, bindings, body) ->
            for (_, rhs) in bindings do
                go scope false rhs
            for _ in bindings do
                emit Bind
            let scope' = (bindings |> List.map fst) @ scope
            go scope' tail body
            if not tail then emit (PopEnv(List.length bindings))
        | If(cond, thenBranch, elseBranch) ->
            go scope false cond
            let lElse = newLabel ()
            let lEnd = newLabel ()
            emitJumpIfFalse lElse
            go scope tail thenBranch
            emitJump lEnd
            mark lElse
            go scope tail elseBranch
            mark lEnd
        | Neg inner ->
            go scope false inner
            emit NegInt
        | BinOp("&&", a, b) -> go scope tail (If(a, b, BoolLit false))
        | BinOp("||", a, b) -> go scope tail (If(a, BoolLit true, b))
        | BinOp(op, a, b) ->
            go scope false a
            go scope false b
            emit (Prim op)
        | Tuple items ->
            for item in items do
                go scope false item
            emit (MakeTuple(List.length items))
        | Nil -> emit NilList
        | Cons(headExpr, tailExpr) ->
            go scope false headExpr
            go scope false tailExpr
            emit ConsList
        | Construct(name, args) ->
            for arg in args do
                go scope false arg
            emit (MakeConstruct(name, List.length args))
        | Record fields ->
            for (_, fieldExpr) in fields do
                go scope false fieldExpr
            emit (MakeRecord(fields |> List.map fst))
        | Field(record, fieldName) ->
            go scope false record
            emit (GetField fieldName)
        | Match(scrut, cases) -> compileMatchExpr scope tail scrut cases

    and compileMatchExpr (scope: string list) (tail: bool) (scrut: Expr) (cases: (Pattern * Expr) list) =
        go scope false scrut
        emit Bind
        let scrutScope = "$scrut" :: scope
        let tree, bindingsByRow = compileMatch (cases |> List.map fst)
        let bodies = cases |> List.map snd |> Array.ofList
        let lJoin = newLabel ()
        let loadScrut (currentScope: string list) (path: Path) =
            let idx =
                match localIndex currentScope "$scrut" with
                | Some i -> i
                | None -> raise (RuntimeError "lost the match scrutinee")
            emit (LoadLocal idx)
            for step in path do
                emit (Proj step)
        let rec lower (tree: DecisionTree) =
            match tree with
            | Leaf row ->
                let binds = bindingsByRow.[row]
                let mutable curScope = scrutScope
                for (varName, path) in binds do
                    loadScrut curScope path
                    emit Bind
                    curScope <- varName :: curScope
                go curScope tail bodies.[row]
                if not tail then emit (PopEnv(List.length binds))
                emitJump lJoin
            | FailNode -> emit MatchFail
            | SwitchNode(path, casesList, defaultTree) ->
                match casesList, defaultTree with
                | [ (test, sub) ], None when isTrivialTest test -> lower sub
                | _ ->
                    for (test, sub) in casesList do
                        let lNext = newLabel ()
                        loadScrut scrutScope path
                        emit (testInstr test)
                        emitJumpIfFalse lNext
                        lower sub
                        mark lNext
                    match defaultTree with
                    | Some dt -> lower dt
                    | None -> emit MatchFail
        lower tree
        mark lJoin
        if not tail then emit (PopEnv 1)

    go rootScope rootTail expr
    emit Ret
    for (idx, lbl) in fixups do
        let target = labels.[lbl]
        code.[idx] <-
            match code.[idx] with
            | Jump _ -> Jump target
            | JumpIfFalse _ -> JumpIfFalse target
            | other -> other
    code.ToArray()
