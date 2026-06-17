module Fml.Core.Eval

open Fml.Core.Ast
open Fml.Core.Value

let rec eval (env: Env) (expr: Expr) : Value =
    match expr with
    | IntLit n -> VInt n
    | BoolLit b -> VBool b
    | StringLit s -> VString s
    | UnitLit -> VUnit
    | Var x ->
        match Map.tryFind x env with
        | Some v -> v
        | None -> raise (RuntimeError(sprintf "unbound variable '%s'" x))
    | Lambda(x, body) -> VClosure(x, body, ref env)
    | App(fn, arg) ->
        let fnValue = eval env fn
        let argValue = eval env arg
        match fnValue with
        | VClosure(x, body, closureEnv) -> eval (Map.add x argValue closureEnv.Value) body
        | VBuiltin(_, f) -> f argValue
        | _ -> raise (RuntimeError "attempt to call a non-function value")
    | Let(false, name, rhs, body) ->
        let v = eval env rhs
        eval (Map.add name v env) body
    | Let(true, name, rhs, body) ->
        let v = eval env rhs
        match v with
        | VClosure(_, _, closureEnv) ->
            closureEnv.Value <- Map.add name v closureEnv.Value
            eval (Map.add name v env) body
        | _ -> eval (Map.add name v env) body
    | LetGroup(isRec, bindings, body) ->
        let groupEnv = evalLetGroup env isRec bindings
        eval groupEnv body
    | If(cond, thenBranch, elseBranch) ->
        match eval env cond with
        | VBool true -> eval env thenBranch
        | VBool false -> eval env elseBranch
        | _ -> raise (RuntimeError "if condition is not a boolean")
    | Neg inner ->
        match eval env inner with
        | VInt n -> VInt(-n)
        | _ -> raise (RuntimeError "negation of a non-integer")
    | BinOp(op, l, r) -> evalBinOp env op l r
    | Tuple items -> VTuple(List.map (eval env) items)
    | Nil -> VList []
    | Cons(headExpr, tailExpr) ->
        let headValue = eval env headExpr
        match eval env tailExpr with
        | VList items -> VList(headValue :: items)
        | _ -> raise (RuntimeError "tail of a cons is not a list")
    | Construct(name, args) -> VConstruct(name, List.map (eval env) args)
    | Record fields -> VRecord(fields |> List.map (fun (n, e) -> n, eval env e) |> Map.ofList)
    | Field(record, fieldName) ->
        match eval env record with
        | VRecord fields ->
            match Map.tryFind fieldName fields with
            | Some fv -> fv
            | None -> raise (RuntimeError(sprintf "record has no field '%s'" fieldName))
        | _ -> raise (RuntimeError "field access on a non-record value")
    | Match(scrut, cases) ->
        let scrutValue = eval env scrut
        let rec tryCases cs =
            match cs with
            | [] -> raise (RuntimeError "match failure: no pattern matched")
            | (pat, body) :: rest ->
                match matchPattern pat scrutValue with
                | Some binds -> eval (List.fold (fun e (n, bv) -> Map.add n bv e) env binds) body
                | None -> tryCases rest
        tryCases cases

and evalLetGroup (env: Env) (isRec: bool) (bindings: (string * Expr) list) : Env =
    if isRec then
        let closures =
            bindings
            |> List.map (fun (name, rhs) ->
                match eval env rhs with
                | VClosure(_, _, closureEnv) as closure -> name, closure, Some closureEnv
                | other -> name, other, None)
        let groupEnv =
            closures |> List.fold (fun e (name, closure, _) -> Map.add name closure e) env
        for (_, _, closureEnvOpt) in closures do
            match closureEnvOpt with
            | Some closureEnv -> closureEnv.Value <- groupEnv
            | None -> ()
        groupEnv
    else
        let evaluated = bindings |> List.map (fun (name, rhs) -> name, eval env rhs)
        evaluated |> List.fold (fun e (name, v) -> Map.add name v e) env

and evalBinOp (env: Env) (op: string) (l: Expr) (r: Expr) : Value =
    match op with
    | "&&" ->
        match eval env l with
        | VBool false -> VBool false
        | VBool true -> eval env r
        | _ -> raise (RuntimeError "&& applied to a non-boolean")
    | "||" ->
        match eval env l with
        | VBool true -> VBool true
        | VBool false -> eval env r
        | _ -> raise (RuntimeError "|| applied to a non-boolean")
    | _ ->
        let a = eval env l
        let b = eval env r
        match op, a, b with
        | "+", VInt x, VInt y -> VInt(x + y)
        | "-", VInt x, VInt y -> VInt(x - y)
        | "*", VInt x, VInt y -> VInt(x * y)
        | "/", VInt x, VInt y -> if y = 0 then raise (RuntimeError "division by zero") else VInt(x / y)
        | "mod", VInt x, VInt y -> if y = 0 then raise (RuntimeError "division by zero") else VInt(x % y)
        | "<", VInt x, VInt y -> VBool(x < y)
        | "<=", VInt x, VInt y -> VBool(x <= y)
        | ">", VInt x, VInt y -> VBool(x > y)
        | ">=", VInt x, VInt y -> VBool(x >= y)
        | "=", _, _ -> VBool(valueEquals a b)
        | "<>", _, _ -> VBool(not (valueEquals a b))
        | "^", VString x, VString y -> VString(x + y)
        | _ -> raise (RuntimeError(sprintf "invalid operands for %s" op))

and matchPattern (pat: Pattern) (v: Value) : (string * Value) list option =
    match pat, v with
    | PWildcard, _ -> Some []
    | PVar x, _ -> Some [ (x, v) ]
    | PInt n, VInt m when n = m -> Some []
    | PInt _, _ -> None
    | PBool a, VBool b when a = b -> Some []
    | PBool _, _ -> None
    | PString a, VString b when a = b -> Some []
    | PString _, _ -> None
    | PUnit, VUnit -> Some []
    | PUnit, _ -> None
    | PTuple ps, VTuple vs when List.length ps = List.length vs -> matchList ps vs
    | PTuple _, _ -> None
    | PNil, VList [] -> Some []
    | PNil, _ -> None
    | PCons(headPat, tailPat), VList(x :: xs) ->
        match matchPattern headPat x with
        | None -> None
        | Some headBinds ->
            match matchPattern tailPat (VList xs) with
            | None -> None
            | Some tailBinds -> Some(headBinds @ tailBinds)
    | PCons _, _ -> None
    | PConstruct(name, argPats), VConstruct(tag, argVals) when name = tag && List.length argPats = List.length argVals ->
        matchList argPats argVals
    | PConstruct _, _ -> None
    | PRecord fields, VRecord recordVals ->
        let rec go remaining acc =
            match remaining with
            | [] -> Some acc
            | (fieldName, fieldPat) :: rest ->
                match Map.tryFind fieldName recordVals with
                | Some fv ->
                    match matchPattern fieldPat fv with
                    | Some binds -> go rest (acc @ binds)
                    | None -> None
                | None -> None
        go fields []
    | PRecord _, _ -> None

and matchList (ps: Pattern list) (vs: Value list) : (string * Value) list option =
    match ps, vs with
    | [], [] -> Some []
    | p :: pr, v :: vr ->
        match matchPattern p v with
        | None -> None
        | Some b1 ->
            match matchList pr vr with
            | None -> None
            | Some b2 -> Some(b1 @ b2)
    | _ -> None
