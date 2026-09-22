module Fml.Core.Infer

open Fml.Core.Ast
open Fml.Core.Types
open Fml.Core.Decls
open Fml.Core.Match

type TypeEnv = Map<string, Ty>

let private ctorFieldType (name: string) : Ty list * Ty =
    match lookupCtor name with
    | Some info -> instantiateCtor info
    | None -> raise (TypeError(sprintf "unknown constructor '%s'" name))

let private recordTypeOfField (fieldName: string) : string =
    match lookupField fieldName with
    | Some info -> info.TypeName
    | None -> raise (TypeError(sprintf "unknown record field '%s'" fieldName))

let private requireRecFunction (rhs: Expr) =
    match rhs with
    | Lambda _ -> ()
    | _ -> raise (TypeError "let rec binding must be a function")

let rec inferPattern (pat: Pattern) : Ty * (string * Ty) list =
    match pat with
    | PWildcard -> newVar (), []
    | PVar x ->
        let v = newVar ()
        v, [ (x, v) ]
    | PInt _ -> TInt, []
    | PBool _ -> TBool, []
    | PString _ -> TString, []
    | PUnit -> TUnit, []
    | PTuple parts ->
        let inferred = parts |> List.map inferPattern
        TTuple(List.map fst inferred), List.collect snd inferred
    | PNil -> TList(newVar ()), []
    | PCons(headPat, tailPat) ->
        let headTy, headBinds = inferPattern headPat
        let tailTy, tailBinds = inferPattern tailPat
        unify tailTy (TList headTy)
        TList headTy, headBinds @ tailBinds
    | PConstruct(name, argPats) ->
        let argTys, resultTy = ctorFieldType name
        if List.length argTys <> List.length argPats then
            raise (TypeError(sprintf "constructor '%s' expects %d argument(s)" name (List.length argTys)))
        let inferred = argPats |> List.map inferPattern
        List.iter2 (fun expected (actual, _) -> unify expected actual) argTys inferred
        resultTy, List.collect snd inferred
    | PRecord fields ->
        match fields with
        | [] -> raise (TypeError "record pattern must mention at least one field")
        | (firstName, _) :: _ ->
            let typeName = recordTypeOfField firstName
            let fieldTys, recordTy = instantiateRecordType typeName
            let binds =
                fields
                |> List.collect (fun (fieldName, fieldPat) ->
                    match Map.tryFind fieldName fieldTys with
                    | Some expected ->
                        let actual, subBinds = inferPattern fieldPat
                        unify expected actual
                        subBinds
                    | None -> raise (TypeError(sprintf "record type '%s' has no field '%s'" typeName fieldName)))
            recordTy, binds

let rec infer (env: TypeEnv) (expr: Expr) : Ty =
    match expr with
    | IntLit _ -> TInt
    | BoolLit _ -> TBool
    | StringLit _ -> TString
    | UnitLit -> TUnit
    | Var x ->
        match Map.tryFind x env with
        | Some scheme -> instantiate scheme
        | None -> raise (TypeError(sprintf "unbound variable '%s'" x))
    | Lambda(x, body) ->
        let paramTy = newVar ()
        let bodyTy = infer (Map.add x paramTy env) body
        TArrow(paramTy, bodyTy)
    | App(fn, arg) ->
        let fnTy = infer env fn
        let argTy = infer env arg
        let resultTy = newVar ()
        unify fnTy (TArrow(argTy, resultTy))
        resultTy
    | Let(false, name, rhs, body) ->
        enterLevel ()
        let rhsTy = infer env rhs
        leaveLevel ()
        generalize rhsTy
        infer (Map.add name rhsTy env) body
    | Let(true, name, rhs, body) ->
        requireRecFunction rhs
        enterLevel ()
        let nameTy = newVar ()
        let rhsTy = infer (Map.add name nameTy env) rhs
        unify nameTy rhsTy
        leaveLevel ()
        generalize nameTy
        infer (Map.add name nameTy env) body
    | LetGroup(isRec, bindings, body) ->
        let groupEnv = inferLetGroup env isRec bindings
        infer groupEnv body
    | If(cond, thenBranch, elseBranch) ->
        unify (infer env cond) TBool
        let thenTy = infer env thenBranch
        let elseTy = infer env elseBranch
        unify thenTy elseTy
        thenTy
    | Neg inner ->
        unify (infer env inner) TInt
        TInt
    | BinOp(op, l, r) ->
        let lt = infer env l
        let rt = infer env r
        match op with
        | "+" | "-" | "*" | "/" | "mod" ->
            unify lt TInt
            unify rt TInt
            TInt
        | "<" | "<=" | ">" | ">=" ->
            unify lt TInt
            unify rt TInt
            TBool
        | "=" | "<>" ->
            unify lt rt
            TBool
        | "&&" | "||" ->
            unify lt TBool
            unify rt TBool
            TBool
        | "^" ->
            unify lt TString
            unify rt TString
            TString
        | _ -> raise (TypeError(sprintf "unknown operator %s" op))
    | Tuple items -> TTuple(List.map (infer env) items)
    | Nil -> TList(newVar ())
    | Cons(headExpr, tailExpr) ->
        let headTy = infer env headExpr
        let tailTy = infer env tailExpr
        unify tailTy (TList headTy)
        TList headTy
    | Construct(name, args) ->
        let argTys, resultTy = ctorFieldType name
        if List.length argTys <> List.length args then
            raise (TypeError(sprintf "constructor '%s' expects %d argument(s)" name (List.length argTys)))
        List.iter2 (fun expected actual -> unify expected (infer env actual)) argTys args
        resultTy
    | Record fields ->
        match fields with
        | [] -> raise (TypeError "record expression must have at least one field")
        | (firstName, _) :: _ ->
            let typeName = recordTypeOfField firstName
            let fieldTys, recordTy = instantiateRecordType typeName
            let provided = fields |> List.map fst |> Set.ofList
            let declared = fieldTys |> Map.toList |> List.map fst |> Set.ofList
            if provided <> declared then
                raise (TypeError(sprintf "record of type '%s' must set exactly its fields" typeName))
            for (fieldName, fieldExpr) in fields do
                unify fieldTys.[fieldName] (infer env fieldExpr)
            recordTy
    | Field(record, fieldName) ->
        let fieldTy, recordTy =
            match lookupField fieldName with
            | Some info -> instantiateField info
            | None -> raise (TypeError(sprintf "unknown record field '%s'" fieldName))
        unify (infer env record) recordTy
        fieldTy
    | Match(scrut, cases) ->
        let scrutTy = infer env scrut
        let resultTy = newVar ()
        for (pat, body) in cases do
            let patTy, binds = inferPattern pat
            unify scrutTy patTy
            let branchEnv = List.fold (fun e (n, ty) -> Map.add n ty e) env binds
            unify resultTy (infer branchEnv body)
        checkMatch (cases |> List.map fst)
        resultTy

and inferLetGroup (env: TypeEnv) (isRec: bool) (bindings: (string * Expr) list) : TypeEnv =
    if isRec then
        bindings |> List.iter (snd >> requireRecFunction)
        enterLevel ()
        let placeholders = bindings |> List.map (fun (name, _) -> name, newVar ())
        let recEnv = List.fold (fun e (n, ty) -> Map.add n ty e) env placeholders
        for ((_, rhs), (_, nameTy)) in List.zip bindings placeholders do
            unify nameTy (infer recEnv rhs)
        leaveLevel ()
        placeholders |> List.iter (fun (_, ty) -> generalize ty)
        List.fold (fun e (n, ty) -> Map.add n ty e) env placeholders
    else
        let typed =
            bindings
            |> List.map (fun (name, rhs) ->
                enterLevel ()
                let ty = infer env rhs
                leaveLevel ()
                generalize ty
                name, ty)
        List.fold (fun e (n, ty) -> Map.add n ty e) env typed
