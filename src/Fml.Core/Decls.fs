module Fml.Core.Decls

open Fml.Core.Ast
open Fml.Core.Types

type CtorInfo =
    { Tag: string
      TypeName: string
      Params: string list
      ArgTypes: TyExpr list }

type FieldInfo =
    { Name: string
      TypeName: string
      Params: string list
      FieldType: TyExpr
      AllFields: string list }

type TypeInfo =
    { Name: string
      Params: string list
      Ctors: string list
      Fields: string list
      IsRecord: bool }

let mutable typeInfos: Map<string, TypeInfo> = Map.empty
let mutable ctorInfos: Map<string, CtorInfo> = Map.empty
let mutable fieldInfos: Map<string, FieldInfo> = Map.empty

let resetDecls () =
    typeInfos <- Map.empty
    ctorInfos <- Map.empty
    fieldInfos <- Map.empty

let lookupCtor (name: string) : CtorInfo option = Map.tryFind name ctorInfos
let lookupField (name: string) : FieldInfo option = Map.tryFind name fieldInfos
let lookupType (name: string) : TypeInfo option = Map.tryFind name typeInfos
let ctorArity (name: string) : int =
    match Map.tryFind name ctorInfos with
    | Some info -> List.length info.ArgTypes
    | None -> raise (TypeError(sprintf "unknown constructor '%s'" name))

let registerDecl (decl: TypeDecl) =
    match decl.Body with
    | Variants variants ->
        let tags = variants |> List.map fst
        typeInfos <-
            Map.add decl.Name
                { Name = decl.Name
                  Params = decl.Params
                  Ctors = tags
                  Fields = []
                  IsRecord = false }
                typeInfos
        for (tag, argTypes) in variants do
            ctorInfos <-
                Map.add tag
                    { Tag = tag
                      TypeName = decl.Name
                      Params = decl.Params
                      ArgTypes = argTypes }
                    ctorInfos
    | RecordDef fields ->
        let fieldNames = fields |> List.map fst
        typeInfos <-
            Map.add decl.Name
                { Name = decl.Name
                  Params = decl.Params
                  Ctors = []
                  Fields = fieldNames
                  IsRecord = true }
                typeInfos
        for (fieldName, fieldTy) in fields do
            fieldInfos <-
                Map.add fieldName
                    { Name = fieldName
                      TypeName = decl.Name
                      Params = decl.Params
                      FieldType = fieldTy
                      AllFields = fieldNames }
                    fieldInfos

let rec tyExprToTy (subst: Map<string, Ty>) (te: TyExpr) : Ty =
    match te with
    | TEVar name ->
        match Map.tryFind name subst with
        | Some ty -> ty
        | None -> raise (TypeError(sprintf "unbound type variable '%s" name))
    | TEList inner -> TList(tyExprToTy subst inner)
    | TETuple parts -> TTuple(List.map (tyExprToTy subst) parts)
    | TEArrow(a, b) -> TArrow(tyExprToTy subst a, tyExprToTy subst b)
    | TECon(name, args) ->
        match name, args with
        | "int", [] -> TInt
        | "bool", [] -> TBool
        | "string", [] -> TString
        | "unit", [] -> TUnit
        | "list", [ single ] -> TList(tyExprToTy subst single)
        | _ -> TCon(name, List.map (tyExprToTy subst) args)

let instantiateCtor (info: CtorInfo) : Ty list * Ty =
    let subst = info.Params |> List.map (fun p -> p, newVar ()) |> Map.ofList
    let argTys = info.ArgTypes |> List.map (tyExprToTy subst)
    let resultTy = TCon(info.TypeName, info.Params |> List.map (fun p -> subst.[p]))
    argTys, resultTy

let instantiateField (info: FieldInfo) : Ty * Ty =
    let subst = info.Params |> List.map (fun p -> p, newVar ()) |> Map.ofList
    let fieldTy = tyExprToTy subst info.FieldType
    let recordTy = TCon(info.TypeName, info.Params |> List.map (fun p -> subst.[p]))
    fieldTy, recordTy

let instantiateRecordType (typeName: string) : Map<string, Ty> * Ty =
    match Map.tryFind typeName typeInfos with
    | Some info when info.IsRecord ->
        let subst = info.Params |> List.map (fun p -> p, newVar ()) |> Map.ofList
        let fieldTys =
            info.Fields
            |> List.map (fun fieldName ->
                let fieldInfo = fieldInfos.[fieldName]
                fieldName, tyExprToTy subst fieldInfo.FieldType)
            |> Map.ofList
        let recordTy = TCon(typeName, info.Params |> List.map (fun p -> subst.[p]))
        fieldTys, recordTy
    | _ -> raise (TypeError(sprintf "'%s' is not a record type" typeName))
