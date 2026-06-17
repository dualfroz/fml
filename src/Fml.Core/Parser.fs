module Fml.Core.Parser

open System
open Fml.Core.Ast
open Fml.Core.Lexer

exception ParseError of string

let private isUpper (s: string) = s.Length > 0 && Char.IsUpper s.[0]

let parse (tokenList: Token list) : Phrase list =
    let toks = Array.ofList tokenList
    let mutable pos = 0
    let peek () = toks.[pos]
    let advance () =
        let t = toks.[pos]
        pos <- pos + 1
        t
    let expect tok =
        if toks.[pos] = tok then pos <- pos + 1
        else raise (ParseError(sprintf "expected %A but found %A" tok toks.[pos]))
    let expectIdent () =
        match advance () with
        | TkIdent s -> s
        | t -> raise (ParseError(sprintf "expected an identifier but found %A" t))
    let startsAtom () =
        match peek () with
        | TkInt _ | TkBool _ | TkStr _ | TkIdent _ | TkUnderscore | TkLParen | TkLBracket | TkLBrace -> true
        | _ -> false

    let desugarParams ps body =
        List.foldBack (fun p acc -> Lambda(p, acc)) ps body

    let flattenArg (e: Expr) : Expr list =
        match e with
        | Tuple items -> items
        | _ -> [ e ]

    let makeLet isRec (bindings: (string * Expr) list) body =
        match bindings with
        | [ (name, rhs) ] -> Let(isRec, name, rhs, body)
        | _ -> LetGroup(isRec, bindings, body)

    let rec parseExpr () =
        match peek () with
        | TkLet -> parseLetExpr ()
        | TkFun -> parseFun ()
        | TkIf -> parseIf ()
        | TkMatch -> parseMatch ()
        | _ -> parseOr ()

    and parseParams () =
        let ps = ResizeArray<string>()
        let mutable go = true
        while go do
            match peek () with
            | TkIdent s -> advance () |> ignore; ps.Add s
            | TkUnderscore -> advance () |> ignore; ps.Add "_"
            | _ -> go <- false
        List.ofSeq ps

    and parseBinding () =
        let name = expectIdent ()
        let ps = parseParams ()
        expect TkEquals
        let rhs = parseExpr ()
        name, desugarParams ps rhs

    and parseBindings () =
        let bindings = ResizeArray<string * Expr>()
        bindings.Add(parseBinding ())
        while peek () = TkKwAnd do
            advance () |> ignore
            bindings.Add(parseBinding ())
        List.ofSeq bindings

    and parseLetExpr () =
        expect TkLet
        let isRec = if peek () = TkRec then (advance () |> ignore; true) else false
        let bindings = parseBindings ()
        expect TkIn
        let body = parseExpr ()
        makeLet isRec bindings body

    and parseFun () =
        expect TkFun
        let ps = parseParams ()
        if List.isEmpty ps then raise (ParseError "fun requires at least one parameter")
        expect TkArrow
        let body = parseExpr ()
        desugarParams ps body

    and parseIf () =
        expect TkIf
        let c = parseExpr ()
        expect TkThen
        let t = parseExpr ()
        expect TkElse
        let e = parseExpr ()
        If(c, t, e)

    and parseMatch () =
        expect TkMatch
        let scrut = parseExpr ()
        expect TkWith
        if peek () = TkBar then advance () |> ignore
        let cases = ResizeArray<Pattern * Expr>()
        let firstPat = parsePattern ()
        expect TkArrow
        let firstBody = parseExpr ()
        cases.Add(firstPat, firstBody)
        while peek () = TkBar do
            advance () |> ignore
            let p = parsePattern ()
            expect TkArrow
            let b = parseExpr ()
            cases.Add(p, b)
        Match(scrut, List.ofSeq cases)

    and parseOr () =
        let mutable left = parseAnd ()
        while peek () = TkOr do
            advance () |> ignore
            left <- BinOp("||", left, parseAnd ())
        left

    and parseAnd () =
        let mutable left = parseCompare ()
        while peek () = TkAnd do
            advance () |> ignore
            left <- BinOp("&&", left, parseCompare ())
        left

    and parseCompare () =
        let mutable left = parseCons ()
        let isCmp () =
            match peek () with
            | TkEquals | TkNotEq | TkLt | TkLe | TkGt | TkGe -> true
            | _ -> false
        while isCmp () do
            let op =
                match advance () with
                | TkEquals -> "="
                | TkNotEq -> "<>"
                | TkLt -> "<"
                | TkLe -> "<="
                | TkGt -> ">"
                | TkGe -> ">="
                | _ -> ""
            left <- BinOp(op, left, parseCons ())
        left

    and parseCons () =
        let left = parseAdd ()
        if peek () = TkCons then
            advance () |> ignore
            Cons(left, parseCons ())
        else left

    and parseAdd () =
        let mutable left = parseMul ()
        let isOp () =
            match peek () with
            | TkPlus | TkMinus | TkCaret -> true
            | _ -> false
        while isOp () do
            let op =
                match advance () with
                | TkPlus -> "+"
                | TkMinus -> "-"
                | TkCaret -> "^"
                | _ -> ""
            left <- BinOp(op, left, parseMul ())
        left

    and parseMul () =
        let mutable left = parseUnary ()
        let isOp () =
            match peek () with
            | TkStar | TkSlash | TkMod -> true
            | _ -> false
        while isOp () do
            let op =
                match advance () with
                | TkStar -> "*"
                | TkSlash -> "/"
                | TkMod -> "mod"
                | _ -> ""
            left <- BinOp(op, left, parseUnary ())
        left

    and parseUnary () =
        if peek () = TkMinus then
            advance () |> ignore
            Neg(parseUnary ())
        else
            parseApp ()

    and parseApp () =
        let head = parsePostfix ()
        match head with
        | Construct(name, []) when isUpper name ->
            if startsAtom () then Construct(name, flattenArg (parsePostfix ()))
            else head
        | _ ->
            let mutable f = head
            while startsAtom () do
                f <- App(f, parsePostfix ())
            f

    and parsePostfix () =
        let mutable base' = parseAtom ()
        while peek () = TkDot do
            advance () |> ignore
            let name = expectIdent ()
            base' <-
                match base' with
                | Construct(modName, []) when isUpper modName -> Var(modName + "." + name)
                | _ -> Field(base', name)
        base'

    and parseAtom () =
        match advance () with
        | TkInt n -> IntLit n
        | TkBool b -> BoolLit b
        | TkStr s -> StringLit s
        | TkIdent s -> if isUpper s then Construct(s, []) else Var s
        | TkUnderscore -> Var "_"
        | TkLBrace -> parseRecordLiteral ()
        | TkLParen ->
            if peek () = TkRParen then
                advance () |> ignore
                UnitLit
            else
                let first = parseExpr ()
                if peek () = TkComma then
                    let items = ResizeArray<Expr>()
                    items.Add first
                    while peek () = TkComma do
                        advance () |> ignore
                        items.Add(parseExpr ())
                    expect TkRParen
                    Tuple(List.ofSeq items)
                else
                    expect TkRParen
                    first
        | TkLBracket ->
            if peek () = TkRBracket then
                advance () |> ignore
                Nil
            else
                let items = ResizeArray<Expr>()
                items.Add(parseExpr ())
                while peek () = TkSemi do
                    advance () |> ignore
                    items.Add(parseExpr ())
                expect TkRBracket
                List.foldBack (fun e acc -> Cons(e, acc)) (List.ofSeq items) Nil
        | t -> raise (ParseError(sprintf "unexpected token %A" t))

    and parseRecordLiteral () =
        let fields = ResizeArray<string * Expr>()
        let parseField () =
            let name = expectIdent ()
            expect TkEquals
            let value = parseExpr ()
            fields.Add(name, value)
        parseField ()
        while peek () = TkSemi do
            advance () |> ignore
            if peek () <> TkRBrace then parseField ()
        expect TkRBrace
        Record(List.ofSeq fields)

    and parsePattern () = parsePatCons ()

    and parsePatCons () =
        let left = parsePatApp ()
        if peek () = TkCons then
            advance () |> ignore
            PCons(left, parsePatCons ())
        else left

    and startsPatAtom () =
        match peek () with
        | TkInt _ | TkBool _ | TkStr _ | TkIdent _ | TkUnderscore | TkLParen | TkLBracket | TkLBrace -> true
        | _ -> false

    and parsePatApp () =
        match peek () with
        | TkIdent s when isUpper s ->
            advance () |> ignore
            if startsPatAtom () then
                let arg = parsePatAtom ()
                let args =
                    match arg with
                    | PTuple ps -> ps
                    | _ -> [ arg ]
                PConstruct(s, args)
            else PConstruct(s, [])
        | _ -> parsePatAtom ()

    and parsePatAtom () =
        match advance () with
        | TkInt n -> PInt n
        | TkBool b -> PBool b
        | TkStr s -> PString s
        | TkUnderscore -> PWildcard
        | TkIdent s -> if isUpper s then PConstruct(s, []) else PVar s
        | TkLBrace -> parseRecordPattern ()
        | TkLParen ->
            if peek () = TkRParen then
                advance () |> ignore
                PUnit
            else
                let first = parsePattern ()
                if peek () = TkComma then
                    let items = ResizeArray<Pattern>()
                    items.Add first
                    while peek () = TkComma do
                        advance () |> ignore
                        items.Add(parsePattern ())
                    expect TkRParen
                    PTuple(List.ofSeq items)
                else
                    expect TkRParen
                    first
        | TkLBracket ->
            if peek () = TkRBracket then
                advance () |> ignore
                PNil
            else
                let items = ResizeArray<Pattern>()
                items.Add(parsePattern ())
                while peek () = TkSemi do
                    advance () |> ignore
                    items.Add(parsePattern ())
                expect TkRBracket
                List.foldBack (fun p acc -> PCons(p, acc)) (List.ofSeq items) PNil
        | t -> raise (ParseError(sprintf "unexpected token in pattern %A" t))

    and parseRecordPattern () =
        let fields = ResizeArray<string * Pattern>()
        let parseField () =
            let name = expectIdent ()
            expect TkEquals
            let p = parsePattern ()
            fields.Add(name, p)
        parseField ()
        while peek () = TkSemi do
            advance () |> ignore
            if peek () <> TkRBrace then parseField ()
        expect TkRBrace
        PRecord(List.ofSeq fields)

    and parseTyExpr () = parseTyArrow ()

    and parseTyArrow () =
        let left = parseTyTuple ()
        if peek () = TkArrow then
            advance () |> ignore
            TEArrow(left, parseTyArrow ())
        else left

    and parseTyTuple () =
        let first = parseTyApp ()
        if peek () = TkStar then
            let parts = ResizeArray<TyExpr>()
            parts.Add first
            while peek () = TkStar do
                advance () |> ignore
                parts.Add(parseTyApp ())
            TETuple(List.ofSeq parts)
        else first

    and parseTyApp () =
        let mutable base' = parseTyAtom ()
        let mutable go = true
        while go do
            match peek () with
            | TkIdent name when not (isUpper name) ->
                advance () |> ignore
                base' <- TECon(name, [ base' ])
            | _ -> go <- false
        base'

    and parseTyAtom () =
        match advance () with
        | TkTyVar a -> TEVar a
        | TkIdent name -> TECon(name, [])
        | TkLParen ->
            let first = parseTyExpr ()
            if peek () = TkComma then
                let args = ResizeArray<TyExpr>()
                args.Add first
                while peek () = TkComma do
                    advance () |> ignore
                    args.Add(parseTyExpr ())
                expect TkRParen
                let conName = expectIdent ()
                TECon(conName, List.ofSeq args)
            else
                expect TkRParen
                first
        | t -> raise (ParseError(sprintf "unexpected token in type expression %A" t))

    and parseTypeParams () =
        match peek () with
        | TkTyVar a -> advance () |> ignore; [ a ]
        | TkLParen ->
            advance () |> ignore
            let ps = ResizeArray<string>()
            let readVar () =
                match advance () with
                | TkTyVar a -> ps.Add a
                | t -> raise (ParseError(sprintf "expected a type variable but found %A" t))
            readVar ()
            while peek () = TkComma do
                advance () |> ignore
                readVar ()
            expect TkRParen
            List.ofSeq ps
        | _ -> []

    and parseTypeDecl () =
        let ps = parseTypeParams ()
        let name = expectIdent ()
        expect TkEquals
        let body =
            if peek () = TkLBrace then parseRecordDef ()
            else parseVariants ()
        { Name = name; Params = ps; Body = body }

    and parseRecordDef () =
        expect TkLBrace
        let fields = ResizeArray<string * TyExpr>()
        let parseField () =
            let name = expectIdent ()
            expect TkColon
            let ty = parseTyExpr ()
            fields.Add(name, ty)
        parseField ()
        while peek () = TkSemi do
            advance () |> ignore
            if peek () <> TkRBrace then parseField ()
        expect TkRBrace
        RecordDef(List.ofSeq fields)

    and parseVariants () =
        if peek () = TkBar then advance () |> ignore
        let variants = ResizeArray<string * TyExpr list>()
        let parseVariant () =
            let ctorName = expectIdent ()
            if not (isUpper ctorName) then
                raise (ParseError(sprintf "constructor name must start uppercase: %s" ctorName))
            let argTypes =
                if peek () = TkOf then
                    advance () |> ignore
                    match parseTyExpr () with
                    | TETuple parts -> parts
                    | single -> [ single ]
                else []
            variants.Add(ctorName, argTypes)
        parseVariant ()
        while peek () = TkBar do
            advance () |> ignore
            parseVariant ()
        Variants(List.ofSeq variants)

    let rec parsePhrase () =
        match peek () with
        | TkLet ->
            expect TkLet
            let isRec = if peek () = TkRec then (advance () |> ignore; true) else false
            let bindings = parseBindings ()
            if peek () = TkIn then
                advance () |> ignore
                let body = parseExpr ()
                PhraseExpr(makeLet isRec bindings body)
            else
                PhraseLet(isRec, bindings)
        | TkType ->
            expect TkType
            let decls = ResizeArray<TypeDecl>()
            decls.Add(parseTypeDecl ())
            while peek () = TkKwAnd do
                advance () |> ignore
                decls.Add(parseTypeDecl ())
            PhraseType(List.ofSeq decls)
        | TkModule ->
            expect TkModule
            let name = expectIdent ()
            expect TkEquals
            expect TkStruct
            let inner = ResizeArray<Phrase>()
            while peek () <> TkEnd && peek () <> TkEOF do
                inner.Add(parsePhrase ())
                while peek () = TkDoubleSemi do advance () |> ignore
            expect TkEnd
            PhraseModule(name, List.ofSeq inner)
        | TkOpen ->
            expect TkOpen
            let name = expectIdent ()
            PhraseOpen name
        | _ -> PhraseExpr(parseExpr ())

    let phrases = ResizeArray<Phrase>()
    if peek () <> TkEOF then
        phrases.Add(parsePhrase ())
        while peek () = TkDoubleSemi do
            advance () |> ignore
            if peek () <> TkEOF then phrases.Add(parsePhrase ())
    if peek () <> TkEOF then
        raise (ParseError(sprintf "unexpected token %A after phrase" (peek ())))
    List.ofSeq phrases
