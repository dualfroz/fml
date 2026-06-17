module Fml.Core.Lexer

open System

type Token =
    | TkInt of int
    | TkBool of bool
    | TkStr of string
    | TkIdent of string
    | TkTyVar of string
    | TkLet | TkRec | TkIn | TkFun | TkIf | TkThen | TkElse | TkMatch | TkWith
    | TkType | TkOf | TkKwAnd | TkModule | TkStruct | TkEnd | TkOpen
    | TkArrow | TkEquals | TkNotEq | TkLt | TkLe | TkGt | TkGe
    | TkPlus | TkMinus | TkStar | TkSlash | TkMod | TkCaret
    | TkAnd | TkOr
    | TkLParen | TkRParen | TkLBracket | TkRBracket | TkLBrace | TkRBrace
    | TkComma | TkSemi | TkDoubleSemi | TkCons | TkBar | TkUnderscore
    | TkDot | TkColon
    | TkEOF

exception LexError of string

let tokenize (src: string) : Token list =
    let n = src.Length
    let tokens = ResizeArray<Token>()
    let mutable i = 0
    let peek k = if i + k < n then src.[i + k] else '\000'
    let isIdentStart c = Char.IsLetter c || c = '_'
    let isIdentCont c = Char.IsLetterOrDigit c || c = '_' || c = '\''
    while i < n do
        let c = src.[i]
        if c = ' ' || c = '\t' || c = '\r' || c = '\n' then
            i <- i + 1
        elif c = '(' && peek 1 = '*' then
            let mutable depth = 1
            i <- i + 2
            while i < n && depth > 0 do
                if i + 1 < n && src.[i] = '(' && src.[i + 1] = '*' then depth <- depth + 1; i <- i + 2
                elif i + 1 < n && src.[i] = '*' && src.[i + 1] = ')' then depth <- depth - 1; i <- i + 2
                else i <- i + 1
            if depth > 0 then raise (LexError "unterminated block comment")
        elif c = '"' then
            let sb = Text.StringBuilder()
            i <- i + 1
            let mutable closed = false
            while i < n && not closed do
                let ch = src.[i]
                if ch = '"' then
                    closed <- true
                    i <- i + 1
                elif ch = '\\' then
                    let e = if i + 1 < n then src.[i + 1] else '\000'
                    let r =
                        match e with
                        | 'n' -> '\n'
                        | 't' -> '\t'
                        | 'r' -> '\r'
                        | '\\' -> '\\'
                        | '"' -> '"'
                        | _ -> raise (LexError (sprintf "invalid escape sequence in string"))
                    sb.Append(r) |> ignore
                    i <- i + 2
                else
                    sb.Append(ch) |> ignore
                    i <- i + 1
            if not closed then raise (LexError "unterminated string literal")
            tokens.Add(TkStr(sb.ToString()))
        elif c = '\'' && isIdentStart (peek 1) then
            i <- i + 1
            let start = i
            while i < n && isIdentCont src.[i] do i <- i + 1
            tokens.Add(TkTyVar(src.Substring(start, i - start)))
        elif Char.IsDigit c then
            let start = i
            while i < n && Char.IsDigit src.[i] do i <- i + 1
            tokens.Add(TkInt(Int32.Parse(src.Substring(start, i - start))))
        elif isIdentStart c then
            let start = i
            while i < n && isIdentCont src.[i] do i <- i + 1
            let word = src.Substring(start, i - start)
            let tok =
                match word with
                | "let" -> TkLet
                | "rec" -> TkRec
                | "in" -> TkIn
                | "fun" -> TkFun
                | "if" -> TkIf
                | "then" -> TkThen
                | "else" -> TkElse
                | "match" -> TkMatch
                | "with" -> TkWith
                | "type" -> TkType
                | "of" -> TkOf
                | "and" -> TkKwAnd
                | "module" -> TkModule
                | "struct" -> TkStruct
                | "end" -> TkEnd
                | "open" -> TkOpen
                | "mod" -> TkMod
                | "true" -> TkBool true
                | "false" -> TkBool false
                | "_" -> TkUnderscore
                | _ -> TkIdent word
            tokens.Add tok
        else
            let two = if i + 1 < n then src.Substring(i, 2) else ""
            match two with
            | "->" -> tokens.Add TkArrow; i <- i + 2
            | "<>" -> tokens.Add TkNotEq; i <- i + 2
            | "<=" -> tokens.Add TkLe; i <- i + 2
            | ">=" -> tokens.Add TkGe; i <- i + 2
            | "::" -> tokens.Add TkCons; i <- i + 2
            | "&&" -> tokens.Add TkAnd; i <- i + 2
            | "||" -> tokens.Add TkOr; i <- i + 2
            | ";;" -> tokens.Add TkDoubleSemi; i <- i + 2
            | _ ->
                match c with
                | '=' -> tokens.Add TkEquals; i <- i + 1
                | '<' -> tokens.Add TkLt; i <- i + 1
                | '>' -> tokens.Add TkGt; i <- i + 1
                | '+' -> tokens.Add TkPlus; i <- i + 1
                | '-' -> tokens.Add TkMinus; i <- i + 1
                | '*' -> tokens.Add TkStar; i <- i + 1
                | '/' -> tokens.Add TkSlash; i <- i + 1
                | '^' -> tokens.Add TkCaret; i <- i + 1
                | '(' -> tokens.Add TkLParen; i <- i + 1
                | ')' -> tokens.Add TkRParen; i <- i + 1
                | '[' -> tokens.Add TkLBracket; i <- i + 1
                | ']' -> tokens.Add TkRBracket; i <- i + 1
                | '{' -> tokens.Add TkLBrace; i <- i + 1
                | '}' -> tokens.Add TkRBrace; i <- i + 1
                | ',' -> tokens.Add TkComma; i <- i + 1
                | ';' -> tokens.Add TkSemi; i <- i + 1
                | '|' -> tokens.Add TkBar; i <- i + 1
                | '.' -> tokens.Add TkDot; i <- i + 1
                | ':' -> tokens.Add TkColon; i <- i + 1
                | _ -> raise (LexError (sprintf "unexpected character '%c'" c))
    tokens.Add TkEOF
    List.ofSeq tokens
