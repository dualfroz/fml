module Fml.Cli.Program

open System
open Fml.Core.Lexer
open Fml.Core.Parser
open Fml.Core.Types
open Fml.Core.Match
open Fml.Core.Value
open Fml.Core.Interpreter

let private printResult (r: PhraseResult) =
    printfn "val %s : %s = %s" r.Name r.TypeText r.ValueText

let private reportError (kind: string) (message: string) = eprintfn "%s: %s" kind message

let private runFile (path: string) : int =
    let source = IO.File.ReadAllText path
    let session = Session()
    session.LoadPrelude()
    try
        for phrase in parsePhrases source do
            session.RunPhrase phrase |> List.iter printResult
        0
    with
    | LexError m -> reportError "Lex error" m; 1
    | ParseError m -> reportError "Parse error" m; 1
    | TypeError m -> reportError "Type error" m; 1
    | MatchError m -> reportError "Match error" m; 1
    | RuntimeError m -> reportError "Runtime error" m; 1

let private repl () =
    printfn "fml REPL - enter an expression or a definition, Ctrl-D to quit."
    let session = Session()
    session.LoadPrelude()
    let mutable running = true
    while running do
        Console.Out.Write "fml> "
        Console.Out.Flush()
        let line = Console.In.ReadLine()
        if isNull line then
            running <- false
        elif line.Trim() = "" then
            ()
        else
            try
                for phrase in parsePhrases line do
                    session.RunPhrase phrase |> List.iter printResult
            with
            | LexError m -> printfn "Lex error: %s" m
            | ParseError m -> printfn "Parse error: %s" m
            | TypeError m -> printfn "Type error: %s" m
            | MatchError m -> printfn "Match error: %s" m
            | RuntimeError m -> printfn "Runtime error: %s" m
    printfn "bye"

[<EntryPoint>]
let main argv =
    match argv with
    | [||] ->
        repl ()
        0
    | [| path |] -> runFile path
    | _ ->
        eprintfn "usage: fml [file.fml]"
        1
