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

let private runFile (backend: Backend) (path: string) : int =
    if not (IO.File.Exists path) then
        reportError "File error" (sprintf "cannot find file '%s'" path)
        1
    else
        let source = IO.File.ReadAllText path
        let session = Session(backend)
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

let private repl (backend: Backend) =
    printfn "fml REPL - enter an expression or a definition, Ctrl-D to quit."
    let session = Session(backend)
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

let parseArgs (argv: string []) : (Backend * string option) option =
    let backend = if Array.contains "--vm" argv then Vm else Interp
    match argv |> Array.filter ((<>) "--vm") with
    | [||] -> Some(backend, None)
    | [| path |] when not (path.StartsWith "--") -> Some(backend, Some path)
    | _ -> None

[<EntryPoint>]
let main argv =
    match parseArgs argv with
    | Some(backend, None) ->
        repl backend
        0
    | Some(backend, Some path) -> runFile backend path
    | None ->
        eprintfn "usage: fml [--vm] [file.fml]"
        1
