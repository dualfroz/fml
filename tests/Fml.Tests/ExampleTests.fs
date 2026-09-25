module Fml.Tests.ExampleTests

open System
open System.IO
open Xunit
open Fml.Core.Lexer
open Fml.Core.Parser
open Fml.Core.Types
open Fml.Core.Match
open Fml.Core.Value
open Fml.Core.Interpreter

let private examplesDir = Path.Combine(AppContext.BaseDirectory, "examples")

let private transcript (backend: Backend) (source: string) : string =
    let output = new StringWriter()
    output.NewLine <- "\n"
    let previousOut = Console.Out
    Console.SetOut output
    try
        let session = Session(backend)
        session.LoadPrelude()
        try
            for phrase in parsePhrases source do
                for phraseResult in session.RunPhrase phrase do
                    output.WriteLine(sprintf "val %s : %s = %s" phraseResult.Name phraseResult.TypeText phraseResult.ValueText)
        with
        | LexError m -> output.WriteLine("Lex error: " + m)
        | ParseError m -> output.WriteLine("Parse error: " + m)
        | TypeError m -> output.WriteLine("Type error: " + m)
        | MatchError m -> output.WriteLine("Match error: " + m)
        | RuntimeError m -> output.WriteLine("Runtime error: " + m)
    finally
        Console.SetOut previousOut
    output.ToString()

let exampleNames () : seq<obj []> =
    Directory.GetFiles(examplesDir, "*.fml")
    |> Array.sort
    |> Seq.map (fun path -> [| box (Path.GetFileNameWithoutExtension path) |])

[<Theory>]
[<MemberData(nameof exampleNames)>]
let ``example prints its documented output on both engines`` (name: string) =
    let source = File.ReadAllText(Path.Combine(examplesDir, name + ".fml"))
    let expected = File.ReadAllText(Path.Combine(examplesDir, name + ".out")).Replace("\r\n", "\n")
    let fromInterp = transcript Interp source
    let fromVm = transcript Vm source
    Assert.Equal(expected, fromInterp)
    Assert.Equal(fromInterp, fromVm)

[<Fact>]
let ``cli selects the backend from the vm flag`` () =
    Assert.Equal(Some(Interp, Some "a.fml"), Fml.Cli.Program.parseArgs [| "a.fml" |])
    Assert.Equal(Some(Vm, Some "a.fml"), Fml.Cli.Program.parseArgs [| "--vm"; "a.fml" |])
    Assert.Equal(Some(Vm, Some "a.fml"), Fml.Cli.Program.parseArgs [| "a.fml"; "--vm" |])
    Assert.Equal(Some(Vm, None), Fml.Cli.Program.parseArgs [| "--vm" |])
    Assert.Equal(None, Fml.Cli.Program.parseArgs [| "--vmm"; "a.fml" |])
    Assert.Equal(None, Fml.Cli.Program.parseArgs [| "a.fml"; "b.fml" |])

[<Fact>]
let ``cli runs a file on the vm`` () =
    let path = Path.Combine(examplesDir, "lists.fml")
    let expected = File.ReadAllText(Path.Combine(examplesDir, "lists.out")).Replace("\r\n", "\n")
    let output = new StringWriter()
    output.NewLine <- "\n"
    let previousOut = Console.Out
    Console.SetOut output
    let exitCode =
        try
            Fml.Cli.Program.main [| "--vm"; path |]
        finally
            Console.SetOut previousOut
    Assert.Equal(0, exitCode)
    Assert.Equal(expected, output.ToString())

[<Fact>]
let ``cli reports a missing input file and exits with 1`` () =
    let path = Path.Combine(examplesDir, "missing.fml")
    let errors = new StringWriter()
    errors.NewLine <- "\n"
    let previousError = Console.Error
    Console.SetError errors
    let exitCode =
        try
            Fml.Cli.Program.main [| path |]
        finally
            Console.SetError previousError
    Assert.Equal(1, exitCode)
    Assert.Equal(sprintf "File error: cannot find file '%s'\n" path, errors.ToString())
