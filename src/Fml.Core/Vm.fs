module Fml.Core.Vm

open System.Collections.Generic
open Fml.Core.Value
open Fml.Core.Bytecode

let private projectIndex (i: int) (v: Value) : Value =
    match v with
    | VTuple xs -> List.item i xs
    | VList(h :: t) -> if i = 0 then h else VList t
    | VConstruct(_, args) -> List.item i args
    | VRecord fields -> (Map.toList fields |> List.item i) |> snd
    | _ -> raise (RuntimeError "invalid projection during pattern match")

let private prim (op: string) (a: Value) (b: Value) : Value =
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

type private Frame =
    { Code: Instr []
      Pc: int
      Env: Value list }

type Machine(functions: ResizeArray<Instr []>) =

    member _.Run(entry: Instr []) : Value =
        let stack = Stack<Value>()
        let frames = Stack<Frame>()
        let mutable code = entry
        let mutable pc = 0
        let mutable env: Value list = []
        let mutable result = VUnit
        let mutable running = true

        let popN n =
            let arr = Array.zeroCreate n
            for i in n - 1 .. -1 .. 0 do
                arr.[i] <- stack.Pop()
            List.ofArray arr

        while running do
            let instr = code.[pc]
            pc <- pc + 1
            match instr with
            | Push v -> stack.Push v
            | LoadLocal i -> stack.Push(List.item i env)
            | LoadGlobal cell -> stack.Push cell.Value
            | MakeClosure id -> stack.Push(VCode(id, ref env))
            | MakeRec ids ->
                let shared = ref env
                let closures = ids |> List.map (fun id -> VCode(id, shared))
                let newEnv = closures @ env
                shared.Value <- newEnv
                env <- newEnv
            | Bind -> env <- stack.Pop() :: env
            | PopEnv n -> env <- List.skip n env
            | Pop -> stack.Pop() |> ignore
            | Call ->
                let arg = stack.Pop()
                let fn = stack.Pop()
                match fn with
                | VBuiltin(_, f) -> stack.Push(f arg)
                | VCode(id, capturedEnv) ->
                    frames.Push { Code = code; Pc = pc; Env = env }
                    code <- functions.[id]
                    pc <- 0
                    env <- arg :: capturedEnv.Value
                | _ -> raise (RuntimeError "attempt to call a non-function value")
            | TailCall ->
                let arg = stack.Pop()
                let fn = stack.Pop()
                match fn with
                | VBuiltin(_, f) -> stack.Push(f arg)
                | VCode(id, capturedEnv) ->
                    code <- functions.[id]
                    pc <- 0
                    env <- arg :: capturedEnv.Value
                | _ -> raise (RuntimeError "attempt to call a non-function value")
            | Ret ->
                if frames.Count = 0 then
                    result <- stack.Pop()
                    running <- false
                else
                    let fr = frames.Pop()
                    code <- fr.Code
                    pc <- fr.Pc
                    env <- fr.Env
            | Jump target -> pc <- target
            | JumpIfFalse target ->
                match stack.Pop() with
                | VBool false -> pc <- target
                | VBool true -> ()
                | _ -> raise (RuntimeError "condition is not a boolean")
            | MakeTuple n -> stack.Push(VTuple(popN n))
            | NilList -> stack.Push(VList [])
            | ConsList ->
                let tail = stack.Pop()
                let head = stack.Pop()
                match tail with
                | VList xs -> stack.Push(VList(head :: xs))
                | _ -> raise (RuntimeError "tail of a cons is not a list")
            | MakeConstruct(tag, n) -> stack.Push(VConstruct(tag, popN n))
            | MakeRecord names -> stack.Push(VRecord(List.zip names (popN(List.length names)) |> Map.ofList))
            | GetField name ->
                match stack.Pop() with
                | VRecord fields ->
                    match Map.tryFind name fields with
                    | Some v -> stack.Push v
                    | None -> raise (RuntimeError(sprintf "record has no field '%s'" name))
                | _ -> raise (RuntimeError "field access on a non-record value")
            | Proj i -> stack.Push(projectIndex i (stack.Pop()))
            | NegInt ->
                match stack.Pop() with
                | VInt n -> stack.Push(VInt(-n))
                | _ -> raise (RuntimeError "negation of a non-integer")
            | Prim op ->
                let b = stack.Pop()
                let a = stack.Pop()
                stack.Push(prim op a b)
            | TestInt n ->
                match stack.Pop() with
                | VInt m -> stack.Push(VBool(m = n))
                | _ -> stack.Push(VBool false)
            | TestStr s ->
                match stack.Pop() with
                | VString t -> stack.Push(VBool(t = s))
                | _ -> stack.Push(VBool false)
            | TestBool b ->
                match stack.Pop() with
                | VBool c -> stack.Push(VBool(c = b))
                | _ -> stack.Push(VBool false)
            | TestNil ->
                match stack.Pop() with
                | VList [] -> stack.Push(VBool true)
                | _ -> stack.Push(VBool false)
            | TestCons ->
                match stack.Pop() with
                | VList(_ :: _) -> stack.Push(VBool true)
                | _ -> stack.Push(VBool false)
            | TestCtor tag ->
                match stack.Pop() with
                | VConstruct(t, _) -> stack.Push(VBool(t = tag))
                | _ -> stack.Push(VBool false)
            | MatchFail -> raise (RuntimeError "match failure: no pattern matched")

        result

let compileAndRun (functions: ResizeArray<Instr []>) (globals: Map<string, Value ref>) (expr: Ast.Expr) : Value =
    let entry = compileBlock functions globals [] false expr
    let machine = Machine(functions)
    machine.Run entry
