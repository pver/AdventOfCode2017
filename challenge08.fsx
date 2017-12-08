open System
open System.Collections.Generic

type Operation =
    | Inc of int
    | Dec of int

type CompareValue =
    | LiteralInt of int

type Comparison =
    | LowerThan of CompareValue
    | GreaterThan of CompareValue
    | Equal of CompareValue
    | NotEqual of CompareValue
    | LowerThanOrEqual of CompareValue
    | GreaterThanOrEqual of CompareValue

type Condition = {registerName:string;comparison:Comparison}

type Instruction = {targetRegister:string;operation:Operation;condition:Condition}

let parse (input:string) =
    let inputParts = input.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)

    match inputParts with
    | [|target;operation;operationvalue;_;conditionregister;comparisonOperator;comparisonvalue|] -> 
            let op = match operation with 
                     | "inc" -> Inc <| int operationvalue
                     | "dec" -> Dec <| int operationvalue
                     | _ -> failwith "invalid operation detected"
            let compValue = LiteralInt <| int comparisonvalue
            let comparison = match comparisonOperator with
                             | "<" -> LowerThan compValue
                             | ">" -> GreaterThan compValue
                             | "==" -> Equal compValue
                             | "!=" -> NotEqual compValue
                             | "<=" -> LowerThanOrEqual compValue
                             | ">=" -> GreaterThanOrEqual compValue
                             | _ -> failwith "invalid comparisonOperator detected"
            let condition = {registerName=conditionregister;comparison=comparison}
            {targetRegister=target;operation=op;condition=condition}
    | _ -> failwith "invalid input detected"

let processor (instructions:Instruction[]) =
    let registers = new Dictionary<string, int>()
    instructions 
                    |> Array.map (fun x -> x.targetRegister) 
                    |> Array.append (instructions |> Array.map (fun x -> x.condition.registerName)) 
                    |> Array.distinct 
                    |> Seq.iter (fun x -> registers.Add(x,0))

    let processInstruction (instruction:Instruction) = 
        let checkingRegisterValue = registers.[instruction.condition.registerName]
        
        let compareFunction = match instruction.condition.comparison with
                                | LowerThan (LiteralInt x) -> (>) x
                                | GreaterThan (LiteralInt x) -> (<) x
                                | Equal (LiteralInt x) -> (=) x
                                | NotEqual (LiteralInt x) -> (<>) x
                                | LowerThanOrEqual (LiteralInt x) -> (>=) x
                                | GreaterThanOrEqual (LiteralInt x) -> (<=) x
        let operationFunction = match instruction.operation with
                                | Inc x -> (+) x
                                | Dec x -> fun y -> y - x

        let newValue = match (compareFunction checkingRegisterValue) with
                        | true -> operationFunction registers.[instruction.targetRegister]
                        | false -> registers.[instruction.targetRegister]
        registers.[instruction.targetRegister] <- newValue
        newValue

    let maxMemory = instructions |> Seq.map processInstruction |> Seq.max
    
    (registers, maxMemory)

let parseFile (filepath:string) =
    System.IO.File.ReadAllLines(filepath) 
    |> Array.map parse

let instructions = parseFile "inputChallenge08.txt"
instructions |> printfn "%A"

let (registers, maxValue) = processor instructions
registers |> Seq.toArray |> Seq.maxBy (fun x-> x.Value) |> printfn "%A"
maxValue |> printfn "%d"