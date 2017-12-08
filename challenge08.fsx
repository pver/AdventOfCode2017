open System
// b inc 5 if a > 1
// a inc 1 if b < 5
// c dec -10 if a >= 1
// c inc -20 if c == 10
// These instructions would be processed as follows:

// Because a starts at 0, it is not greater than 1, and so b is not modified.
// a is increased by 1 (to 1) because b is less than 5 (it is 0).
// c is decreased by -10 (to 10) because a is now greater than or equal to 1 (it is 1).
// c is increased by -20 (to -10) because c is equal to 10.
// After this process, the largest value in any register is 1.

// You might also encounter <= (less than or equal to) or != (not equal to). However, the CPU doesn't have the bandwidth to tell you what all the registers are named, and leaves that to you to determine.

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

type Register = {name:string; value:int}
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

System.IO.File.ReadAllLines("inputChallenge08.txt") |> Array.map parse |> printfn "%A"