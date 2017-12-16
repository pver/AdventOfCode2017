open System

let factorA = (int64)16807
let factorB = (int64)48271


let rec countMatches (remaining:int) (count:int) (currentA:int64) (currentB:int64) (generatorA:int64->int64->int64) (generatorB:int64->int64->int64)=
    // if (remaining%10000)=0 then
    //     printfn "rem=%d" remaining

    match remaining with
    | 0 -> count
    | c -> let newA = generatorA factorA currentA
           let newB = generatorB factorB currentB
           //printfn "%d | %d" newA newB
           let matches = (newA &&& (int64)65535) = (newB &&& (int64)65535)
           countMatches (c-1) (count+ (if matches then 1 else 0)) newA newB generatorA generatorB

let startA = 699
let startB = 124

let exampleStartA = 65
let exampleStartB = 8921

let nextValue (factor:int64) (current:int64) =
    (current * factor) % (int64)2147483647

//printfn "Example A matching pairs=%d" <| countMatches 40000000 0 ((int64)exampleStartA) ((int64)exampleStartB) nextValue nextValue
//printfn "Challenge 15 A matching pairs=%d" <| countMatches 40000000 0 ((int64)startA) ((int64)startB) nextValue nextValue

let nextValuePart2 (modulo:int64) (factor:int64) (current:int64)  =
    let rec returnOrGenerateNext (c:int64) =
        match c % modulo with
        | 0L -> c
        | _ -> returnOrGenerateNext (nextValue factor c)
    returnOrGenerateNext (nextValue factor current)

let generatorA = nextValuePart2 4L
let generatorB = nextValuePart2 8L

let pairCount = 5000000
printfn "Example B matching pairs=%d" <| countMatches pairCount 0 ((int64)exampleStartA) ((int64)exampleStartB) generatorA generatorB
printfn "Challenge 15 B matching pairs=%d" <| countMatches pairCount 0 ((int64)startA) ((int64)startB) generatorA generatorB