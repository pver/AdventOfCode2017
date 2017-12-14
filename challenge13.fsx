open System

let splitStringByLines (s:string) = s.Split([|System.Environment.NewLine|],StringSplitOptions.RemoveEmptyEntries)
let splitStringByColons (s:string) = s.Split([|':'|],StringSplitOptions.RemoveEmptyEntries)
let input = "0: 3
1: 2
2: 4
4: 4
6: 5
8: 8
10: 6
12: 6
14: 8
16: 6
18: 6
20: 8
22: 12
24: 8
26: 8
28: 12
30: 8
32: 12
34: 9
36: 14
38: 12
40: 12
42: 12
44: 14
46: 14
48: 10
50: 14
52: 12
54: 14
56: 12
58: 17
60: 10
64: 14
66: 14
68: 12
70: 12
72: 18
74: 14
78: 14
82: 14
84: 24
86: 14
94: 14" 

let exampleInput = "0: 3
1: 2
4: 4
6: 4"

let toRowId ((x,_):int*int) = x

let parse (input:string) = 
    let parsed = input 
                    |> splitStringByLines 
                    |> Seq.map (splitStringByColons >> (fun x -> (int x.[0], int x.[1]))) 
                    |> Seq.toArray
    let rowIds = parsed |> Seq.map toRowId
    let max = rowIds |> Seq.max
    let ids = Array.init (max + 1) id
    let missingIds = ids |> Seq.except rowIds
    missingIds 
    |> Seq.fold (fun acc x -> Array.append acc [|(x,0)|]) parsed 
    |> Array.sortBy toRowId

let isCaught (time:int) (depth:int) = ((time % (depth*2-2))=0 || depth=0)

let severity (layers:(int*int)[]) = layers |> Seq.sumBy (fun (time,y) -> if isCaught time y then time*y else 0)


let parsedExample = parse exampleInput
let parsedInput = parse input
printfn "example %A" parsedExample
printfn "number caught: %d" <| severity parsedExample
printfn "number caught: %d" <| severity parsedInput


let rec solve (delay:int) (layers:(int*int)[])  = 
    let severity = layers |> Array.map (fun (x,y)->(x+delay, y)) |> severity
    match severity with
    | 0 -> delay
    | _ -> solve (delay+1) layers

let totalTimeExampleUncaught = parsedExample |> solve 0
printfn "Total time: %d" totalTimeExampleUncaught

let totalChallengeUncaught = parsedInput |> solve 0
printfn "Total time: %d" totalChallengeUncaught