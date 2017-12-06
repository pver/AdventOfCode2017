open System

let splitStringByTabs (s:string) = s.Split([|'\t'|],StringSplitOptions.RemoveEmptyEntries)

let maxIndexAndValue (a:int[])  =
    a 
    |> Seq.mapi (fun i x -> (i,x)) 
    |> Seq.fold (fun (maxI, maxX) (i,x) -> if x>maxX then (i,x) else (maxI, maxX)) (0,(Array.head a))

let distributeValue (a:int[]) (pos:int) =
    let value = a.[pos]
    a.[pos] <- 0

    let rec fillValue (a:int[]) (pos:int) (value:int) =
        match value with
        | 0 -> ()
        | remaining -> a.[pos] <- a.[pos]+1
                       fillValue a ((pos+1)%a.Length) (remaining-1)

    fillValue a ((pos+1)%a.Length) value

let solve (a:int[]) =
    let rec processRow (a:int[]) (seen:list<int[]>) =
        let (maxIndex,_) = maxIndexAndValue a
        distributeValue a maxIndex
        
        let alreadySeen = (seen|>List.contains a)
        match alreadySeen with
        | true -> let itemsSeen = seen.Length
                  let firstSeen = (seen |> List.findIndex (fun x -> x=a))
                  (itemsSeen, itemsSeen - firstSeen)
        | false -> processRow (a|>Seq.toArray) (seen@[a])
    
    processRow a [(a|>Seq.toArray)]

let challenge6 (input:string) =
    let a = (splitStringByTabs input |> Array.map int)
    solve a

let inputExample = "0	2	7	0"
let (challenge6AExampleResult, challenge6BExampleResult) = challenge6 inputExample
printfn "Example A: %d" <| challenge6AExampleResult
printfn "Example B: %d" <| challenge6BExampleResult

let inputChallenge6 = "14	0	15	12	11	11	3	5	1	6	8	4	9	1	8	4"
let (challenge6AResult, challenge6BResult) = challenge6 inputChallenge6
printfn "Challenge 6A: %d" <| challenge6AResult
printfn "Challenge 6B: %d" <| challenge6BResult