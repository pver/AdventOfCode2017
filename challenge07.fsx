open System
open System.Collections.Generic

let inputChallenge7Path = "inputChallenge07.txt"
let splitBy (separator:string) (s:string)  = s.Split([|separator|],StringSplitOptions.RemoveEmptyEntries)
let splitByArrow = splitBy " -> "
let splitByComma = splitBy ", "
let splitNameAndWeight (s:string) = splitBy " (" s |> fun x -> (x.[0], x.[1].Trim(')') |> int)

type TowerItem = {name:string; weight:int; items:string[]}

let parseItems (inputPath:string) =
    inputPath 
    |> System.IO.File.ReadAllLines 
    |> Array.map (splitByArrow >> (fun x -> 
                    match x.Length with
                    | 1 | 2 -> let (name, weight) = x |> Array.head |> splitNameAndWeight                  
                               let subItems = match x |> Array.tail with
                                               | [|subItems|] -> subItems|>splitByComma
                                               | _ ->  [||] 
                               {name=name; weight=weight; items=subItems}
                    | _ -> failwith "invalid item found"
                    ) )

let findBottom (items:TowerItem[]) =
    let candidates = items |> Array.map (fun x -> x.name)
    let subItemNames = items |> Array.collect (fun x -> x.items)
    candidates |> Array.except subItemNames |> Array.exactlyOne // should contain only one: the one and only bottom

parseItems inputChallenge7Path |> findBottom |> printfn "Challenge 7A: %s"

let solveChallenge (startItems:TowerItem[]) =
    let calculated = new Dictionary<string, int>()

    let rec loop (items:TowerItem[]) =
        let resultsFound = 
            items 
            |> Array.map (fun item -> 
                                match item.items with
                                | [||] -> calculated.Add(item.name, item.weight) 
                                          Option.None
                                | subItems -> 
                                            match (subItems|>Seq.forall calculated.ContainsKey) with
                                            | false -> Option.None
                                            | true ->
                                                    let weights = subItems |> Array.map (fun item -> calculated.[item])
                                                    let allSameWeights = (weights |> Seq.distinct |> Seq.length)=1
                                                    match allSameWeights with
                                                    | true -> calculated.Add(item.name, item.weight + (weights|>Seq.sum))
                                                              Option.None
                                                    | false -> 
                                                            let (wrongWeight, i) = weights 
                                                                                    |> Seq.countBy id
                                                                                    |> Seq.filter (fun (_, occurrence)-> occurrence=1) 
                                                                                    |> Seq.map (fun (value, _)-> value) 
                                                                                    |> Seq.head
                                                                                    |> fun x -> (x, weights |> Seq.findIndex ((=) x))
                                                            let correctWeight = weights |> Seq.filter ((<>) wrongWeight) |> Seq.head
                                                            let overWeight = (wrongWeight-correctWeight)

                                                            let misbehavingItem = startItems |> Array.filter (fun x -> x.name = (subItems.[i])) |> Array.head
                                                            
                                                            let expectedWeight = (misbehavingItem.weight-overWeight)
                                                            Option.Some(expectedWeight)
                            )

            |> Array.filter (Option.isSome)
            |> Array.map (Option.get)
        
        match resultsFound with
        | [||] -> loop (items|> Array.filter (fun x -> not (calculated.ContainsKey x.name)))
        | x -> x |> Array.head

    loop startItems

printfn "Challenge 7B: %d" <| solveChallenge (parseItems inputChallenge7Path)