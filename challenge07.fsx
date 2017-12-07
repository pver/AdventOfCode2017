open System
open System.Collections.Generic

let splitBy (separator:string) (s:string)  = s.Split([|separator|],StringSplitOptions.RemoveEmptyEntries)
let splitByArrow = splitBy " -> "
let splitByComma = splitBy ", "
let splitNameAndWeight (s:string) = splitBy " (" s |> fun x -> (x.[0], x.[1].Trim(')') |> int)

type TowerItem = {name:string; weight:int; items:string[]}

let parseItems (inputPath:string) =
    inputPath 
    |> System.IO.File.ReadAllLines 
    |> Seq.map (splitByArrow >> (fun x -> 
                    let (name, weight) = x.[0]|>splitNameAndWeight
                    let upperItems = if(x.Length)>1 then (x.[1]|>splitByComma) else [||] 
                    {name=name; weight=weight; items=upperItems}
                    ))
    |> Seq.toArray


let findBottom (items:TowerItem[]) =
    let itemsNoTops =
        items
        |> Array.filter (fun x -> not (Array.isEmpty x.items))
    let candidates = itemsNoTops |> Array.map (fun x -> x.name)
    let upperItems = itemsNoTops |> Array.collect (fun x -> x.items)
    candidates |> Array.except upperItems |> Array.head // should contain only one: the one and only bottom

let inputChallenge7Path = "inputChallenge07.txt"

parseItems inputChallenge7Path |> findBottom |> printfn "Challenge 7A: %s"

let solve () =
    let startItems = parseItems inputChallenge7Path
    
    let calculated = new Dictionary<string, int>()
    
    let rec loop (items:TowerItem[]) =

        let resultsFound = 
            items 
            |> Array.map (fun item -> 
                                match Array.isEmpty item.items with
                                | true -> 
                                            calculated.Add(item.name, item.weight) 
                                            Option.None
                                | false -> 
                                            if (item.items|>Seq.forall calculated.ContainsKey) 
                                            then
                                                let weights = item.items |> Seq.map (fun item -> calculated.[item])
                                                let allSameWeights = (weights |> Seq.distinct |> Seq.length)=1
                                                match allSameWeights with
                                                | true -> 
                                                            calculated.Add(item.name, item.weight + (weights|>Seq.sum))
                                                            Option.None
                                                | false -> 
                                                        let (wrongWeight, i) = weights 
                                                                                |> Seq.countBy (fun x -> x) 
                                                                                |> Seq.filter (fun (_, occurrence)-> occurrence=1) 
                                                                                |> Seq.map (fun (value, _)-> value) 
                                                                                |> Seq.head
                                                                                |> fun x -> (x, weights |> Seq.findIndex ((=) x))
                                                        let correctWeight = weights |> Seq.filter (fun x -> x <> wrongWeight) |> Seq.head
                                                        let tooMuchWeight = (wrongWeight-correctWeight)

                                                        let misbehavingItem = startItems |> Array.filter (fun x -> x.name = (item.items.[i])) |> Array.head
                                                        
                                                        let expectedWeight = (misbehavingItem.weight-tooMuchWeight)
                                                        Option.Some(expectedWeight)

                                            else 
                                                Option.None
                            )

            |> Array.filter (Option.isSome)
            |> Array.map (Option.get)
        
        match resultsFound with
        | [||] -> loop (items|> Array.filter (fun x -> not (calculated.ContainsKey x.name)))
        | x -> x |> Array.head

    loop startItems

printfn "Challenge 7B: %d" <| solve ()