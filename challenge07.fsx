open System
open System.Collections.Generic

let splitBy (separator:string) (s:string)  = s.Split([|separator|],StringSplitOptions.RemoveEmptyEntries)
let splitByArrow = splitBy " -> "
let splitByComma = splitBy ", "
let splitNameAndWeight (s:string) = splitBy " (" s |> fun x -> (x.[0], x.[1].Trim(')') |> int)

let parseItems (inputPath:string) =
    inputPath 
    |> System.IO.File.ReadAllLines 
    |> Seq.map (splitByArrow >> (fun x -> 
                    let (name, weight) = x.[0]|>splitNameAndWeight
                    let upperItems = if(x.Length)>1 then (x.[1]|>splitByComma) else [||] 
                    (name, weight, upperItems)
                    ))
    |> Seq.toArray


let findBottom (items:(string*int*string[])[]) =
    let itemsNoTops =
        items
        |> Array.filter (fun (_,_,upperItems) -> not (Array.isEmpty upperItems))
    let candidates = itemsNoTops |> Array.map (fun (name,_,_) -> name)
    let upperItems = itemsNoTops |> Array.collect (fun (_,_,upperItems) -> upperItems)
    candidates |> Array.except upperItems |> Array.head // should contain only one: the one and only bottom

let inputChallenge7Path = "inputChallenge07.txt"

parseItems inputChallenge7Path |> findBottom |> printfn "Challenge 7A: %s"

let solve =
    let startItems = parseItems inputChallenge7Path
    
    let calculated = new Dictionary<string, int>()
    
    let rec loop (items:(string*int*string[])[]) =

        let resultsFound = 
            items 
            |> Array.map (fun (name, weight, subitems) -> 
                                match Array.isEmpty subitems with
                                | true -> 
                                            calculated.Add(name, weight) 
                                            Option.None
                                | false -> 
                                            if (subitems|>Seq.forall calculated.ContainsKey) 
                                            then
                                                let weights = subitems |> Seq.map (fun item -> calculated.[item])
                                                let allSameWeights = (weights |> Seq.distinct |> Seq.length)=1
                                                match allSameWeights with
                                                | true -> 
                                                            calculated.Add(name, weight + (weights|>Seq.sum))
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

                                                        let (_,itemOwnWeight,_) = startItems |> Array.filter (fun (name,_,_) -> name = (subitems.[i])) |> Array.head
                                                        
                                                        let expectedWeight = (itemOwnWeight-tooMuchWeight)
                                                        Option.Some(expectedWeight)

                                            else 
                                                Option.None
                            )

            |> Array.filter (Option.isSome)
            |> Array.map (Option.get)
        
        match resultsFound with
        | [||] -> loop (items|> Array.filter (fun (name,_,_) -> not (calculated.ContainsKey name)))
        | x -> x |> Array.head

    loop startItems


let calculated = solve
printfn "Challenge 7B: %d" calculated