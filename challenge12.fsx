open System

let add (x:int) (y:int) (groups:int list list) =
    let groupX = groups |> Seq.filter (fun g->g|>Seq.contains x) |> Seq.tryHead
    let groupY = groups |> Seq.filter (fun g->g|>Seq.contains y) |> Seq.tryHead

    match (groupX, groupY) with
    | (Option.None, Option.None) -> [[x;y]]@groups
    | (Option.Some gX, Option.None) -> groups |> List.except [gX] |> List.append [(gX@[y])]
    | (Option.None, Option.Some gY) -> groups |> List.except [gY] |> List.append [(gY@[x])]
    | (Option.Some gX, Option.Some gY) -> match gX=gY with
                                          | true -> groups
                                          | false -> groups |> List.except [gX] |> List.except [gY] |> List.append [gX@gY]
                                       
let input () = 
    System.IO.File.ReadAllLines "inputChallenge12.txt" 
    |> Seq.map (fun x->
        let line = x.Split([|" <-> "|],StringSplitOptions.RemoveEmptyEntries)
        let from = int line.[0]
        let tos = (line.[1]).Split([|", "|],StringSplitOptions.RemoveEmptyEntries)
        tos |> Seq.map (fun y ->(from, int y))
        )
    |> Seq.collect id

let result = input() |> Seq.fold (fun groups (x, y) -> add x y groups) []
printfn "Group count = %d" (result |> Seq.length)

let zeroGroup = (result |> Seq.filter (fun g->g|>Seq.contains 0) |> Seq.head)
printfn "Group with 0= %A, count=%d" zeroGroup (zeroGroup |> Seq.distinct |>Seq.length)