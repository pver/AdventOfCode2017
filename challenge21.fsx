open System

type Grid = string[]
let start : Grid = [|".#.";"..#";"###"|]

let gridToString (g:Grid) = String.Join("/", g)

let stringToGrid (s:string) = s.Split([|"/"|],StringSplitOptions.RemoveEmptyEntries)

type Pattern = {input:string;output:string}

let rotate2 (grid:Grid) =
    let g2d = grid |> Array.map (fun x->x.ToCharArray())
    [|(sprintf "%c%c" g2d.[1].[0] g2d.[0].[0]);(sprintf "%c%c" g2d.[1].[1] g2d.[0].[1])|]

let rotate3 (grid:Grid) =
    let g3d = grid |> Array.map (fun x->x.ToCharArray())
    [|(sprintf "%c%c%c" g3d.[1].[0] g3d.[0].[0] g3d.[0].[1]);
      (sprintf "%c%c%c" g3d.[2].[0] g3d.[1].[1] g3d.[0].[2]);
      (sprintf "%c%c%c" g3d.[2].[1] g3d.[2].[2] g3d.[1].[2]);|]

let generateVariants (pattern:Pattern) =
    let inp = pattern.input
    let inpGrid = stringToGrid inp

    let flipHorizontal (g:Grid) = Array.rev g
    let flipVertical (g:Grid) = g |> Array.map (fun x->string(x.ToCharArray()|>Array.rev|> fun x -> String(x)))

    let inputVariants = 
        match inpGrid.Length with
        | 2 ->  let rot1 = rotate2 inpGrid
                let rot2 = rotate2 rot1
                let rot3 = rotate2 rot2
                [|inpGrid;rot1;rot2;rot3;|] 
        | 3 ->  let rot1 = rotate3 inpGrid
                let rot2 = rotate3 rot1
                let rot3 = rotate3 rot2
                let rot4 = rotate3 rot3
                let rot5 = rotate3 rot4
                let rot6 = rotate3 rot5
                let rot7 = rotate3 rot6
                let rot8 = rotate3 rot7
                [|inpGrid;
                //rot1;
                rot2;
                //rot3;
                rot4;
                //rot5;
                rot6;
                //rot7;
                //rot8;
                |]
        | _ -> failwith "invalid input variant size"

    inputVariants 
    |> Array.collect (fun x -> [|x;(flipHorizontal x);(flipVertical x)|]) 
    |> Array.map (gridToString >> fun x->{input=x;output=pattern.output})

let inputPatterns (input:string) = 
    let patterns = input.Split([|System.Environment.NewLine|],StringSplitOptions.RemoveEmptyEntries)

    patterns 
    |> Seq.map ((fun x -> x.Split([|" => "|],StringSplitOptions.RemoveEmptyEntries)) >> (fun x -> {input=x.[0];output=x.[1]}))
    |> Seq.collect generateVariants
    |> Seq.toArray

let patterns = inputPatterns <| System.IO.File.ReadAllText("inputChallenge21.txt")
//<| "../.# => ##./#../...
//.#./..#/### => #..#/..../..../#..#"//

start |> gridToString |> printfn "Start iteration 0: %s"
let iteration0 = start |> gridToString

// find pattern:
let applyPattern (inp:string) = 
    let results = patterns |> Array.filter (fun x -> x.input = inp) |> Array.distinct
    if (results |> Array.length) >1 then 
        printfn "duplicates found! %A" results
        results |> Array.map (fun x -> x.output)  |> Array.skip 1 |> Array.head
    else
        results |> Array.map (fun x -> x.output)  |> Array.head
let iteration1 = iteration0 |> applyPattern
iteration1 |> printfn "After iteration 1: %s"

let iteration1ToGrid = stringToGrid iteration1 
iteration1ToGrid |> printfn "Grid after iteration 1: %A"

let rec split2ArrayGrid (g:Grid) =
    printfn "2A g.[0].Length=%d" g.[0].Length
    match g.[0].Length with
    | 2 -> (g |> gridToString |> applyPattern |> stringToGrid)
    | length -> 
                let parts = Array.init (length/2) (fun x -> x*2)
                             |> Array.map (fun x -> split2ArrayGrid  [| g.[0].Substring(x,2); g.[1].Substring(x,2) |])
                
                let row1 = Array.init (parts.Length) id |> Array.map (fun x -> parts.[x].[0]) |> Array.fold (+) ""
                let row2 = Array.init (parts.Length) id |> Array.map (fun x -> parts.[x].[1]) |> Array.fold (+) ""
                let row3 = Array.init (parts.Length) id |> Array.map (fun x -> parts.[x].[2]) |> Array.fold (+) ""
                [|row1;row2;row3|]

                // let firstPart = split2ArrayGrid  [| g.[0].Substring(0,2); g.[1].Substring(0,2) |]
                // let secondPart = split2ArrayGrid [| g.[0].Substring(length/2); g.[1].Substring(length/2) |];
                // [|  (firstPart.[0]+secondPart.[0]);
                //     (firstPart.[1]+secondPart.[1]);
                //     (firstPart.[2]+secondPart.[2])|]

let rec split3ArrayGrid (g:Grid) =
    printfn "3A g.[0].Length=%d" g.[0].Length
    match g.[0].Length with
    | 3 -> (g |> gridToString |> applyPattern |> stringToGrid)
    | length -> let firstPart = split3ArrayGrid  [| g.[0].Substring(0,(length/3)); g.[1].Substring(0,(length/3)); g.[2].Substring(0,(length/3)) |]
                let secondPart = split3ArrayGrid [| g.[0].Substring(length/3,length/3); g.[1].Substring(length/3,length/3) ; g.[2].Substring(length/3,length/3) |];
                let thirdPart = split3ArrayGrid [| g.[0].Substring(2*length/3); g.[1].Substring(2*length/3); g.[2].Substring(2*length/3) |];
                
                [|(firstPart.[0]+secondPart.[0]+thirdPart.[0]);
                  (firstPart.[1]+secondPart.[1]+thirdPart.[1]);
                  (firstPart.[2]+secondPart.[2]+thirdPart.[2]);
                  (firstPart.[3]+secondPart.[3]+thirdPart.[3])|]


let transformGrid (g:Grid) = 
    printfn "transforming size %d" g.[0].Length
    if (g.Length % 2) = 0
    then  
         g 
         |> Array.chunkBySize 2
         |> Array.collect split2ArrayGrid
    else 
         g 
         |> Array.chunkBySize 3
         |> Array.collect split3ArrayGrid
//     |> Array.map (fun arrs -> [|sprintf "%s%s" (arrs.[0].Substring(0,(arrs.[0].Length/2)-1)) (arrs.[1].Substring(0,(arrs.[0].Length/2)-1)); 
//                                     sprintf "%s%s" (arrs.[0].Substring(arrs.[0].Length/2)) (arrs.[1].Substring(arrs.[0].Length/2))|])
//     |> Array.map (fun g -> gridToString g)
//     |> Array.map applyPattern

iteration1ToGrid |> transformGrid |> printfn "Grid after split after iter 1: %A"
let resultITeration5 = start |> transformGrid |> transformGrid |> transformGrid |> transformGrid |> transformGrid 
resultITeration5 |> printfn "Grid after iter 5 with new transform: %A"

resultITeration5 |> gridToString |> fun x -> x.ToCharArray() |> Array.filter ((=) '#') |> Array.length |> printfn "Items ON after iter 5: %d"

let resultITeration18 = start 
                        |> transformGrid 
                        |> transformGrid 
                        |> transformGrid 
                        |> transformGrid 
                        |> transformGrid 
                        |> transformGrid 
                        |> transformGrid 
                        |> transformGrid 
                        |> transformGrid 
                        |> transformGrid 
                        |> transformGrid 
                        |> transformGrid 
                        |> transformGrid 
                        |> transformGrid 
                        |> transformGrid 
                        |> transformGrid 
                        |> transformGrid 
                        |> transformGrid 
resultITeration18 |> gridToString |> fun x -> x.ToCharArray() |> Array.filter ((=) '#') |> Array.length |> printfn "Items ON after iter 18: %d"