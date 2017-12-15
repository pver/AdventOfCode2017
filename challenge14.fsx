#load "challenge10.fsx"
#load "challenge12.fsx"

let getHash = Challenge10.challenge10B

let hexCharToInt (c:char) = System.Convert.ToInt32(sprintf "%c" c, 16)
let intTo4BitString (i:int) = System.Convert.ToString(i,2).PadLeft(4,'0')

let fromHexToBits (inp:string) = 
    inp.ToCharArray() 
    |> Array.map (hexCharToInt >> intTo4BitString)
    |> Array.fold (fun acc x->sprintf "%s%s" acc x) ""

let convertToBitSquare (inp:string) = 
    Array.init 128 (fun x -> sprintf "%s-%d" inp x)
    |> Array.map (getHash >> fromHexToBits >> (fun x -> x.ToCharArray()))

let exampleInput = "flqrgnkx"
let challengeInput = "xlqgujun"
let solveA (inp:string) = convertToBitSquare inp |> Seq.fold (fun acc x -> acc + (x|>Seq.filter (fun c->c='1') |> Seq.length )) 0

solveA exampleInput |> printfn "Ch 14-A example: %A"
solveA challengeInput |> printfn "Ch 14-A: %A"

let getConnections (bits:char[]) =
    bits 
    |> Seq.mapi (fun i c -> (if c='1' then [|(i,i)|] else [||]) |> Seq.append    (if i>bits.Length-2 || c='0' || bits.[i+1]='0' then [||]
                                                                                 else [|(i, i+1)|]))
    |> Seq.concat
    |> Seq.toArray

let solveB (inp:string) = 
    
    let square = convertToBitSquare inp
    let rowConnections = square |> Seq.mapi (fun i r -> getConnections r |> Seq.map (fun (a,b) -> (a+(i*128),b+(i*128)))) |> Seq.concat

    let transposedSquare = Seq.init square.Length id |> Seq.map (fun colId -> Seq.init square.Length id |> Seq.map (fun rowId -> square.[rowId].[colId])|>Seq.toArray) |>Seq.toArray
    let colConnections = transposedSquare |> Seq.mapi (fun i r -> getConnections r |> Seq.map (fun (a,b) -> (a*128+(i),b*128+i))) |> Seq.concat

    let connections = rowConnections |> Seq.append colConnections
    connections |> Challenge12.createConnectedGroups |> Seq.length

solveB exampleInput |> printfn "Ch 14-B example: %A"
solveB challengeInput |> printfn "Ch 14-B: %A"

//results:
// Ch 14-A example: 8108
// Ch 14-A: 8204
// Ch 14-B example: 1242
// Ch 14-B: 1089