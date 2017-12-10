open System 

let swapElementsWithOverflow (arr:int[]) (a:int) (b:int) = 
    let bValue = arr.[b%arr.Length]
    arr.[b%arr.Length] <- arr.[a%arr.Length]
    arr.[a%arr.Length] <- bValue

let elementsToSwap (start:int) (length:int) = List.init (length/2) (fun x -> (start+x, start+length-1-x))

let swapLength (arr:int[]) (pos:int) (length:int) =
    elementsToSwap pos length |> Seq.iter (fun (a:int,b:int) -> swapElementsWithOverflow arr a b)

let processLengths (arr:int[]) (lengths:int list) (pos:int) (skipSize:int) =
    let rec processLength (arr:int[]) (lengths:int list) (pos:int) (skipSize:int) =
        match lengths with
        | [] -> (arr, pos, skipSize)
        | length::tail ->   swapLength arr pos length
                            processLength arr tail (pos+length+skipSize) (skipSize+1)
    processLength arr lengths pos skipSize

let challengeInputA = [88;88;211;106;141;1;78;254;2;111;77;255;90;0;54;205]

let challenge10A() =
    let (resultArray, _, _) = processLengths (Array.init 256 id) challengeInputA 0 0
    resultArray.[0]*resultArray.[1]

printfn "Challenge A result multiplication: %d" <| challenge10A ()

let reworkInputString (input:string) = 
    input.ToCharArray() 
    |> Array.map int 
    |> fun x -> Array.append x [|17; 31; 73; 47; 23|] 
    |> Array.toList

let processMultipleRounds (arr:int[]) (lengths:int list) =
    let rec runRounds (roundsLeft:int) (pos:int) (skipSize:int) =
        match roundsLeft with
        | 0 -> arr
        | _ -> let (_, newpos, newskipSize) = processLengths arr lengths pos skipSize
               runRounds (roundsLeft-1) newpos newskipSize
    runRounds 64 0 0

let challengeInputB = reworkInputString "88,88,211,106,141,1,78,254,2,111,77,255,90,0,54,205"

let xor (arr:int[]) = arr |> Array.fold (^^^) 0
let toHex (input:int) = (sprintf "%x" input).PadLeft(2,'0')

let challenge10B (lengths:int list) =
    processMultipleRounds (Array.init 256 id) lengths
                |> Array.chunkBySize 16 
                |> Array.map (xor>>toHex)
                |> Array.fold (+) String.Empty

printfn "Challenge B result: %s" <| challenge10B challengeInputB
let testExample = fun (x,y) ->  let result = challenge10B (reworkInputString x)
                                printfn "Example '%s' should return %s => %s %s" x y result (if result=y then "OK" else "FAIL")

[("", "a2582a3a0e66e6e86e3812dcb672a272"); 
("AoC 2017", "33efeb34ea91902bb2f59c9920caa6cd");
("1,2,3", "3efbe78a8d82f29979031a4aa0b16a9d");
("1,2,4", "63960835bcdc130f0b66d7ff4f6a5a8e")] |> List.iter testExample