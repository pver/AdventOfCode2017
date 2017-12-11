open System 

type HexagonCount = {n:int; ne:int; se:int; s:int; sw:int; nw:int }

let stepCount (h:HexagonCount) =
    h.n + h.s + h.ne + h.se + h.nw + h.sw

let splitStringByCommas (s:string) = s.Split([|','|],StringSplitOptions.RemoveEmptyEntries)


let reduceOpposites (h:HexagonCount) = 
    {h with n = System.Math.Max(h.n-h.s, 0);
            s = System.Math.Max(h.s-h.n, 0);
            se = System.Math.Max(h.se-h.nw, 0);
            nw = System.Math.Max(h.nw-h.se, 0);
            ne = System.Math.Max(h.ne-h.sw, 0);
            sw = System.Math.Max(h.sw-h.ne, 0);}

let mergeNeighbours (h:HexagonCount) = 
    let n = System.Math.Min(h.ne, h.nw) 
    let s = System.Math.Min(h.se, h.sw) 
    let t = {h with n = h.n + n;
                s = h.s + s;
                se = h.se - s;
                nw = h.nw - n;
                ne = h.ne - n;
                sw = h.sw - s;}

    let se = System.Math.Min(h.ne, h.s) 
    let nw = System.Math.Min(h.n, h.sw) 
    {t with n = t.n - nw;
            s = t.s - se;
            se = t.se + se;
            nw = t.nw + nw;
            ne = t.ne - se;
            sw = t.sw - nw;}   

let reduce (h:HexagonCount) = h |> mergeNeighbours |> reduceOpposites |> mergeNeighbours |> reduceOpposites 

let parseInput (input:string) =
    input 
    |> splitStringByCommas 
    |> Array.fold (fun (acc, max) x -> 
        let newX = reduce <| match x with
                    | "n" -> {acc with n = acc.n+1}
                    | "ne" -> {acc with ne = acc.ne+1}
                    | "se" -> {acc with se = acc.se+1}
                    | "s" -> {acc with s = acc.s+1}
                    | "sw" -> {acc with sw = acc.sw+1}
                    | "nw" -> {acc with nw = acc.nw+1}
                    | _ -> failwith "invalid input"
        let steps = stepCount newX
        (newX, (if steps > max then steps else max))

    ) ({n=0; ne=0; se=0; s=0; sw=0; nw=0 },0)
  

let solve (s:string)  = s |> parseInput         

"ne,ne,ne" |> solve |> printfn "%A"
"ne,ne,sw,sw" |> solve |> printfn "%A"
"ne,ne,s,s" |> solve |> printfn "%A"
"se,sw,se,sw,sw" |> solve |> printfn "%A"

let challengeInput = System.IO.File.ReadAllLines "inputChallenge11.txt" |> Seq.head 
challengeInput |> solve |> fun (h, maxStepCount) -> printfn "Challenge 11A: %d" (stepCount h)
                                                    printfn "Challenge 11B: %d" (maxStepCount)
