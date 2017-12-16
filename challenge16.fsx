open System

// Spin, written sX, makes X programs move from the end to the front, but maintain their order otherwise. (For example, s3 on abcde produces cdeab).
// Exchange, written xA/B, makes the programs at positions A and B swap places.
// Partner, written pA/B, makes the programs named A and B swap places.

type DanceMove =
    | Spin of int
    | Exchange of int*int
    | Partner of char*char

let inputChallenge16 = System.IO.File.ReadAllLines("inputChallenge16.txt") |> Seq.head

let parseInput (input:string) = 
    let danceMoveStrings = input.Split([|','|],StringSplitOptions.RemoveEmptyEntries)
    danceMoveStrings 
    |> Array.map (fun x->   let moveInfo = x.Substring(1)
                            match x.[0] with
                             | 's' -> Spin (int moveInfo)
                             | 'x' -> let parts = moveInfo.Split([|'/'|],StringSplitOptions.RemoveEmptyEntries)
                                      Exchange (int parts.[0], int parts.[1])
                             | 'p' -> let parts = moveInfo.Split([|'/'|],StringSplitOptions.RemoveEmptyEntries)
                                      Partner (parts.[0].[0], parts.[1].[0])
                             | _ -> failwith "invalid move found"
    )

let rec dance (move:DanceMove) (input:char[]) =
    let numberOfPartners = input.Length
    match move with
    | Spin X -> //printfn "Spin %s: %d" input X
                let endPart = input |> Array.skip (numberOfPartners - X)
                let startPart = input |> Array.take (numberOfPartners - X)
                Array.append endPart startPart
    | Exchange (A,B) -> //printfn "Exch %s: %d->%d" input A B
                        let backupA = input.[A]
                        input.[A] <- input.[B]
                        input.[B] <- backupA
                        input
    | Partner (A,B) ->  
                        let posA = input |> Array.findIndex ((=) A)
                        let posB = input |> Array.findIndex ((=) B)
                        let backupA = input.[posA]
                        input.[posA] <- input.[posB]
                        input.[posB] <- backupA
                        input


let challenge16 (numberOfDances:int) (numberOfPartners:int) (danceMoves:DanceMove[])   = 
    let startPositions = Array.init numberOfPartners (fun i->(char)(i+int 'a'))
    let seen = new System.Collections.Generic.List<String>()

    let rec doDances (n:int) (startpos:char[]) = 
        let adjustedN = 
            if seen.Contains (String(startpos)) then 
                //printfn "Found %s again after %d iterations at position %d" (String(startpos)) (numberOfDances - n) (seen.IndexOf( String(startpos) ))
                seen.Clear()
                n % (numberOfDances - n)
            else
                n
        seen.Add (String(startpos))
        match adjustedN with
        | 0 -> startpos
        | c -> let endpos = danceMoves |> Array.fold (fun acc x -> dance x acc) startpos
               doDances (c-1) endpos

    doDances numberOfDances startPositions

let challenge16A = challenge16 1
let challenge16B = challenge16 1000000000
                                

printfn "Challenge 16 A Example: %s" <| String (challenge16A 5 <| parseInput "s1,x3/4,pe/b")
printfn "Challenge 16 A: %s" <| String (challenge16A 16 <| parseInput inputChallenge16)

printfn "Challenge 16 B Example: %s" <| String ( challenge16B 5 <| parseInput "s1,x3/4,pe/b")
printfn "Challenge 16 B: %s" <| String ( challenge16B 16 <| parseInput inputChallenge16)