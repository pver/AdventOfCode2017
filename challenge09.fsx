open System

let rec removeIgnores (input:string) =
    match input.IndexOf '!' with
    | -1 -> input
    | x -> input.Remove(x, 2) |> removeIgnores

let removeGarbage (input:string) =
    let rec remove (removeCount:int) (input:string) = 
        match input.IndexOf '<' with
        | -1 -> (input, removeCount)
        | x ->  let lengthToRemove = (input.Substring(x).IndexOf '>')+1
                input.Remove(x, lengthToRemove) |> remove (removeCount+lengthToRemove-2)
    
    let (result, removeCount) = remove 0 input
    printfn "Removed %d chars" removeCount
    result

let removeCommas (input:string) = input.Replace(",", "")

let countScore (input:string) =
    let rec count (i:list<char>) (openCount:int) (counter:int) =
        match i with
        | [] -> counter
        | head::tail -> match head with
                        | '{' -> count tail (openCount+1) counter
                        | '}' -> count tail (openCount-1) (counter+openCount)
                        | _ -> failwith "error in input string"

    count (input.ToCharArray()|>Seq.toList) 0 0

let readInput() = System.IO.File.ReadAllText "inputChallenge09.txt"

let calculateScore = removeIgnores >> removeGarbage >> removeCommas >> countScore

let testExample = fun (x,y) ->  let result = (calculateScore x)
                                printfn "Example '%s' should return %d => %d %s" x y result (if result=y then "OK" else "FAIL")

[("{}", 1); 
("{{{}}}", 6);
("{{},{}}", 5);
("{{{},{},{{}}}}", 16);
("{<a>,<a>,<a>,<a>}", 1);
("{{<ab>},{<ab>},{<ab>},{<ab>}}", 9);
("{{<!!>},{<!!>},{<!!>},{<!!>}}", 9);
("{{<a!>},{<a!>},{<a!>},{<ab>}}", 3)] |> List.iter testExample

let solve = readInput >> calculateScore
solve() |> printfn "Challenge 9 - Group score: %d"