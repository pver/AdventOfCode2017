open System

let example = "     |          
     |  +--+    
     A  |  C    
 F---|----E|--+ 
     |  |  |  D 
     +B-+  +--+ 
"

type Direction =
    | Up
    | Down
    | Left
    | Right
    | Stop


let getStart (lines:string[]) = (0,(lines.[0]).IndexOf('|'),Down, "")

let rec follow (lines:string[]) (currentState:int*int*Direction*string) (stepCount:int) = 
    //printfn "%A" currentState
    let width = lines.[0].Length
    let height = lines.Length
    let (row, col, dir, seen) = currentState
    let posVal = lines.[row].[col]
    match dir with
    | Stop -> (seen, stepCount-1)
    | Up -> let state = match posVal with
                          | '|' | '-' -> (row-1, col, Up, seen)
                          | '+' ->  if (col+1 <width) && lines.[row].[col+1] <> ' ' 
                                    then (row, col+1, Right, seen)
                                    else (row, col-1, Left, seen)
                          | ' ' -> (row, col, Stop, seen)
                          | x -> (row-1, col, Up, sprintf "%s%c" seen x)
            follow lines state (stepCount+1)
    | Down -> let state = match posVal with
                              | '|' | '-' -> (row+1, col, Down, seen)
                              | '+' ->  if (col+1 <width) && lines.[row].[col+1] <> ' ' 
                                        then (row, col+1, Right, seen)
                                        else (row, col-1, Left, seen)
                              | ' ' -> (row, col, Stop, seen)
                              | x -> (row+1, col, Down, sprintf "%s%c" seen x)
              follow lines state (stepCount+1)
    | Left -> let state = 
                        match posVal with
                        | '|' | '-' -> (row, col-1, Left, seen)
                        | '+' ->  if (row+1<height) && lines.[row+1].[col] <> ' ' 
                                  then (row+1, col, Down, seen)
                                  else (row-1, col, Up, seen)
                        | ' ' -> (row, col, Stop, seen)
                        | x -> (row, col-1, Left, sprintf "%s%c" seen x)
              follow lines state (stepCount+1)
    | Right -> let state = match posVal with
                              | '|' | '-' -> (row, col+1, Right, seen)
                              | '+' ->  if (row+1<height) && lines.[row+1].[col] <> ' ' 
                                        then (row+1, col, Down, seen)
                                        else (row-1, col, Up, seen)
                              | ' ' -> (row, col, Stop, seen)
                              | x -> (row, col+1, Right, sprintf "%s%c" seen x)
               follow lines state (stepCount+1)


let exampleLines = example.Split([|System.Environment.NewLine|],StringSplitOptions.RemoveEmptyEntries)
printfn "Example result=%A" <| follow exampleLines (getStart exampleLines) 0

let challengeLines = System.IO.File.ReadAllLines("inputChallenge19.txt")
printfn "Challenge 19A result=%A" <| follow challengeLines (getStart challengeLines) 0