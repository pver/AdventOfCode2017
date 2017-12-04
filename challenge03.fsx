open System

let squareSize (input:int) = 
    let squareCeiling = input |> float |> Math.Sqrt |> Math.Ceiling |> int
    if (squareCeiling % 2 = 0) then squareCeiling + 1 else squareCeiling

let challenge3a (input:int) =
    let s = squareSize input
    let insideDirection = s / 2

    let zeroPoints = [s*s-insideDirection; s*s-insideDirection-(2*insideDirection);  s*s-insideDirection-(4*insideDirection);s*s-insideDirection-(6*insideDirection);]

    let sideDirection = zeroPoints |> Seq.map (fun x-> Math.Abs (x-input)) |> Seq.filter (fun x -> x<=insideDirection) |> Seq.head
    sideDirection + insideDirection

printfn "12=> %d" <| challenge3a 12
printfn "23=> %d" <| challenge3a 23
printfn "1024=> %d" <| challenge3a 1024

let inputChallenge3 = 277678

printfn "Solution 3A, challengeInput: %d => %d" inputChallenge3 <| challenge3a inputChallenge3

type Direction =
    | Up
    | Down
    | Left
    | Right

let spiral = Array2D.init 11 11 (fun x y -> if (x=5 && y=5) then 1 else 0)
let stopValue = inputChallenge3
///assuming array is big enough to not check borders for this for now:
let rec followSpiral (row,col) (dir:Direction) =

    let (nextRow, nextCol) = match dir with
                                | Up ->  (row-1, col)
                                | Down -> (row+1, col)
                                | Left -> (row, col-1)
                                | Right -> (row, col+1)

    spiral.[nextRow,nextCol] <- spiral.[nextRow-1,nextCol] 
                                        + spiral.[nextRow+1,nextCol] 
                                        + spiral.[nextRow,nextCol-1] 
                                        + spiral.[nextRow,nextCol+1] 
                                        + spiral.[nextRow-1,nextCol-1] 
                                        + spiral.[nextRow-1,nextCol+1]
                                        + spiral.[nextRow+1,nextCol-1] 
                                        + spiral.[nextRow+1,nextCol+1]

    let nextDir  = match dir with
                    | Up -> if (spiral.[nextRow,nextCol-1]=0) then Left else Up
                    | Down -> if (spiral.[nextRow,nextCol+1]=0) then Right else Down
                    | Left -> if (spiral.[nextRow+1,nextCol]=0) then Down else Left
                    | Right -> if (spiral.[nextRow-1,nextCol]=0) then Up else Right
    
    match stopValue< spiral.[nextRow,nextCol] with
    | true -> spiral.[nextRow,nextCol] 
    | false -> followSpiral (nextRow, nextCol) nextDir
    
let result = followSpiral (5,5) Right
printfn "Solution 3B, challengeInput: %d => %d" inputChallenge3 result