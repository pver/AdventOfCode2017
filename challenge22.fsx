open System

let inputExample = "..#
#..
..."

let newGrid (input:string) (dimension:int) = 
    let lines = input.Split([|System.Environment.NewLine|],StringSplitOptions.RemoveEmptyEntries)
    let l = lines.Length
    
    let inputGrid = Array2D.init l l (fun r c -> lines.[r].[c])

    let grid = Array2D.init dimension dimension (fun _ _ -> '.')
    let gridCenter = dimension/2
    let inputGridCenter = l/2
    grid.[gridCenter-inputGridCenter..gridCenter+inputGridCenter,gridCenter-inputGridCenter..gridCenter+inputGridCenter] <- inputGrid
    grid


type Direction =
    | Up
    | Down
    | Left
    | Right

let nextPos ((row,col):int*int) (dir:Direction) = 
    let (nextRow, nextCol) = 
        match dir with
        | Up ->  (row-1, col)
        | Down -> (row+1, col)
        | Left -> (row, col-1)
        | Right -> (row, col+1)
    (nextRow, nextCol)

let walkGrid (grid:char[,]) ((row,col):int*int) (dir:Direction) (infectionsDone:int) =
    match grid.[row,col] with 
    | '.' -> // clean, turn left
             let newDirection = match dir with
                                | Up ->  Left
                                | Down -> Right
                                | Left -> Down
                                | Right -> Up
             grid.[row,col] <- '#'
             let newPos = nextPos (row,col) newDirection
             (newPos, newDirection, (infectionsDone+1))

    | '#' -> // infected, turn right
             let newDirection = match dir with
                                | Up ->  Right
                                | Down -> Left
                                | Left -> Up
                                | Right -> Down
             grid.[row,col] <- '.'
             let newPos = nextPos (row,col) newDirection
             (newPos, newDirection, infectionsDone)
    | _ -> failwith "invalid grid data"


let rec walkGridIterations (iterations:int) (grid:char[,]) ((row,col):int*int) (dir:Direction) (infectionsDone:int) =
    match iterations with 
    | 0 -> ((row,col), dir, infectionsDone)
    | i ->  let (newPos, newDirection, newInfectionsDone) = walkGrid grid (row,col) dir infectionsDone
            walkGridIterations (i-1) grid newPos newDirection newInfectionsDone

let exampleGrid = newGrid inputExample 1001 

let exampleAfter70Iterations = walkGridIterations 10000 exampleGrid (500,500) Up 0
exampleAfter70Iterations |> printfn "Example after 70 iterations: grid: %A %A" exampleGrid

let challengeInput = (System.IO.File.ReadAllText("inputChallenge22.txt"))
let challengeAGrid = newGrid challengeInput 1001 

let challengeA = walkGridIterations 10000 challengeAGrid (500,500) Up 0
challengeA |> printfn "Challenge after 10000 iterations: %A" 
