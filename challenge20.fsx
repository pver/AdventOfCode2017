open System
open System.Numerics

let exampleInput = "p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>
p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>"

type State = {id:int; p:int*int*int; v:int*int*int; a:int*int*int}


let parseLines (lines:string[]) =
    lines 
    |> Array.map (fun x ->  x.Split([|">"|], StringSplitOptions.RemoveEmptyEntries)
                            |> Array.map (fun y -> y.Split([|"<"|], StringSplitOptions.RemoveEmptyEntries)|>Array.skip(1)|>Array.head|> fun z->z.Split([|","|], StringSplitOptions.RemoveEmptyEntries))|>Array.map (Array.map int)
    )
    |> Array.mapi (fun i x-> {id=i; p=(x.[0].[0],x.[0].[1],x.[0].[2]); v=(x.[1].[0],x.[1].[1],x.[1].[2]); a=(x.[2].[0],x.[2].[1],x.[2].[2])})

let parse (input:string) =
    parseLines <| input.Split([|System.Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries)


let closestOnLongTerm (states:State[]) =
    states
    |> Array.minBy (fun x-> let (x,y,z) = x.a
                            x*x + y*y + z*z)

parse exampleInput |> printfn "Example parse result: %A"
parse exampleInput |> closestOnLongTerm |> printfn "Example closest long term: %A"

let challengeInput = System.IO.File.ReadAllLines("inputChallenge20.txt")
//parseLines challengeInput |> printfn "Challenge parse result: %A"
parseLines challengeInput |> closestOnLongTerm |> printfn "Challenge closest long term: %A"


let tickUpdate (state:State) =
    let (aX,aY,aZ) = state.a
    let (vX,vY,vZ) = state.v
    let (pX,pY,pZ) = state.p
    let (newVX, newVY, newVZ) = (vX+aX,vY+aY,vZ+aZ)
    {state with p=(pX+newVX,pY+newVY,pZ+newVZ);v=(newVX, newVY, newVZ)}

//parse exampleInput |> Array.map tickUpdate |> printfn "Example after tic: %A"

let tickAndCleanUp (states:State[]) = 
    let newStates = states |> Array.map tickUpdate
    newStates 
    |> Seq.groupBy (fun s -> s.p) 
    |> Seq.filter (fun (_, items) -> (items|>Seq.length) <=1) 
    |> Seq.map (fun (_, items) -> (items|>Seq.head))
    |> Seq.toArray

let rec numberOfParticlesDontColliding (triesLeft:int) (states:State[]) =
    match triesLeft with
    | 0 -> states.Length
    | t -> numberOfParticlesDontColliding (t-1) <| tickAndCleanUp states

let exampleInput2 = "p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>
p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>
p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>
p=<3,0,0>, v=<-1,0,0>, a=<0,0,0>"
numberOfParticlesDontColliding 20 (parse exampleInput2) |> printfn "Particles not colliding long term in example: %d"

parseLines challengeInput |> numberOfParticlesDontColliding 10000 |> printfn "Particles not colliding long term in challenge: %A"