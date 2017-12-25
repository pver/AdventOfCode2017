open System.Collections.Generic

type State = A|B|C|D|E|F

let startState = A
let checkSumAt = 12386363

let stateMachineStep (values:Dictionary<int,bool>) ((currentState,currentPos):State*int)=
    let currentValue =  let (_,y) = values.TryGetValue(currentPos)
                        y

    let write (b:bool) = values.[currentPos] <- b
    let toTheLeft = currentPos - 1
    let toTheRight = currentPos + 1
    
    match currentState with
    | A -> match currentValue with
           | false -> write true
                      (B, toTheRight)
           | true -> write false
                     (E, toTheLeft)

    | B -> match currentValue with
           | false -> write true
                      (C, toTheLeft)
           | true -> write false
                     (A, toTheRight)

    | C -> match currentValue with
           | false -> write true
                      (D, toTheLeft)
           | true -> write false
                     (C, toTheRight)

    | D -> match currentValue with
           | false -> write true
                      (E, toTheLeft)
           | true -> write false
                     (F, toTheLeft)

    | E -> match currentValue with
           | false -> write true
                      (A, toTheLeft)
           | true -> write true
                     (C, toTheLeft)

    | F -> match currentValue with
           | false -> write true
                      (E, toTheLeft)
           | true -> write true
                     (A, toTheRight)

let rec challengeA (steps:int) (values:Dictionary<int,bool>) ((currentState,currentPos):State*int) =
    match steps = 0 with
    | true -> values.Values |> Seq.filter id |> Seq.length
    | false -> challengeA (steps-1) values <| stateMachineStep values (currentState,currentPos)

challengeA checkSumAt (Dictionary<int,bool>()) (A,0) |> printfn "Checksum: %d"