let numberOfStepsChallenge = 345
let numberOfStepsExample = 3

let rec add (totalCount:int) (currentPos:int) (steps:int) (spinlock:int list) =

    let sizeSoFar = spinlock.Length
    let insertPosition = (currentPos + (steps%sizeSoFar))%sizeSoFar

    let firstPart = spinlock |> List.take (insertPosition+1)
    let secondPart = spinlock |> List.skip (insertPosition+1)
    
    let result = firstPart @ [sizeSoFar] @ secondPart
    match result.Length = totalCount with
    | true -> result
    | false -> add totalCount (insertPosition+1) steps result

let resultExample = add 10 0 numberOfStepsExample [0]
printfn "Example A: %A" resultExample

let resultChallengeA = add 2018 0 numberOfStepsChallenge [0]
//printfn "Result challenge17A: %A" resultChallengeA
let positionAfter2017 = (resultChallengeA |> List.findIndex ((=)2017))+1
printfn "Challenge A result value after 2017=%d" (resultChallengeA |> List.item positionAfter2017  )

let rec challengeB (totalCount:int) (currentPos:int) (steps:int) (pos0:int) (valueAfter0:int) (sizeSoFar:int) =
    let insertPosition = ((currentPos + (steps%sizeSoFar))%sizeSoFar) + 1

    let newPos0 = if (insertPosition = pos0) then pos0 + 1 else pos0
    let newValueAfter0 = if (insertPosition = pos0 + 1) then sizeSoFar else valueAfter0

    match sizeSoFar = totalCount with
    | true -> valueAfter0
    | false -> challengeB totalCount insertPosition steps newPos0 newValueAfter0 (sizeSoFar+1)

printfn "-------------------------------"
let resultExampleB = challengeB 10 0 numberOfStepsExample 0 0 1
printfn "Example B: %A" resultExampleB

let resultChallengeB = challengeB 50000000 0 numberOfStepsChallenge 0 0 1
printfn "Challenge B: %A" resultChallengeB