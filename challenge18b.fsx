open System
open System.Collections.Generic

type Value =
    | Int of int64
    | Register of char

type Operation =
    | Set of char * Value
    | Add of char * Value
    | Mul of char * Value
    | Mod of char * Value
    | Snd of Value
    | Rcv of char
    | Jgz of Value * Value

type MsgQueue = Dictionary<int,int64 list>
type State = {programId:int; nextInstruction:int64; registers:Dictionary<char,int64>; sendCount:int}


let parse (input:string) =
    let inputParts = input.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)

    let asRegisterChar (s:string) =
        s.ToCharArray()|>Seq.head

    let asRegister (s:string) =
        Register (s.ToCharArray()|>Seq.head)

    let asValue (s:string) =
        match s.Length with
        | 1 ->  match Int64.TryParse(s) with
                | (true, x) -> Int x
                | _ -> asRegister s
        | _ -> Int ((int64) s)
 
    match inputParts.[0] with
    // snd X plays a sound with a frequency equal to the value of X.
    | "snd" -> Snd (asValue inputParts.[1]) 
    // set X Y sets register X to the value of Y.
    | "set" -> Set (asRegisterChar inputParts.[1], asValue inputParts.[2]) 
    // add X Y increases register X by the value of Y.
    | "add" -> Add (asRegisterChar inputParts.[1], asValue inputParts.[2]) 
    // mul X Y sets register X to the result of multiplying the value contained in register X by the value of Y.
    | "mul" -> Mul (asRegisterChar inputParts.[1], asValue inputParts.[2]) 
    // mod X Y sets register X to the remainder of dividing the value contained in register X by the value of Y (that is, it sets X to the result of X modulo Y).
    | "mod" -> Mod (asRegisterChar inputParts.[1], asValue inputParts.[2]) 
    // rcv X recovers the frequency of the last sound played, but only when the value of X is not zero. (If it is zero, the command does nothing.)
    | "rcv" -> Rcv (asRegisterChar inputParts.[1]) 
    // jgz X Y jumps with an offset of the value of Y, but only if the value of X is greater than zero. (An offset of 2 skips the next instruction, an offset of -1 jumps to the previous instruction, and so on.)
    | "jgz" -> Jgz (asValue inputParts.[1], asValue inputParts.[2]) 
    | _ -> failwith "invalid instruction found"


let instructionsChallenge = System.IO.File.ReadAllLines("inputChallenge18.txt") |> Array.map parse

let exammpleInput = "set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2"

let instructionsExample = exammpleInput.Split([|System.Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries) |> Array.map parse 

instructionsExample |> printfn "Parsed example: %A"
instructionsChallenge |>  printfn "Parsed challenge: %A"

let processInstruction (state:State) (instr:Operation) (queues:MsgQueue) =
    let resolve (v:Value) =
        match v with
        | Register r -> if (not (state.registers.ContainsKey(r))) then state.registers.Add(r,0L)
                        state.registers.[r]
        | Int i -> (int64) i
    let otherProgramId = (state.programId + 1) % 2
    match instr with 
    | Set (X,Y) -> state.registers.[X] <- resolve Y
                   {state with nextInstruction=state.nextInstruction+1L}
    | Add (X, Y) -> state.registers.[X] <- (resolve (Register X)) + (resolve Y)
                    {state with nextInstruction=state.nextInstruction+1L} 
    | Mul (X, Y) -> state.registers.[X] <- (resolve (Register X)) * (resolve Y)
                    {state with nextInstruction=state.nextInstruction+1L} 
    | Mod (X, Y) -> state.registers.[X] <- (resolve (Register X)) % (resolve Y)
                    {state with nextInstruction=state.nextInstruction+1L} 
    | Snd (X) -> //printfn "queued: %A process: %A" queues.[otherProgramId] (queues.[otherProgramId] @ [(resolve X)])
                 queues.[otherProgramId] <- (queues.[otherProgramId] @ [(resolve X)])
                 {state with nextInstruction=state.nextInstruction+1L;sendCount=state.sendCount+1} 
    | Rcv (X) -> match queues.[state.programId] with
                 | [] -> state 
                 | h::tail -> 
                              state.registers.[X] <- h
                              queues.[state.programId] <- tail
                              {state with nextInstruction=state.nextInstruction+1L}
    | Jgz (X, Y) -> match (resolve X) > 0L with
                    | false -> {state with nextInstruction=state.nextInstruction+1L} 
                    | true ->   let offset = (resolve Y)
                                if offset = 0L then printfn "offset 0"
                                {state with nextInstruction=state.nextInstruction+offset} 


let startState (programId:int) = 
    let state = {programId=programId;nextInstruction=0L;registers=new Dictionary<char,int64>();sendCount=0}
    state.registers.['p'] <- (int64)programId
    printfn "state: %A" state
    state


//run (startState()) instructionsExample |> printfn "End state example: %A"
let rec solve (state0:State) (state1:State) (instructions:Operation[]) (queues:MsgQueue) =
    let instrP0 = state0.nextInstruction
    let instrP1 = state1.nextInstruction

    let newState0 = if state0.nextInstruction >= (int64)instructions.Length 
                    then state0
                    else processInstruction state0 instructions.[(int)instrP0] queues
    let newState1 = if state1.nextInstruction >= (int64)instructions.Length 
                    then state1
                    else processInstruction state1 instructions.[(int)instrP1] queues

    match newState0.nextInstruction = instrP0 && newState1.nextInstruction = instrP1 with
    | true -> state1.sendCount // both stopped or deadlock
    | false ->  solve newState0 newState1 instructions queues

let newQueues() =
    let d = new Dictionary<int,int64 list>()
    d.[0] <- []
    d.[1] <- []
    d
solve (startState(0)) (startState(1)) instructionsChallenge (newQueues())|> printfn "End state challenge B: %A"