open System
open System.Collections.Generic

type Value =
    | Int of int64
    | Register of char

type Operation =
    | Set of char * Value
    | Add of char * Value
    | Sub of char * Value
    | Mul of char * Value
    | Mod of char * Value
    | Snd of Value
    | Rcv of char
    | Jgz of Value * Value
    | Jnz of Value * Value

type MsgQueue = Dictionary<int,int64 list>
type State = {nextInstruction:int64; registers:Dictionary<char,int64>; instrCount:Dictionary<string,int64>}


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
    // sub X Y decreases register X by the value of Y.
    | "sub" -> Sub (asRegisterChar inputParts.[1], asValue inputParts.[2]) 
    // mul X Y sets register X to the result of multiplying the value contained in register X by the value of Y.
    | "mul" -> Mul (asRegisterChar inputParts.[1], asValue inputParts.[2]) 
    // mod X Y sets register X to the remainder of dividing the value contained in register X by the value of Y (that is, it sets X to the result of X modulo Y).
    | "mod" -> Mod (asRegisterChar inputParts.[1], asValue inputParts.[2]) 
    // rcv X recovers the frequency of the last sound played, but only when the value of X is not zero. (If it is zero, the command does nothing.)
    | "rcv" -> Rcv (asRegisterChar inputParts.[1]) 
    // jgz X Y jumps with an offset of the value of Y, but only if the value of X is greater than zero. (An offset of 2 skips the next instruction, an offset of -1 jumps to the previous instruction, and so on.)
    | "jgz" -> Jgz (asValue inputParts.[1], asValue inputParts.[2]) 
    // jnz X Y jumps with an offset of the value of Y, but only if the value of X is not zero. (An offset of 2 skips the next instruction, an offset of -1 jumps to the previous instruction, and so on.)
    | "jnz" -> Jnz (asValue inputParts.[1], asValue inputParts.[2]) 
    | i -> failwith (sprintf "invalid instruction found: %s" i)


let instructionsChallenge = System.IO.File.ReadAllLines("inputChallenge23.txt") |> Array.map parse


let processInstruction (state:State) (instr:Operation) (queues:MsgQueue) =
    let resolve (v:Value) =
        match v with
        | Register r -> if (not (state.registers.ContainsKey(r))) then state.registers.Add(r,0L)
                        state.registers.[r]
        | Int i -> (int64) i

    match instr with 
    | Set (X,Y) -> state.registers.[X] <- resolve Y
                   state.instrCount.["Set"] <- state.instrCount.["Set"] + 1L
                   {state with nextInstruction=state.nextInstruction+1L}
    | Add (X, Y) -> state.registers.[X] <- (resolve (Register X)) + (resolve Y)
                    state.instrCount.["Add"] <- state.instrCount.["Add"] + 1L
                    {state with nextInstruction=state.nextInstruction+1L} 
    | Sub (X, Y) -> state.registers.[X] <- (resolve (Register X)) - (resolve Y)
                    state.instrCount.["Sub"] <- state.instrCount.["Sub"] + 1L
                    {state with nextInstruction=state.nextInstruction+1L} 
    | Mul (X, Y) -> state.registers.[X] <- (resolve (Register X)) * (resolve Y)
                    state.instrCount.["Mul"] <- state.instrCount.["Mul"] + 1L
                    {state with nextInstruction=state.nextInstruction+1L} 
    | Mod (X, Y) -> state.registers.[X] <- (resolve (Register X)) % (resolve Y)
                    state.instrCount.["Mod"] <- state.instrCount.["Mod"] + 1L
                    {state with nextInstruction=state.nextInstruction+1L} 
    | Snd (X) -> 
                 // nop
                 state.instrCount.["Snd"] <- state.instrCount.["Snd"] + 1L
                 {state with nextInstruction=state.nextInstruction+1L} 
    | Rcv (X) -> state.instrCount.["Rcv"] <- state.instrCount.["Rcv"] + 1L
                 //nop
                 {state with nextInstruction=state.nextInstruction+1L}
    | Jgz (X, Y) -> state.instrCount.["Jgz"] <- state.instrCount.["Jgz"] + 1L
                    match (resolve X) > 0L with
                    | false -> {state with nextInstruction=state.nextInstruction+1L} 
                    | true ->   let offset = (resolve Y)
                                {state with nextInstruction=state.nextInstruction+offset} 
    | Jnz (X, Y) -> state.instrCount.["Jnz"] <- state.instrCount.["Jnz"] + 1L
                    match (resolve X) <> 0L with
                    | false -> {state with nextInstruction=state.nextInstruction+1L} 
                    | true ->   let offset = (resolve Y)
                                {state with nextInstruction=state.nextInstruction+offset} 


let startState () = 
    let state = {nextInstruction=0L;registers=new Dictionary<char,int64>();instrCount=new Dictionary<string,int64>()}
    state.instrCount.["Set"] <- 0L
    state.instrCount.["Add"] <- 0L
    state.instrCount.["Sub"] <- 0L
    state.instrCount.["Mul"] <- 0L
    state.instrCount.["Mod"] <- 0L
    state.instrCount.["Snd"] <- 0L
    state.instrCount.["Rcv"] <- 0L
    state.instrCount.["Jgz"] <- 0L
    state.instrCount.["Jnz"] <- 0L
    state

let rec solve (state:State) (instructions:Operation[]) (queues:MsgQueue) =
    if state.nextInstruction < 0L || state.nextInstruction >= (int64)instructions.Length then state
    else    
            let newState = processInstruction state instructions.[(int)state.nextInstruction] queues
            solve newState instructions queues

let newQueues() =
    let d = new Dictionary<int,int64 list>()
    d.[0] <- []
    d.[1] <- []
    d

solve (startState()) instructionsChallenge (newQueues()) |> fun x -> x.instrCount.["Mul"] |> printfn "Challenge 23 A, Mul count = %d"

let isPrime n =
    let rec check i =
        i > n/2 || (n % i <> 0 && check (i + 1))
    check 2

let b = 107900
let c = 124900

seq { b .. 17 .. c } |> Seq.fold (fun acc x -> if (not (isPrime(x))) then (acc + 1) else acc) 0 |> printfn "Challenge 23 B, reg 'h' = %d"
