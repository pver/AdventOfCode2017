// You discover a tablet containing some strange assembly code labeled simply "Duet". Rather than bother the sound card with it, you decide to run the code yourself. Unfortunately, you don't see any documentation, so you're left to figure out what the instructions mean on your own.

// It seems like the assembly is meant to operate on a set of registers that are each named with a single letter and that can each hold a single integer. You suppose each register should start with a value of 0.

// There aren't that many instructions, so it shouldn't be hard to figure out what they do. Here's what you determine:

// snd X plays a sound with a frequency equal to the value of X.
// set X Y sets register X to the value of Y.
// add X Y increases register X by the value of Y.
// mul X Y sets register X to the result of multiplying the value contained in register X by the value of Y.
// mod X Y sets register X to the remainder of dividing the value contained in register X by the value of Y (that is, it sets X to the result of X modulo Y).
// rcv X recovers the frequency of the last sound played, but only when the value of X is not zero. (If it is zero, the command does nothing.)
// jgz X Y jumps with an offset of the value of Y, but only if the value of X is greater than zero. (An offset of 2 skips the next instruction, an offset of -1 jumps to the previous instruction, and so on.)
// Many of the instructions can take either a register (a single letter) or a number. The value of a register is the integer it contains; the value of a number is that number.

// After each jump instruction, the program continues with the instruction to which the jump jumped. After any other instruction, the program continues with the next instruction. Continuing (or jumping) off either end of the program terminates it.

// For example:

// set a 1
// add a 2
// mul a a
// mod a 5
// snd a
// set a 0
// rcv a
// jgz a -1
// set a 1
// jgz a -2
// The first four instructions set a to 1, add 2 to it, square it, and then set it to itself modulo 5, resulting in a value of 4.
// Then, a sound with frequency 4 (the value of a) is played.
// After that, a is set to 0, causing the subsequent rcv and jgz instructions to both be skipped (rcv because a is 0, and jgz because a is not greater than 0).
// Finally, a is set to 1, causing the next jgz instruction to activate, jumping back two instructions to another jump, which jumps again to the rcv, which ultimately triggers the recover operation.
// At the time the recover operation is executed, the frequency of the last sound played is 4.

// // What is the value of the recovered frequency (the value of the most recently played sound) the first time a rcv instruction is executed with a non-zero value?


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
    | Rcv of Value
    | Jgz of char * Value

type State = {nextInstruction:int64; lastSndFreq:int64; registers:Dictionary<char,int64>}


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
    | "rcv" -> Rcv (asValue inputParts.[1]) 
    // jgz X Y jumps with an offset of the value of Y, but only if the value of X is greater than zero. (An offset of 2 skips the next instruction, an offset of -1 jumps to the previous instruction, and so on.)
    | "jgz" -> Jgz (asRegisterChar inputParts.[1], asValue inputParts.[2]) 
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

let processInstruction (state:State) (instr:Operation) =
    let resolve (v:Value) =
        match v with
        | Register r -> if (not (state.registers.ContainsKey(r))) then state.registers.Add(r,0L)
                        state.registers.[r]
        | Int i -> (int64) i
    
    match instr with 
    | Set (X,Y) -> state.registers.[X] <- resolve Y
                   {state with nextInstruction=state.nextInstruction+1L}
    | Add (X, Y) -> state.registers.[X] <- resolve (Register X) + (resolve Y)
                    {state with nextInstruction=state.nextInstruction+1L} 
    | Mul (X, Y) -> state.registers.[X] <- resolve (Register X) * (resolve Y)
                    {state with nextInstruction=state.nextInstruction+1L} 
    | Mod (X, Y) -> state.registers.[X] <- resolve (Register X) % (resolve Y)
                    {state with nextInstruction=state.nextInstruction+1L} 
    | Snd (X) -> printfn "SND: %d" (resolve X)
                 {state with nextInstruction=state.nextInstruction+1L;lastSndFreq=(resolve X)} 
    | Rcv (X) -> match resolve X with
                    | 0L -> ()
                    | x -> printfn "RCV %d" state.lastSndFreq
                 {state with nextInstruction=state.nextInstruction+1L} 
    | Jgz (X, Y) -> match resolve (Register X) with
                    | 0L -> {state with nextInstruction=state.nextInstruction+1L} 
                    | _ -> {state with nextInstruction=state.nextInstruction+(resolve Y)} 
    | _ -> failwith "invalid instruction to process"

let startState() = {nextInstruction=0L;lastSndFreq=0L;registers=new Dictionary<char,int64>()}
printfn "%A" <| processInstruction (processInstruction (startState()) (instructionsExample.[0])) (instructionsExample.[1])

let rec run (state:State) (instructions:Operation[])=
    //printfn "next instr: %d" state.nextInstruction
    if state.nextInstruction >= (int64)instructions.Length then state
    else run (processInstruction state instructions.[(int)state.nextInstruction]) instructions

//run (startState()) instructionsExample |> printfn "End state example: %A"

run (startState()) instructionsChallenge |> printfn "End state example: %A"