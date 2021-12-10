module Day16

open System;
open System.Diagnostics;
open System.IO;

type OpCode =
    | Addr
    | Addi
    | Mulr
    | Muli
    | Banr
    | Bani
    | Borr
    | Bori
    | Setr
    | Seti
    | Gtir
    | Gtri
    | Gtrr
    | Eqir
    | Eqri
    | Eqrr
;;

let parseOperation (input : string) : int * int * int * int =
    let parts = input.Split([| " " |], StringSplitOptions.RemoveEmptyEntries)
                |> Array.toList
                |> List.map Int32.Parse;

    (parts |> List.head
    , parts |> List.skip 1 |> List.head
    , parts |> List.skip 2 |> List.head
    , parts |> List.skip 3 |> List.head);

let parseRegisters (input : string) : int list =
    input.Substring(input.IndexOf('[') + 1, input.Length - input.IndexOf('[') - 2)
        .Split([| ", " |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList
    |> List.map Int32.Parse;    

let rec parseStates (input : string list) : (int list * (int * int * int * int) * int list) list =
    match input with
    | [] -> [];
    | ""::xs -> parseStates xs;
    | x::y::z::is -> (parseRegisters x, parseOperation y, parseRegisters z) :: (parseStates is);
    | _ -> failwith "Unable to parse input!";

let decode (maps : (int * OpCode) list) (instruction : int) : OpCode =
    maps
    |> List.find (fun (i, _) -> i = instruction)
    |> snd;

let getRegister (registers : int list) (location : int) : int =
    registers.[location];

let updateRegisters (registers : int list) (location : int) (value : int) : int list =
   registers
   |> List.mapi (fun i x -> if location = i then value else x);

let compareRegisters (reg1 : int list) (reg2 : int list) : bool =
    List.zip reg1 reg2
    |> List.fold (fun a (x, y) -> a && (x = y)) true;

let apply (registers : int list) ((param1, param2, param3) : int * int * int) (operation : OpCode) : int list =
    let getReg = getRegister registers;
    let updReg = updateRegisters registers;

    match operation with
    | Addr -> ((getReg param1) + (getReg param2)) |> updReg param3;
    | Addi -> ((getReg param1) + param2) |> updReg param3;
    | Mulr -> ((getReg param1) * (getReg param2)) |> updReg param3;
    | Muli -> ((getReg param1) * param2) |> updReg param3;
    | Banr -> ((getReg param1) &&& (getReg param2)) |> updReg param3;
    | Bani -> ((getReg param1) &&& param2) |> updReg param3;
    | Borr -> ((getReg param1) ||| (getReg param2)) |> updReg param3;
    | Bori -> ((getReg param1) ||| param2) |> updReg param3;
    | Setr -> (getReg param1) |> updReg param3;
    | Seti -> param1 |> updReg param3;
    | Gtir -> (if param1 > (getReg param2) then 1 else 0) |> updReg param3;
    | Gtri -> (if (getReg param1) > param2 then 1 else 0) |> updReg param3;
    | Gtrr -> (if (getReg param1) > (getReg param2) then 1 else 0) |> updReg param3;
    | Eqir -> (if param1 = (getReg param2) then 1 else 0) |> updReg param3;
    | Eqri -> (if (getReg param1) = param2 then 1 else 0) |> updReg param3;
    | Eqrr -> (if (getReg param1) = (getReg param2) then 1 else 0) |> updReg param3;

let rec operate (maps : (int * OpCode) list) (registers : int list) (instructions : (int * int * int * int) list) =
    match instructions with
    | [] -> registers;
    | (i, a, b, c)::is ->   is
                            |>  (decode maps i
                                |> apply registers (a, b, c)
                                |> operate maps);

let possibilities ((before, operation, after) : int list * (int * int * int * int) * int list) : int * OpCode list =
    let instructions = [ Addr; Addi; Mulr; Muli; Banr; Bani; Borr; Bori; Setr; Seti; Gtir; Gtri; Gtrr; Eqir; Eqri; Eqrr ]
    let (oi, oa, ob, oc) = operation;

    instructions
    |> List.map (fun i -> (i, apply before (oa, ob, oc) i))
    |> List.filter (fun (_, o) -> compareRegisters o after)
    |> List.map fst
    |> (fun x -> (oi, x));

let rec generateMap (maps : (int * OpCode) list) (possibilities : (int * OpCode list) list) : (int * OpCode) list =
    match possibilities with
    | [] -> maps;
    | _ ->  let solved, remainder = possibilities |> List.partition (fun (_, ps) -> List.length ps = 1);
            let maps' = maps @
                        (solved
                        |> List.map (fun (i, ps) -> (i, List.head ps)));
            let ops =   maps'
                        |> List.map snd;
            let poss' = remainder
                        |> List.map (fun (i, ps) -> (i, ps |> List.filter (fun p -> not (List.contains p ops))));
            generateMap maps' poss';
                       

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = Seq.toList(File.ReadLines(file))

    let test = [ "Before: [3, 2, 1, 1]";
                "9 2 1 2";
                "After:  [3, 2, 2, 1]" ;
                ""];

    let samples =   input
                    |> List.takeWhile (fun x -> x <> "#");

    let instrs =    input
                    |> List.skipWhile (fun x -> x <> "#")
                    |> List.skip 1
                    |> List.map parseOperation;
    
    if testMode then test else samples
    |> parseStates
    |> List.map possibilities
    |> List.filter (fun (_, x) -> List.length x >= 3)
    |> List.length
    |> printfn "Day 16, part 1: %d";

    let hints = samples
                |> parseStates
                |> List.map possibilities;

    let maps = hints
                |> List.groupBy (fun (i, _) -> i)
                |> List.map (fun (k, v) -> (k, v |> List.collect (fun (_, ps) -> ps) |> List.distinct))
                |> generateMap []
    instrs
    |> operate maps [0; 0; 0; 0]
    |> List.head
    |> printfn "Day 16, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
