module Day1

open System;
open System.IO;

let produce (i : int) (is : int list) : int * int =
    if i < (List.length is)-1 then
        (i+1, is.[i]);
    else
        (0, is.[i])

let rec sample (f : int) (i : int) (m : Map<int, bool>) (is : int list) : int =
    let (i', v) = produce i is;
    let f' = f + v;
    if Map.containsKey f' m then
        f';
    else
        let m' = Map.add f' true m;
        sample f' i' m' is;

let run (file : string, testMode : bool) =

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (fun x -> Int32.Parse(x));

    let test = [ "+1"; "-2"; "+3"; "+1" ]
                |> List.map (fun x -> Int32.Parse(x));

    if testMode then test else input
    |> List.fold (fun a x -> a + x) 0
    |> printfn "Day 1, part 1: %d";

    if testMode then test else input
    |> sample 0 0 (new Map<int, bool>([]))
    |> printfn "Day 1, part 2: %d";