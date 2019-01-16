module Day1

open System;
open System.IO;

let produce (i : int) (is : int list) : int * int =
    if i < (List.length is)-1 then
        (i+1, is.[i]);
    else
        (0, is.[i])

let rec sample (f : int) (i : int) (is : int list) (m : Map<int, bool>) : int =
    let (i', v) = produce i is;
    let f' = f + v;
    if Map.containsKey f' m then
        f';
    else
        let m' = Map.add f' true m;
        sample f' i' is m';

let run (file : string) =

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (fun x -> Int32.Parse(x.ToString()));

    input
    |> List.fold (fun a x -> a + x) 0
    |> printfn "Day 1, part 1: %d";

    sample 0 0 input (new Map<int, bool>([]))
    |> printfn "Day 1, part 2: %d";