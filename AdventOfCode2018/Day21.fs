module Day21

open System;
open System.IO;

let run (file : string) =

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (fun x -> Int32.Parse(x.ToString()));

    0
    |> printfn "Day 21, part 1: %d";

    0
    |> printfn "Day 21, part 2: %d";