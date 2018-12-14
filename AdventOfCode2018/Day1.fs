module Day1

open System;
open System.IO;

let produce (ps : int list) (qs : int list) : (int * int list) =
    match ps with
    | [] -> (List.head qs, List.tail qs);
    | x::xs -> (x, xs);

let apply (t : int) (ps : int list) (qs : int list) : (int * int list) =
    let (p, ps') = produce ps qs;
    (t + p, ps);

let rec sample (t : int) (fs : int list) (ps : int list) (qs : int list) : int =
    let (t', ps') = apply t ps qs;
    match List.tryFind(fun x -> x = t') fs with
    | Some _ -> t';
    | None -> sample t' (t'::fs) ps' qs;

let run (file : string) =

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (fun x -> Int32.Parse(x.ToString()));

    input
    |> List.fold (fun a x -> a + x) 0
    |> printfn "Day 1, part 1: %d";

    sample 0 [] input input
    |> printfn "Day 1, part 2: %d";