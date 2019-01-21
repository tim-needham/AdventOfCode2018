module Day5

open System.Diagnostics;
open System.IO;

let step (c : char) (cs : char list) : char list =
    match cs with
    | x::xs when abs((int c) - (int x)) = 32 -> xs;
    | _ -> c::cs;

let react (cs : char list) : int =
    List.foldBack step cs []
    |> List.length;

let remove (c : char) (cs : char list) : char list =
    cs
    |> List.filter (fun x -> c <> x && not(abs((int c) - (int x)) = 32));

let optimise (cs : char list) : int =
    [65..90]
    |> List.map char
    |> List.map (fun x -> (x, remove x cs))
    |> List.map (fun (x, y) -> (x, react y))
    |> List.minBy (fun (_, y) -> y)
    |> snd

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = Seq.toList(File.ReadLines(file)).[0];

    let test = "dabAcCaCBAcCcaDA";

    if testMode then test else input
    |> Seq.toList
    |> react
    |> printfn "Day 5, part 1: %d";

    if testMode then test else input
    |> Seq.toList
    |> optimise
    |> printfn "Day 5, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;