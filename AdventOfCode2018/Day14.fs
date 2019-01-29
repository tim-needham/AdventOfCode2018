module Day14

open System;
open System.Diagnostics;
open System.IO;

let digits (r : int) : int array =
    match r / 10 with
    | 0 -> [| r |];
    | n -> [| n; r%10 |];

let brew (rs : int array) (e : int) (f : int) : int array * int * int =
    let r = rs.[e] + rs.[f];
    let ss = (digits r) |> Array.append rs;
    let g, h = (e + rs.[e] + 1) % ss.Length, (f + rs.[f] + 1) % ss.Length;
    ss, g, h;

let rec cook (rs : int array) (e : int) (f : int) (t : int) : int list =
    match rs.Length with
    | n when n >= t + 10 -> rs
                            |> Seq.toList
                            |> List.take (t + 10)
                            |> List.skip t;
    | _ ->  let ss, g, h = brew rs e f;
            //printfn "%A, %A"  ss fs;
            cook ss g h t;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = Seq.toList(File.ReadLines(file)).[0]
                |> Int32.Parse;

    let test = 9;

    if testMode then test else input
    |> cook [| 3; 7 |] 0 1
    |> printfn "Day 14, part 1: %A";

    0
    |> printfn "Day 14, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;