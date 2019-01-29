module Day14

open System;
open System.Diagnostics;
open System.IO;

let brew (rs : int array) (e : int) (f : int) (i : int) : int array * int * int * int =
    let r = rs.[e] + rs.[f];
    let j = match r / 10 with
            | 0 ->  Array.set rs i r;
                    i + 1;
            | n ->  Array.set rs i n;
                    Array.set rs (i+1) (r%10);
                    i + 2;
    let g, h = (e + rs.[e] + 1) % j, (f + rs.[f] + 1) % j;
    rs, g, h, j;

let rec cook (rs : int array) (e : int) (f : int) (i : int) (t : int) : int array =
    match i with
    | n when n >= t + 10 -> rs.[t..(t+9)];
    | _ ->  let ss, g, h, j = brew rs e f i;
            cook ss g h j t;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = Seq.toList(File.ReadLines(file)).[0]
                |> Int32.Parse;

    let test = 9;

    let recipes = Array.zeroCreate<int> ((if testMode then test else input) + 12);
    Array.set recipes 0 3;
    Array.set recipes 1 7;

    if testMode then test else input
    |> cook recipes 0 1 2
    |> printfn "Day 14, part 1: %A";

    0
    |> printfn "Day 14, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;