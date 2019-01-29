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

let success (t : string) (s : int) (l : int) (ar : int array) : bool = 
    let c = ar.[s..(s+l-1)]
            |> Array.fold (fun a x -> a + x.ToString()) "";
    t.Equals(c);

let rec recook (rs : int array) (e : int) (f : int) (i : int) (t : string) : int =
    let l = t.Length;
    if i > l && success t (i-l) l rs then
        (i-l);
    else if i > l + 1 && success t (i-l-1) l rs then
        (i-l-1);
    else
        let ss, g, h, j = brew rs e f i;
        recook ss g h j t;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = Seq.toList(File.ReadLines(file)).[0]
                |> Int32.Parse;

    let test = 9;
    let test2 = 51589;

    let recipes = Array.zeroCreate<int> ((if testMode then test else input) + 12);
    Array.set recipes 0 3;
    Array.set recipes 1 7;

    let recipes2 = Array.zeroCreate<int> ((if testMode then test else 100*input) + 12);
    Array.set recipes2 0 3;
    Array.set recipes2 1 7;

    if testMode then test else input
    |> cook recipes 0 1 2
    |> printfn "Day 14, part 1: %A";

    if testMode then test2 else input
    |> (fun x -> x.ToString())
    |> recook recipes2 0 1 2
    |> printfn "Day 14, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;