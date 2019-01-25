module Day11

open System;
open System.Diagnostics;
open System.IO;

let power (x : int) (y : int) (s : int) : int =
    let r = x + 10;
    let p = ((r * y) + s) * r;
    let p' = p / 100;
    let q = (p' / 10) * 10;
    p' - q - 5;

let generate (s : int) : int list list =
    [ for j in [1..300] ->
        [ for i in [1..300] ->
            power i j s
        ]
    ];

let powers (n : int) (cs : int list list) : (int * int * int) list =
    [ for j in [0..(300-n)] do
        for i in [0..(300-n)] ->
            [for y in [j..j+n-1] do
                for x in [i..i+n-1] ->
                    cs.[y].[x]]
            |> List.sum
            |> (fun s -> (i+1, j+1, s))
    ];

let power3 (cs : int list list) : (int * int) =
    powers 3 cs
    |> List.maxBy (fun (_, _, z) -> z)
    |> (fun (x, y, _) -> (x, y));

let fastPowers (n : int) (ps : int list list) (cs : int list list) : int list list =
    [ for j in [0..(300-n)] ->
        [ for i in [0..(300-n)] ->
            // Don't count the corner twice...
            ps.[j].[i] + ([for y in [j..j+n-2] -> cs.[y].[i+n-1]]
                            |> List.append [for x in [i..i+n-1] -> cs.[j+n-1].[x]]
                            |> List.sum)
        ]
    ];
    

let rec recursivePowers (n : int) (cs : int list list) : int list list list =
    match n with
    | 1 -> [cs];
    | x when x > 1 ->   let rs = recursivePowers (n-1) cs;
                        let ps = List.head rs;
                        (fastPowers x ps cs)::rs;

let allPowers (cs : int list list) : int*int*int=
    let ps =    recursivePowers 300 cs
                |> List.rev;
    let rs =    ps
                |> List.zip [1..300]
                |> List.map (fun (n, xs) -> xs
                                            |> List.zip [1..300-n+1]
                                            |> List.map (fun (o, ys) -> ys
                                                                        |> List.zip [1..300-n+1]
                                                                        |> List.map (fun (p, z) -> (p, o, z, n)))
                                            |> List.concat)
                |> List.concat;
    rs
    |> List.maxBy (fun (_, _, z, _) -> z)
    |> (fun (x, y, _, n) -> (x, y, n));

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = Seq.toList(File.ReadLines(file)).[0]
                |> Int32.Parse;

    let test = 18;

    let g = if testMode then test else input 
            |> generate;
    
    g
    |> power3
    |> printfn "Day 11, part 1: %A";

    g
    |> allPowers
    |> printfn "Day 11, part 2: %A";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;