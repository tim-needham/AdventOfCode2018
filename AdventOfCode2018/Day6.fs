module Day6

open System;
open System.Diagnostics;
open System.IO;

let parse (s : string) : int*int =
    match s.Split(',') with
    | [| x; y |] -> (Int32.Parse x, Int32.Parse y)
    | _ -> failwith "Invalid input for parsing";

let extent (cs : (int*int) list) : int*int =
    (cs |> List.maxBy fst |> fst, cs |> List.maxBy snd |> snd);

let manhattan ((px, py) : int*int) ((qx, qy) : int*int) : int =
    abs(px - qx) + abs(py - qy);

let distance (l : char) ((px, py) : int*int) ((ex, ey) : int*int) : (char*int*int*int) list =
    [ for j in [0..ey] do for i in [0..ex] -> (l, i, j, manhattan (px, py) (i, j)) ];

let closest (vs : (char*int*int*int) list) : char =
    let (l, _, _, m) = List.minBy (fun (_, _, _, d) -> d) vs;
    let d = vs |> List.filter (fun (_, _, _, d) -> d = m) |> List.length;
    if d = 1 then l else '.';

let removeBoundaries ((ex, ey) : int*int) (cs : ((int*int)*char) list) : ((int*int)*char) list =
    let bs = cs
            |> List.filter (fun ((x, y), l) -> (x = 0 || y = 0 || x = ex || y = ey) && l <> '.')
            |> List.map (fun (_, l) -> l)
            |> List.distinct;
    
    cs
    |> List.map (fun (c, l) -> if List.contains l bs then (c, '.') else (c, l))

let plot1 (cs : (int*int) list) : ((int*int)*char) list =
    let e = extent cs;
    [0..cs.Length-1]
    |> List.map (fun c -> char (c+65))
    |> List.zip cs
    |> List.map (fun (c, l) -> distance l c e)
    |> List.concat
    |> List.groupBy (fun (_, x, y, _) -> (x, y))
    |> List.map (fun (g, vs) -> (g, closest vs))
    |> removeBoundaries e;

let plot2 (cs : (int*int) list) : ((int*int)*int) list =
    let (ex, ey) = extent cs;
    [ for j in [0..ey] do 
        for i in [0..ex] -> 
            let d = cs |> List.map (fun c -> manhattan c (i, j)) |> List.sum;
            ((i, j), d);
        ];

let largest (ps : ((int * int) * char) list) : int =
    ps
    |> List.filter (fun (_, l) -> l <> '.')
    |> List.groupBy (fun (_, l) -> l)
    |> List.maxBy (fun (_, is) -> is.Length)
    |> snd
    |> List.length;

let safe (m : int) (ps : ((int * int) * int) list) : int =
    ps
    |> List.filter (fun (_, d) -> d < m)
    |> List.length;

let prettyPrint (ps : ((int * int) * 'a) list) : unit =
    let (ex, ey) = ps |> List.map (fun (p, _) -> p)
                |> extent;

    for j in [0..ey] do
        for i in [0..ex] do
            let c = ps |> List.find (fun ((x, y), _) -> x = i && y = j) |> snd;
            printf "%A" c;
        printfn "";

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = Seq.toList(File.ReadLines(file))
                |> List.map parse;

    let test = [ "1, 1";
                "1, 6";
                "8, 3";
                "3, 4";
                "5, 5";
                "8, 9" ]
                |> List.map parse;

    if testMode then test else input
    |> plot1
    |> largest
    |> printfn "Day 6, part 1: %d";

    if testMode then test else input
    |> plot2
    |> safe (if testMode then 32 else 10000)
    |> printfn "Day 6, part 2: %A";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
