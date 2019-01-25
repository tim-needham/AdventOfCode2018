module Day10

open System;
open System.Diagnostics;
open System.IO;

type Point = {
        X : int;
        Y : int;
        DX : int;
        DY : int
    };;

let parse (s : string) : Point =
    match s.Replace("position=<", "").Replace("> velocity=<", ",").Replace(">", "").Split(',') with
    | [| x; y ; dx; dy |]  -> { X = Int32.Parse x; Y = Int32.Parse y; DX = Int32.Parse dx; DY = Int32.Parse dy };
    | a -> failwith "Invalid input for parsing";

let fst ((x, _, _) : 'a*'b*'c) : 'a =
    x;

let snd ((_, y, _) : 'a*'b*'c) : 'b =
    y;

let thd ((_, _, z) : 'a*'b*'c) : 'c =
    z;

let tick (ps : Point list) : Point list =
    ps
    |> List.map (fun x -> { X = x.X + x.DX; Y = x.Y + x.DY; DX = x.DX; DY = x.DY })

let bounds (ps : Point list) : (int*int)*int*int =
    let b = List.head ps;

    ps
    |> List.fold (fun (ax, ay, bx, by) p -> (Math.Min(ax, p.X), Math.Min(ay, p.Y), Math.Max(bx, p.X), Math.Max(by, p.Y))) (b.X, b.Y, b.X, b.Y)
    |> (fun (ax, ay, bx, by) -> ((ax, ay), bx-ax, by-ay));

let rec move (b : (int*int)*int*int) (i : int) (ps : Point list) : int * Point list=
    let ps' = tick ps;
    let b' = bounds ps';

    if snd b' >= snd b || thd b' >= thd b then
        i, ps;
    else
        move b' (i+1) ps';

let plot (ps : Point list) : unit =
    let b = bounds ps;
    let (ax, ay) = fst b;

    let ps' =   ps
                |> List.map (fun p -> (p.X - ax, p.Y - ay));
    
    for j in [0..(thd b)] do 
        for i in [0..(snd b)] do
            match ps' |> List.tryFind (fun (p, q) -> p = i && q = j) with
            | None -> printf " ";
            | Some _ -> printf "*";
        printfn "";


let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = Seq.toList(File.ReadLines(file))
                |> List.map parse;

    let test = ["position=< 9,  1> velocity=< 0,  2>";
                "position=< 7,  0> velocity=<-1,  0>";
                "position=< 3, -2> velocity=<-1,  1>";
                "position=< 6, 10> velocity=<-2, -1>";
                "position=< 2, -4> velocity=< 2,  2>";
                "position=<-6, 10> velocity=< 2, -2>";
                "position=< 1,  8> velocity=< 1, -1>";
                "position=< 1,  7> velocity=< 1,  0>";
                "position=<-3, 11> velocity=< 1, -2>";
                "position=< 7,  6> velocity=<-1, -1>";
                "position=<-2,  3> velocity=< 1,  0>";
                "position=<-4,  3> velocity=< 2,  0>";
                "position=<10, -3> velocity=<-1,  1>";
                "position=< 5, 11> velocity=< 1, -2>";
                "position=< 4,  7> velocity=< 0, -1>";
                "position=< 8, -2> velocity=< 0,  1>";
                "position=<15,  0> velocity=<-2,  0>";
                "position=< 1,  6> velocity=< 1,  0>";
                "position=< 8,  9> velocity=< 0, -1>";
                "position=< 3,  3> velocity=<-1,  1>";
                "position=< 0,  5> velocity=< 0, -1>";
                "position=<-2,  2> velocity=< 2,  0>";
                "position=< 5, -2> velocity=< 1,  2>";
                "position=< 1,  4> velocity=< 2,  1>";
                "position=<-2,  7> velocity=< 2, -2>";
                "position=< 3,  6> velocity=<-1, -1>";
                "position=< 5,  0> velocity=< 1,  0>";
                "position=<-6,  0> velocity=< 2,  0>";
                "position=< 5,  9> velocity=< 1, -2>";
                "position=<14,  7> velocity=<-2,  0>";
                "position=<-3,  6> velocity=< 2, -1>"]
                |> List.map parse;

    let d = if testMode then test else input;
    let b = bounds d;
    let (i, ps) = move b 0 d;
    
    printfn "Day 10, part 1:";

    plot ps;

    i
    |> printfn "Day 10, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;