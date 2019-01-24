module Day8

open System;
open System.Diagnostics;
open System.IO;

type Node = {
        Id : int;
        Children : Node list;
        MetaData : int list;
    };;

let rec build (c : int) (is : int list) : Node list * int list =
    match c with
    | 0 -> ([], is);
    | _ ->  match is with
            | [] -> failwith "Ran out of input"; //([], []);
            | x::xs ->  let m, ys = xs |> List.head, xs |> List.tail;
                        let cs, zs= if x <> 0 then
                                        build x ys;
                                    else
                                        [], ys;
                        let ms, ns = zs
                                    |> List.splitAt m;
                        let (ps, qs) = build (c-1) ns;
                        ({ Id = 0; Children = cs; MetaData = ms }::ps, qs);

let parse (s : string) : Node =
    let ds = s.Split(' ')
            |> Seq.toList
            |> List.map Int32.Parse;

    build 1 ds
    |> fst
    |> List.head;

let rec meta (n : Node) : int =
    let c = match n.Children with 
            | [] -> 0; 
            | ys -> ys
                    |> List.map meta
                    |> List.sum;
    c + (n.MetaData |> List.sum);

let rec meta2 (n : Node) : int =
    match n.Children with
    | [] -> n.MetaData |> List.sum;
    | xs -> n.MetaData
            |> List.map (fun y ->   match y with
                                    | 0 -> 0;
                                    | a when a > xs.Length -> 0
                                    | a -> meta2 xs.[a-1])
            |> List.sum;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = Seq.toList(File.ReadLines(file)).[0]
                |> parse

    let test = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
                |> parse

    let tree = if testMode then test else input;

    tree
    |> meta
    |> printfn "Day 8, part 1: %d";

    tree
    |> meta2
    |> printfn "Day 8, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;