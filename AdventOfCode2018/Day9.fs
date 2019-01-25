module Day9

open System;
open System.Diagnostics;
open System.IO;

let parse (s : string) : int * int = 
    match s.Split(' ') with
    | [| n; "players;"; "last"; "marble"; "is"; "worth"; p; "points" |] -> Int32.Parse n, Int32.Parse p;
    | _ -> failwith "Invalid input for parsing";

let insert (ms : int list) (p : int) (m : int) : int*int list =
    match ms with
    | [] -> (m, [m]);
    | [x] -> (m, x::[m]);
    | xs -> match ms |> List.findIndex (fun q -> p = q) with
            | i when i + 2 = xs.Length -> (m, List.append xs [m]);
            | i ->  (m, xs
                        |> List.splitAt ((i+2) % xs.Length)
                        |> (fun (a, b) -> List.append a (m::b)));

let remove (ms : int list) (p : int) : int*int*int list =
    let i = ms
            |> List.findIndex (fun q -> q = p)
            |> (fun x -> x-7)
            |> (fun x -> if x < 0 then x + ms.Length else x);
    
    let a, b =  ms
                |> List.splitAt i;

    let c, ds = b |> List.head, b |> List.tail;

    (c, ds |> List.head, List.append a ds);

let rec play (ms : int list) (ns : (int*int) list) (n : int) (p : int) (v : int) (l : int) : (int*int) list =
    let n' = (n+1) % ns.Length;

    match v with
    | x when x > l -> ns;
    | x when x > 0 && x <= l && x%23 = 0 -> let (s, p', ms') = remove ms p;
                                            let ns' =   ns
                                                        |> List.map (fun (q, t) -> if q = n then (q, t + s + v) else (q, t));
                                            //printfn "%A" ms';
                                            play ms' ns' n' p' (v+1) l;
    | x when x <= l ->  let (p', ms') = insert ms p v;
                        //printfn "%A" ms';
                        play ms' ns n' p' (v+1) l;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = Seq.toList(File.ReadLines(file)).[0]
                |> parse;

    let test = "9 players; last marble is worth 25 points"
                |> parse;

    let n, p = if testMode then test else input;
    let ns = [0..n-1] |> List.map (fun p -> (p, 0));

    play [] ns 0 0 0 p
    |> List.map (fun (_, x) -> x)
    |> List.max
    |> printfn "Day 9, part 1: %d";

    play [] ns 0 0 0 (p*100)
    |> List.map (fun (_, x) -> x)
    |> List.max
    |> printfn "Day 9, part 2: %A";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
