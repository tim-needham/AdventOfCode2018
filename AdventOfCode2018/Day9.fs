module Day9

open System;
open System.Diagnostics;
open System.IO;

type CircularList<'a> = {
        Value : 'a; 
        mutable Previous : CircularList<'a> option;
        mutable Next : CircularList<'a> option
    };;

let parse (s : string) : int * int = 
    match s.Split(' ') with
    | [| n; "players;"; "last"; "marble"; "is"; "worth"; p; "points" |] -> Int32.Parse n, Int32.Parse p;
    | _ -> failwith "Invalid input for parsing";

let singleton (v : 'a) : CircularList<'a> =
    let c = { Value = v; Previous = None; Next = None };
    c.Previous <- Some c;
    c.Next <- Some c;
    c;

let rec rotate (a : int) (c : CircularList<'a>) : CircularList<'a> =
    match a, c.Previous, c.Next with
    | 0, _, _ -> c;
    | v, _, Some n when v > 0 -> rotate (v-1) n;
    | v, Some p, _ when v < 0 -> rotate (v+1) p;
    | _ -> failwith "Invalid circular list.";

let insert (v : 'a) (c : CircularList<'a>) : CircularList<'a> =
    let d = {
                Value = v;
                Previous = c.Previous;
                Next = Some c
            };
    c.Previous.Value.Next <- Some d
    c.Previous <- Some d;
    d;

let pop (c : CircularList<'a>) : 'a * CircularList<'a> option =
    match c.Previous, c.Next with
    | Some p, Some n when p = n -> c.Value, None;
    | Some p, Some n -> p.Next <- Some n;
                        n.Previous <- Some p;
                        c.Value, Some n;
    | _, _ -> failwith "Invalid circular list";

let score (n : int) (s : int64) (ns : Map<int, int64>) : Map<int, int64> =
    match Map.containsKey n ns with
    | false ->  Map.add n s ns;
    | true ->   let e = Map.find n ns;
                Map.add n (e+s) ns;

let rec play (c : CircularList<int>) (ns : Map<int, int64>) (n : int) (m : int) (v : int) (l : int) : Map<int, int64> =
    let n' = (n+1) % m;

    match v with
    | x when x > l -> ns
    | x when 0 <> x && x%23 = 0 ->  match c |> rotate -7 |> pop with
                                    | _, None -> failwith "You popped a singleton. Now what?";
                                    | p, Some d ->  let ns' = score n (int64(p) + int64(v)) ns;
                                                    play d ns' n' m (v+1) l;
    | x when x <= l ->  let d = c
                                |> rotate 2
                                |> insert v;
                        play d ns n' m (v+1) l;
    
let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = Seq.toList(File.ReadLines(file)).[0]
                |> parse;

    let test = "9 players; last marble is worth 25 points"
                |> parse;

    let n, p = if testMode then test else input;
    let ns = new Map<int,int64>([]);
    
    play (singleton 0) ns 0 n 1 p
    |> Map.toList
    |> List.map (fun (_, x) -> x)
    |> List.max
    |> printfn "Day 9, part 1: %d";
    
    play (singleton 0) ns 0 n 1 (p*100)
    |> Map.toList
    |> List.map (fun (_, x) -> x)
    |> List.max
    |> printfn "Day 9, part 2: %A";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
