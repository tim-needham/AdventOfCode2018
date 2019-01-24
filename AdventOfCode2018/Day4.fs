module Day4

open System;
open System.Diagnostics;
open System.IO;

type State = 
    | Awake
    | Asleep;;

type Action = 
    | Guard of g : int
    | Wakes
    | Sleeps;;

type Event = {
    Timestamp : DateTime;
    Action : Action
};;

let parse (s : string) : Event =
    match s.Replace("[", "").Replace("]", "").Replace("#", "").Split(' ') with
    | [| d; t; "Guard"; n; "begins"; "shift" |] -> { Timestamp = DateTime.Parse(d + " " + t); Action = Guard (Int32.Parse(n)) };
    | [| d; t; "wakes"; "up" |] -> { Timestamp = DateTime.Parse(d + " " + t); Action = Wakes };
    | [| d; t; "falls"; "asleep" |] -> { Timestamp = DateTime.Parse(d + " " + t); Action = Sleeps };
    | _ -> failwith "Invalid input for parsing";

let addSleep (m : Map<int*int, int>)  (g : int) (t : int) : Map<int*int, int> =
    match Map.containsKey (g, t) m with
    | true ->   let v = Map.find (g, t) m;
                Map.add (g, t) (v + 1) m;
    | false -> Map.add (g, t) 1 m

let rec consume (m : Map<int*int, int>) (g : int) (t : int) (a : State) (es : Event list) : Map<int*int, int> =
    match es with
    | [] -> m;
    | x::xs ->  match x.Action with
                | Guard n ->    match a with
                                | Awake -> consume m n 0 Awake xs;
                                | Asleep -> let m' = [t..59]
                                                    |> List.fold (fun y z -> addSleep y g z) m;
                                            consume m' n 0 Awake xs;
                | Wakes ->  let t' = x.Timestamp.Minute;
                            let m' = [t..(t' - 1)]
                                    |> List.fold (fun y z -> addSleep y g z) m;
                            consume m' g t' Awake xs;
                | Sleeps -> let t' = x.Timestamp.Minute;
                            consume m g t' Asleep xs;

let sleepiest1 (es : Event list) : int =
    let m = consume (new Map<int*int, int>([])) -1 0 Awake es;

    let g = m
            |> Map.toList
            |> List.groupBy (fun ((g, _), _) -> g)
            |> List.sortBy (fun g -> 0 - (g |> snd |> List.map (snd) |> List.sum))
            |> List.head
            |> fst;

    let n = m
            |> Map.filter (fun (x, _) _ -> x = g)
            |> Map.toList
            |> List.sortBy (fun (_, v) -> -v)
            |> List.head
            |> fst
            |> snd;

    g * n;

let sleepiest2 (es : Event list) : int =
    let m = consume (new Map<int*int, int>([])) -1 0 Awake es;

    let x = m
            |> Map.toList
            |> List.sortBy (fun ((_, _), v) -> -v)
            |> List.head;

    let g = (fst (fst x));
    let n = (snd (fst x));

    g * n;

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = Seq.toList(File.ReadLines(file))
                |> List.map parse
                |> List.sortBy (fun x -> x.Timestamp);

    let test = [ "[1518-11-01 00:00] Guard #10 begins shift"; 
                "[1518-11-01 00:05] falls asleep";
                "[1518-11-01 00:25] wakes up";
                "[1518-11-01 00:30] falls asleep";
                "[1518-11-01 00:55] wakes up";
                "[1518-11-01 23:58] Guard #99 begins shift";
                "[1518-11-02 00:40] falls asleep";
                "[1518-11-02 00:50] wakes up";
                "[1518-11-03 00:05] Guard #10 begins shift";
                "[1518-11-03 00:24] falls asleep";
                "[1518-11-03 00:29] wakes up";
                "[1518-11-04 00:02] Guard #99 begins shift";
                "[1518-11-04 00:36] falls asleep";
                "[1518-11-04 00:46] wakes up";
                "[1518-11-05 00:03] Guard #99 begins shift";
                "[1518-11-05 00:45] falls asleep";
                "[1518-11-05 00:55] wakes up" ]
                |> List.map parse
                |> List.sortBy (fun x -> x.Timestamp);

    if testMode then test else input
    |> sleepiest1
    |> printfn "Day 4, part 1: %d";

    if testMode then test else input
    |> sleepiest2
    |> printfn "Day 4, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
