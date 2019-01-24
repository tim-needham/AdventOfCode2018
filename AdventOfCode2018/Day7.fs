module Day7

open System;
open System.Diagnostics;
open System.IO;

type Rule = {
        Pre : string;
        Post : string;
    };;

type Step = {
        Label : string;
        Priors : string list;
    };;

type Worker = {
        Id : int;
        Step : Step Option;
        Wait : int;
    };;

type State = {
        Clock : int;
        BaseCost : int;
        Workers : Worker list;
        Completed : Step list;
    };;

let parse (s : string) : Rule =
    match s.Split(' ') with
    | [| "Step"; a; "must"; "be"; "finished"; "before"; "step"; b; "can"; "begin." |] -> { Pre = a; Post = b };
    | _ -> failwith "Invalid input for parsing";

let steps (rs : Rule list) : Step list =
    let ss = rs
            |> List.groupBy (fun r -> r.Post)
            |> List.map (fun (g, vs) -> { Label = g; Priors = vs |> List.map (fun r -> r.Pre ) })
            |> List.sortBy (fun s -> s.Label);

    // Search for the step(s) with no dependencies add them to the head of the returned list
    ss
    |> List.collect (fun s -> s.Priors)
    |> List.distinct
    |> List.filter (fun l -> not (ss |> List.map (fun s -> s.Label) |> List.contains l))
    |> List.map (fun x -> { Label = x; Priors = [] })
    |> List.append ss;

let candidate (ss : Step list) (s : Step) : bool =
    let ls = ss |> List.map (fun l -> l.Label);

    (s.Priors
    |> List.forall (fun p -> List.contains p ls))
    || s.Priors.Length = 0;

let rec traverse (ss : Step list) (rs : Step list) : Step list =
    match rs with
    | [] -> ss |> List.rev;
    | _ ->  let c = rs
                    |> List.filter (fun x -> candidate ss x)
                    |> List.sortBy (fun x -> x.Label)
                    |> List.head;

            traverse (c::ss) (rs |> List.filter (fun x -> x.Label <> c.Label));

let tick (s : State) : State = 
    let (ws, cs) = s.Workers
                    |> List.fold (fun (bs, cs) x -> match x.Wait with
                                                    | 0 -> (x::bs, cs);
                                                    | 1 -> ({ Id = x.Id; Step = None; Wait = 0 }::bs, x.Step.Value::cs);
                                                    | n -> ({ Id = x.Id; Step = x.Step; Wait = x.Wait - 1 }::bs, cs)) ([], [])
                    |> (fun (x, y) -> x |> List.rev, y |> List.rev);

    {
        Clock = s.Clock + 1; 
        BaseCost = s.BaseCost;
        Workers = ws;
        Completed = cs |> List.append s.Completed
    };

let cost (b : int) (s : string) : int =
    s
    |> Seq.toList
    |> List.head
    |> int
    |> (fun x -> x - 64 + b);

let assign (s : State) (cs : Step list) : State =
    let ws = s.Workers
                |> List.filter (fun x -> x.Wait = 0);
    let c = Math.Min(ws |> List.length, cs |> List.length);
    let ws' = cs 
                |> List.take c
                |> List.zip (ws |> List.take c)
                |> List.map (fun (x, y) -> { Id = x.Id; Step = Some y; Wait = cost s.BaseCost y.Label });
    let ws'' = s.Workers
                |> List.map (fun x ->   match ws' |> List.tryFind (fun y -> y.Id = x.Id) with
                                        | Some z -> z;
                                        | None ->   x;);

    {
        Clock = s.Clock; 
        BaseCost = s.BaseCost;
        Workers = ws'';
        Completed = s.Completed
    };

let printWorker (w : Worker) : string =
    match w.Step with
    | Some x -> x.Label + "\t";
    | None -> ".\t";

let rec pTraverse (s : State) (rs : Step list) : State =
    //let ss = s.Workers |> List.fold (fun a x -> a + (printWorker x)) "";
    //printfn "%d\t%s%s" s.Clock ss (s.Completed |> List.fold (fun a x -> a + x.Label) "")
    let s' = tick s;
    match rs with
    | [] -> // Keep running out the active steps until they're all done.
            match s'.Workers |> List.filter (fun x -> x.Wait > 0) with
            | [] -> s';
            | _ ->  pTraverse s' [];
    | _ ->  // Find the first available worker or keep working if there are none.
            match s'.Workers |> List.filter (fun x -> x.Wait = 0) with
            | [] -> pTraverse s' rs
            | _ -> match    rs
                            |> List.filter (fun x -> candidate s'.Completed x)
                            |> List.sortBy (fun x -> x.Label) with
                    | [] -> pTraverse s' rs;
                    | cs -> let s'' = assign s' cs; 
                            let ps = s''.Workers |> List.filter (fun x -> x.Step <> None) |> List.map (fun x -> x.Step.Value.Label);
                            let rs' = rs |> List.filter (fun x -> not(ps |> List.contains x.Label))
                            pTraverse s'' rs';

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = Seq.toList(File.ReadLines(file))
                |> List.map parse;

    let test = [ "Step C must be finished before step A can begin.";
                "Step C must be finished before step F can begin.";
                "Step A must be finished before step B can begin.";
                "Step A must be finished before step D can begin.";
                "Step B must be finished before step E can begin.";
                "Step D must be finished before step E can begin.";
                "Step F must be finished before step E can begin." ]
                |> List.map parse;

    if testMode then test else input
    |> steps
    |> traverse []
    |> List.fold (fun a x -> a + x.Label) ""
    |> printfn "Day 7, part 1: %A";

    let b = if testMode then 0 else 60;
    let ws = if testMode then [1; 2] else [1..5]
            |> List.map (fun x -> { Id = x; Step = None; Wait = 0 });

    if testMode then test else input
    |> steps
    |> pTraverse { Clock = -1; BaseCost = b; Workers = ws; Completed = [] }
    |> (fun x -> x.Clock)
    |> printfn "Day 7, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;