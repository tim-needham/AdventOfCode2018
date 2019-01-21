module Day3

open System;
open System.Diagnostics;
open System.IO;

type Claim = {
    Id : int;
    X : int;
    Y : int;
    W : int;
    H : int;
};;

let parse (s : string) : Claim =
    match s.Replace("#", "").Replace(":", "").Split(' ') with
    | [| a; "@"; c; d |] -> let i = Int32.Parse a;
                            let xy = c.Split(',') |> Seq.toList |> List.map (fun x -> Int32.Parse x);
                            let wh = d.Split('x') |> Seq.toList |> List.map (fun x -> Int32.Parse x);
                            { Id = i; X = xy.[0]; Y = xy.[1]; W = wh.[0]; H = wh.[1] };
    | _ -> failwith "Invalid input for parsing";

let rec apply (m : Map<int*int, int>) (cs : Claim list) : Map<int*int, int> =
    match cs with
    | [] -> m;
    | c::ds ->  let xys = [for i in [0..c.W-1] do for j in [0..c.H-1] -> (c.X+i, c.Y+j)];
                let m' = xys
                        |> List.fold (fun a (x, y) ->   let v = if Map.containsKey (x, y) a then
                                                                    Map.find (x, y) a;
                                                                else
                                                                    0;
                                                        Map.add (x, y) (v+1) a) m;
                apply m' ds;

let overlap (m : Map<int*int, int>) (cs : Claim list) : int =
    apply m cs
    |> Map.toList
    |> List.where (fun ((x, y), v) -> v > 1)
    |> List.length;

let intersect (a : Claim) (b : Claim) : bool =
    if a.X > (b.X + b.W - 1) || b.X > (a.X + a.W - 1) then
        false;
    else if a.Y > (b.Y + b.H - 1) || b.Y > (a.Y + a.H - 1) then
        false;
    else
        true;

let rec intact (a : Claim) (cs : Claim list) : bool =
    match cs with
    | [] -> true
    | x::xs ->  if intersect a x then
                    false;
                else
                    intact a xs;

let rec findIntact (cs : (Claim * Claim list) list) : Claim =
    match cs with
    | (x, ys)::xs -> if intact x ys then
                        x;
                     else
                        findIntact xs;
    | [] -> failwith "There are no intact claims.";

let without (c : Claim) (cs : Claim list) : Claim list =
    cs
    |> List.filter (fun x -> x.Id <> c.Id);

let permute (cs : Claim list) : (Claim * Claim list) list =
    [for c in cs -> (c, without c cs)];

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (fun x -> parse x);

    let test = [ "#1 @ 1,3: 4x4"; "#2 @ 3,1: 4x4"; "#3 @ 5,5: 2x2" ]
                |> List.map (fun x -> parse x);

    if testMode then test else input
    |> overlap (new Map<int*int, int>([])) 
    |> printfn "Day 3, part 1: %d";

    if testMode then test else input
    |> permute
    |> findIntact
    |> printfn "Day 3, part 2: %A";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;