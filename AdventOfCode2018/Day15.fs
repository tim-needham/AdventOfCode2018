module Day15

open System;
open System.Diagnostics;
open System.IO;

type UnitType =
    | Elf
    | Goblin
;;

type Position = {
    X : int;
    Y : int;
};;

type Unit = {
    Type : UnitType;
    Position : Position;
    Health : int;
    Power : int;
}

type Game = {
    Grid : bool list list;
    Units : Unit list;
};;

let parseUnit ((x, y, t) : int * int * char) : Unit =
    match t with
    | 'E' -> { Type = Elf; Position = { X = x; Y = y }; Health = 200; Power = 3 };
    | 'G' -> { Type = Goblin; Position = { X = x; Y = y }; Health = 200; Power = 3 };
    | _ -> failwithf "Unrecognised input %c!" t;

let parse (ss : string list) : Game =
    let ts = ss |> List.mapi (fun y r -> r |> Seq.toList |> List.mapi (fun x s -> (x, y, s)))
    let us = ts |> List.concat |> List.filter (fun (_, _, t) -> t = 'E' || t = 'G');
    {
        Grid = ts |> List.map (fun r -> r |> List.map (fun (_, _, c) -> c <> '#'));
        Units = us |> List.map parseUnit;
    };

let prettyPrint (g : Game) (ps : Position list) (c : char) : unit =
    for j in 0..g.Grid.Length-1 do
        for i in 0..g.Grid.[j].Length-1 do
            if g.Units |> List.exists (fun e -> e.Position.X = i && e.Position.Y = j && e.Type = Elf) then
                printf "E";
            elif g.Units |> List.exists (fun e -> e.Position.X = i && e.Position.Y = j && e.Type = Goblin) then
                printf "G";
            elif ps |> List.exists (fun e -> e.X = i && e.Y = j) then
                printf "%c" c;
            else 
                printf "%c" (if g.Grid.[j].[i] then '.' else '#')
        printf " ";
        for u in g.Units |> List.filter (fun x -> x.Position.Y = j) |> List.sortBy (fun x -> x.Position.X) do
            printf "%c(%d), " (if u.Type = Elf then 'E' else 'G') u.Health;
        printfn "";
    printfn"";

let clear (a : Position) (g : bool list list) (us : Unit list) : bool =
    g.[a.Y].[a.X] && not (List.exists (fun u -> u.Position.X = a.X && u.Position.Y = a.Y) us);

let adjacent (a : Position) (b : Position) : bool =
    (a.X = b.X && Math.Abs(a.Y - b.Y) = 1) || (a.Y = b.Y && Math.Abs(a.X - b.X) = 1);

let adjacents (p : Position) : Position list =
    // generate in reading order
    [ (0, -1); (-1, 0); (1, 0); (0, 1) ] |> List.map (fun (x, y) -> { X = p.X + x; Y = p.Y + y });

let inRange (u : Unit) (g : bool list list) (us : Unit list) : Position list =
    u.Position |> adjacents |> List.filter (fun p -> clear p g us);

let inCombat (u : Unit) (es : Unit list) : Unit option =
    match es |> List.filter (fun e -> adjacent u.Position e.Position) with
    | [] -> None;
    | xs -> xs
            |> List.groupBy (fun e -> e.Health)
            |> List.sortBy (fun (g, _) -> g)
            |> List.map (fun (_, us) -> us)
            |> List.head
            |> List.sortBy (fun u -> u.Position.Y, u.Position.X)
            |> List.head
            |> Some;

let nearest (ps : (Position * Position list list) list) : (Position * Position list list) list =
    match ps with
    | [] -> [];
    | _ ->  ps
            |> List.groupBy (fun (_, p) -> p |> List.head |> List.length)
            |> List.sortBy (fun (g, _) -> g)
            |> List.head
            |> snd;

let mergeAndPrune (a : Position list list) (b : Position list list) : Position list list =
    (a@b)
    |> List.groupBy List.head
    |> List.map (fun (_, ps) -> List.head ps);

// ps - the paths already walked
// d - the target
// ss - the positions already visited
// g - the grid
// us - all the units
let rec walk (ps: Position list list) (d : Position) (ss : Position list) (g : bool list list) (us : Unit list) : Position list list =
    match ps with
    | [] -> [];
    | _ ->  match List.filter (fun p -> List.head p = d) ps with
            | [] -> let (rs, ts) =  ps
                                    |> List.fold (fun (ns, sn) p -> let h = List.head p;
                                                                    if clear h g us then
                                                                        let ns2 =   adjacents h
                                                                                    |> List.filter (fun x -> not (List.contains x ss))
                                                                                    |> List.map (fun x -> x::p);
                                                                        (mergeAndPrune ns ns2, h::sn)
                                                                    else
                                                                        (ns, h::sn)) ([], ss);
                    walk rs d ts g us;
            | xs -> xs |> List.map List.rev;

let getTarget (ps : (Position * Position list list) list) : (Position * Position list list) option =
    match ps with
    | [] -> None;
    | _ -> ps |> List.sortBy (fun (p, _) -> (p.Y, p.X)) |> List.head |> Some;

let move (v : bool) (c : Unit) (g : bool list list) (cs : Unit list) (es : Unit list) : Unit =
    let us = cs@es;
    let vs = c::us;
    let is = es |> List.map (fun e -> inRange e g (cs@es)) |> List.concat |> List.distinct;

    if v then
        prettyPrint { Grid = g; Units = vs } is '?';
    
    match is with
    | [] -> c; // no free squares so don't move
    | _ ->  let js = is
                        |> List.map (fun i -> (i, walk (adjacents c.Position |> List.map (fun x -> [x])) i [c.Position] g us))
                        |> List.filter (fun (_, p) -> p <> [])

            if v then
                prettyPrint { Grid = g; Units = vs } (js |> List.map fst) '@';

            let ks = nearest js;

            if v
                then prettyPrint { Grid = g; Units = vs } (ks |> List.map fst) '!';

            match getTarget ks with
            | None -> c;
            | Some t -> if v then
                            prettyPrint { Grid = g; Units = vs } [fst t] '+';
                        let c' = snd t
                                |> List.map List.head
                                |> List.distinct
                                |> List.sortBy (fun p -> (p.Y, p.X))
                                |> List.head;

                        { Type = c.Type; Position = c'; Health = c.Health; Power = c.Power };

let attack (p : int) (t : Unit) : Unit option =
    match t.Health - p with
    | n when n <= 0 -> None;
    | n -> Some { Type = t.Type; Position = t.Position; Health = n; Power = t.Power };

let resolveCombat (a : Unit) (e : Unit) (cs : Unit list) (rs : Unit list) : Unit list * Unit list =
    match attack a.Power e with
    | None ->   let ds, ys = List.except [e] cs, List.except [e] rs;
                (ds, ys);
    | Some f -> let ds = List.map (fun u -> if u.Position = f.Position then f else u) cs;
                let ys = List.map (fun u -> if u.Position = f.Position then f else u) rs;
                (ds, ys);

// output signature - continue * round complete * remaining units
let rec step (v : bool) (g : bool list list) (cs : Unit list) (rs : Unit list): bool * bool * Unit list =
    match rs with
    | [] -> match List.partition (fun u -> u.Type = Elf) cs with
            | [], _ -> false, true, cs;
            | _, [] -> false, true, cs;
            | _, _ -> true, true, cs;
    | x::xs ->  match List.partition (fun u -> u.Type = x.Type) (cs@xs) with
                | _, [] -> false, false, cs@rs;
                | fs, es -> match inCombat x es with
                            | Some e -> let (ds, ys) = resolveCombat x e cs xs;
                                        step v g (x::ds) ys;
                            | None ->   let c = move v x g fs es;
                                        match inCombat c es with
                                        | Some e -> let (ds, ys) = resolveCombat c e cs xs;
                                                    step v g (c::ds) ys;
                                        | None -> step v g (c::cs) xs;

let round (v : bool) (g : Game) : bool * bool * Game =
    g.Units
    |> List.sortBy (fun u -> u.Position.Y, u.Position.X)
    |> step v g.Grid []
    |> (fun (c, d, ds) -> c, d, { Grid = g.Grid; Units = ds });

let rec play (v : bool) (n : int) (g : Game) : int =
    match round false g with
    | true, _, h ->     if v then
                            printfn "Round %d:" (n+1);
                            prettyPrint h [] ' ';
                        play v (n+1) h;
    | false, c, h ->    if v then
                            printfn "Round %d %s:" (n+1) (if c then "complete" else "incomplete");
                            prettyPrint h [] ' ';
                        h.Units
                        |> List.sumBy (fun x -> x.Health)
                        |> (fun x -> x * (n + if c then 1 else 0));

let powerup (p : int) (g : Game) : Game =
    {
        Grid = g.Grid;
        Units = g.Units
                |> List.map (fun u ->   if u.Type = Goblin then 
                                            u;
                                        else
                                            { Type = u.Type; Position = u.Position; Health = u.Health; Power = p };)
    };

let rec play2 (v : bool) (n : int) (p : int) (g : Game) (h : Game) : int =
    match h |> powerup p |> round v with
    | true, _, i ->     if v then
                            printfn "Round %d: Power %d" (n+1) p;
                            prettyPrint i [] ' ';
                        if (i.Units |> List.filter (fun x -> x.Type = Elf) |> List.length) = (g.Units |> List.filter (fun x -> x.Type = Elf) |> List.length) then
                            play2 v (n+1) p g i;
                        else
                            play2 v 0 (p+1) g g;
    | false, c, i ->    if v then
                            printfn "Round %d %s: Power %d" (n+1) (if c then "complete" else "incomplete") p;
                            prettyPrint i [] ' ';
                        if (i.Units |> List.filter (fun x -> x.Type = Elf) |> List.length) = (g.Units |> List.filter (fun x -> x.Type = Elf) |> List.length) then
                            i.Units
                            |> List.sumBy (fun x -> x.Health)
                            |> (fun x -> x * (n + if c then 1 else 0));
                        else
                            play2 v 0 (p+1) g g;
    
let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let test = [ "#######";
                "#E..G.#";
                "#...#.#";
                "#.G.#G#";
                "#######" ]
                |> parse;

    let test2 = [ "#######";
                "#.E...#";
                "#.....#";
                "#...G.#";
                "#######" ]
                |> parse;

    let test3 = [ "#########";
                "#G..G..G#";
                "#.......#";
                "#.......#";
                "#G..E..G#";
                "#.......#";
                "#.......#";
                "#G..G..G#";
                "#########" ]
                |> parse;
    
    let test4 = [ "#######";
                "#.G...#";
                "#...EG#";
                "#.#.#G#";
                "#..G#E#";
                "#.....#";
                "#######" ]
                |> parse;

    let test5 = [ "#######";
                "#G..#E#";
                "#E#E.E#";
                "#G.##.#";
                "#...#E#";
                "#...E.#";
                "#######" ]
                |> parse;

    let test6 = [ "#######";
                "#E..EG#";
                "#.#G.E#";
                "#E.##E#";
                "#G..#.#";
                "#..E#.#";
                "#######" ]
                |> parse;

    let test7 = [ "#######";
                "#E.G#.#";
                "#.#G..#";
                "#G.#.G#";
                "#G..#.#";
                "#...E.#";
                "#######" ]
                |> parse;

    let test8 = [ "#######";
                "#.E...#";
                "#.#..G#";
                "#.###.#";
                "#E#G#G#";
                "#...#G#";
                "#######" ]
                |> parse;

    let test9 = [ "#########";
                "#G......#";
                "#.E.#...#";
                "#..##..G#";
                "#...##..#";
                "#...#...#";
                "#.G...G.#";
                "#.....G.#";
                "#########" ]
                |> parse;

    let input = Seq.toList(File.ReadLines(file))
                |> parse;

    if testMode then test9 else input
    |> play false 0
    |> printfn "Day 15, part 1: %d";

    if testMode then test9 else input
    |> (fun x -> x, x)
    ||> play2 false 0 4
    |> printfn "Day 15, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
