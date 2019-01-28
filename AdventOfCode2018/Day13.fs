module Day13

open System.Diagnostics;
open System.IO;

type Direction =
    | North
    | East
    | South
    | West
;;

type Turn =
    | Left
    | Straight
    | Right
;;

[<StructuredFormatDisplay("{AsString}")>]
type Point =
    {
        X : int;
        Y : int;
    }
    override x.ToString() = sprintf "(%d, %d)" (x.X) (x.Y);
    member x.AsString = x.ToString();
;;

type Cart =
    {
        Position : Point;
        Direction : Direction;
        NextTurn : Turn;
    }

type Track = 
    {
        Width : int;
        Height : int;
        Track : Map<Point, char>
    }
;;

let direction (c : char) : Direction =
    match c with
    | '^' -> North;
    | '>' -> East;
    | 'v' -> South;
    | '<' -> West;
    | _ -> failwith "Invalid direction input";

let track (c : char) : char =
    match c with
    | '^' -> '|';
    | '>' -> '-';
    | 'v' -> '|';
    | '<' -> '-';
    | _ -> c;

let parse(cs : char list list) : Track * Cart list =
    let rs =    [ for j in [0..cs.Length-1] do
                    for i in [0..cs.[0].Length-1] ->
                        (i, j, cs.[j].[i]);
                ]
                |> List.filter (fun (_, _, z) -> z <> ' ');
    let ts = rs
                |> List.filter (fun (_, _, z) -> z = '^' || z = 'v' || z = '<' || z = '>')
                |> List.map (fun (x, y, z) -> { Position = { X = x; Y = y }; Direction = direction z; NextTurn = Left });
    let rs' = rs
                |> List.map (fun (x, y, z) -> { X = x; Y = y }, track z)
                |> Map.ofList;

    { Width = cs.[0].Length; Height = cs.Length; Track = rs' }, ts;

let moveCart (t : Track) (c : Cart) : Cart =
    let x, y, d, t =    match Map.find c.Position t.Track with
                        | '|' ->    match c.Direction with 
                                    | North -> 0, -1, c.Direction, c.NextTurn;
                                    | South -> 0, 1, c.Direction, c.NextTurn;
                                    | _ -> failwith "Your cart derailed!";
                        | '-' ->    match c.Direction with 
                                    | East -> 1, 0, c.Direction, c.NextTurn;
                                    | West -> -1, 0, c.Direction, c.NextTurn;
                                    | _ -> failwith "Your cart derailed!";
                        | '/' ->    match c.Direction with 
                                    | North -> 1, 0, East, c.NextTurn;
                                    | East -> 0, -1, North, c.NextTurn;
                                    | South -> -1, 0, West, c.NextTurn;
                                    | West -> 0, 1, South, c.NextTurn;
                        | '\\' ->   match c.Direction with 
                                    | North -> -1, 0, West, c.NextTurn;
                                    | East -> 0, 1, South, c.NextTurn;
                                    | South -> 1, 0, East, c.NextTurn;
                                    | West -> 0, -1, North, c.NextTurn;
                        | '+' ->    let p, q, r =   match c.Direction with
                                                    | North ->  match c.NextTurn with
                                                                | Left -> -1, 0, West;
                                                                | Straight -> 0, -1, North
                                                                | Right -> 1, 0, East;
                                                    | East ->   match c.NextTurn with
                                                                | Left -> 0, -1, North;
                                                                | Straight -> 1, 0, East;
                                                                | Right -> 0, 1, South;

                                                    | South ->  match c.NextTurn with
                                                                | Left -> 1, 0, East;
                                                                | Straight -> 0, 1, South;
                                                                | Right -> -1, 0, West;

                                                    | West ->   match c.NextTurn with
                                                                | Left -> 0, 1, South;
                                                                | Straight -> -1, 0, West;
                                                                | Right -> 0, -1, North;
                                    let s = match c.NextTurn with
                                            | Left -> Straight;
                                            | Straight -> Right;
                                            | Right -> Left;
                                    p, q, r, s;
                        | _ -> failwith "Your cart fell off the track!";
    
    { Position = { X = c.Position.X+x; Y = c.Position.Y+y }; Direction = d; NextTurn = t };

let rec moveCarts (t : Track) (ds : Cart list) (cs : Cart list) : Cart list * Point list =
    match cs with
    | [] -> ds, [];
    | x::xs ->  let x' = moveCart t x;
                
                let ps, qs = ds |> List.partition (fun c -> c.Position.X = x'.Position.X && c.Position.Y = x'.Position.Y);
                let rs, ss = xs |> List.partition (fun c -> c.Position.X = x'.Position.X && c.Position.Y = x'.Position.Y);

                match ps @ rs with
                | [] -> moveCarts t (x'::qs) ss;
                | _ ->  let ys, zs = moveCarts t qs ss;
                        ys, (x'.Position::zs);

let rec findCrash (t : Track) (cs : Cart list) : Point =
    let cs', ps = cs
                    |> List.sortBy (fun c -> c.Position.Y, c.Position.X)
                    |> moveCarts t [];

    if ps.Length > 0 then
        List.head ps;
    else
        findCrash t cs';

let rec removeCrashes (t : Track) (cs : Cart list) : Point =
    match cs with
    | [] -> failwith "You ran out of carts";
    | [c] -> c.Position;
    | _ ->  let cs' = cs
                        |> List.sortBy (fun c -> c.Position.Y, c.Position.X);
            let ds, _ = moveCarts t [] cs';
            removeCrashes t ds;


let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = Seq.toList(File.ReadLines(file))
                |> List.map (Seq.toList)
                |> parse;

    let test = ["/->-\\        ";
                "|   |  /----\\";
                "| /-+--+-\\  |";
                "| | |  | v  |";
                "\\-+-/  \\-+--/";
                "  \\------/   " ]
                |> List.map Seq.toList
                |> parse;

    let test2 = ["/>-<\\  ";
                "|   |  ";
                "| /<+-\\";
                "| | | v";
                "\\>+</ |";
                "  |   ^";
                "  \\<->/" ]
                |> List.map Seq.toList
                |> parse;

    let t, cs = if testMode then test else input

    cs
    |> findCrash t
    |> printfn "Day 13, part 1: %A";

    let t2, cs2 = if testMode then test2 else input

    cs2
    |> removeCrashes t2
    |> printfn "Day 13, part 2: %A";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;