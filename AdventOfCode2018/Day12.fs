module Day12

open System.Diagnostics;
open System.IO;

[<StructuredFormatDisplay("{AsString}")>]
type Rule = 
    {
        Pattern : string;
        Result: string;
    }
    override x.ToString() = sprintf "%s => %s" (x.Pattern) (x.Result);
    member x.AsString = x.ToString();
;;

[<StructuredFormatDisplay("{AsString}")>]
type Soil =
    | Plant
    | Empty
    override x.ToString() = sprintf "%s" (if x = Plant then "#" else ".");
    member x.AsString = x.ToString();
;;

[<StructuredFormatDisplay("{AsString}")>]
type Pot = 
    { 
        Id : int; 
        Contents : Soil; 
    }    
    override x.ToString() = sprintf "%s" (x.Contents.ToString());
    member x.AsString = x.ToString();
;;

let initial (s : string) : Pot list = 
    match s.Replace(": ", ":").Split(':') with
    | [| "initial state"; i |] ->   let is = i
                                            |> Seq.toList;

                                    [0..is.Length-1]
                                    |> List.zip is
                                    |> List.map (fun (s, n) -> { Id = n; Contents = match s with
                                                                                    | '#' -> Plant;
                                                                                    | _ -> Empty; });
    | _ -> failwith "Invalid initial state input";

let rule (s : string) : Rule =
    match s.Replace(" => ", "|").Split('|') with
    | [| p; r |] -> { Pattern = p; Result = r };
    | _ -> failwith "Invalid rule input";

let parse (ss : string list) : Pot list * Rule list =
    match ss with
    | [] -> failwith "Invalid input for parsing";
    | [_] -> failwith "Invalid input for parsing";
    | [_; _] -> failwith "Invalid input for parsing";
    | x::xs ->  let i = initial x;
                let rs = xs 
                            |> List.tail 
                            |> List.map rule
                            |> List.filter (fun r -> r.Result = "#");
                i, rs;

let prettyPrint (ps : Pot list) : string =
    ps
    |> List.fold (fun a x -> a + x.ToString()) "";

let rec by5s (es : 'a list) : 'a list list =
    match es with
    | x::xs when xs.Length >= 4 -> (x::(List.take 4 xs)) :: (by5s xs);
    | _ -> [];

let evolve (rs : Rule list) (ps : Pot list) : Pot list =
    let n, x =  (List.head ps, ps |> List.rev |> List.head)
                |> (fun (a, b) -> a.Id, b.Id);
    let ps' =   [ { Id = n-4; Contents = Empty; }; { Id = n-3; Contents = Empty; }; { Id = n-2; Contents = Empty; }; { Id = n-1; Contents = Empty; } ]
                @ ps @
                [ { Id = x+1; Contents = Empty; }; { Id = x+2; Contents = Empty; }; { Id = x+3; Contents = Empty; }; { Id = x+2; Contents = Empty; } ];
    ps'
    |> by5s
    |> List.map (fun xs -> (xs 
                            |> List.skip 2 
                            |> List.head 
                            |> (fun p -> p.Id), 
                            xs 
                            |> List.fold (fun a p -> a + p.Contents.ToString()) "")
                )
    |> List.map (fun (i, m) ->  match List.tryFind (fun r -> r.Pattern = m) rs with
                                | Some _ -> { Id = i; Contents = Plant };
                                | None -> { Id = i; Contents = Empty };)

let rec ltrim (ps : Pot list) : Pot list =
    match ps with
    | x::xs when x.Id < 0 && x.Contents = Empty -> ltrim xs;
    | _ -> ps;

let rec rtrim (ps : Pot list) : Pot list =
    match ps with
    | [] -> [];
    | [x] when x.Contents = Empty -> [];
    | [x] -> [x];
    | x::xs ->  match rtrim xs with
                | [] -> match x.Contents with
                        | Empty -> [];
                        | _ -> [x];
                | ps' -> x::ps';
                
let trim (ps : Pot list) : Pot list =
    ps
    |> ltrim
    |> rtrim;

let score (ps : Pot list) : int =
    ps
    |> List.filter (fun p -> p.Contents = Plant)
    |> List.sumBy (fun p -> p.Id);

let rec life (n : int) (rs : Rule list) (ps : Pot list) : Pot list =
    match n with
    | 0 -> ps;
    | _ ->  ps
            |> evolve rs
            |> trim
            |> life (n-1) rs;

// Assuming that at some point the inter-generational difference will stabilise...
let rec converge (n : int) (d : int) (rs : Rule list) (ps: Pot list) : int*int =
    let n' = n+1;
    let s = score ps;
    let ps' =   ps
                |> evolve rs
                |> trim;
    let s' = score ps';
    let d' = s'-s;
    if d' = d then
        (n', d);
    else
        converge n' d' rs ps';

let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = Seq.toList(File.ReadLines(file))
                |> parse;

    let test = [ "initial state: #..#.#..##......###...###"; 
                "";
                "...## => #";
                "..#.. => #";
                ".#... => #";
                ".#.#. => #";
                ".#.## => #";
                ".##.. => #";
                ".#### => #";
                "#.#.# => #";
                "#.### => #";
                "##.#. => #";
                "##.## => #";
                "###.. => #";
                "###.# => #";
                "####. => #" ]
                |> parse;

    let ps, rs = if testMode then test else input;

    ps
    |> life 20 rs
    |> score
    |> printfn "Day 12, part 1: %A";

    let n, d = converge 0 0 rs ps;
    //printfn "After %d steps, difference settled at %d" n d;

    ps
    |> life n rs
    |> score
    |> (fun x -> int64(x) + ((50000000000L - int64(n)) * int64(d)))
    |> printfn "Day 12, part 2: %d";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
