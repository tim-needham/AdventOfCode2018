module Day2

open System.Diagnostics;
open System.IO;

let nples (n : int) (s : string) : int =
    s
    |> Seq.toList
    |> List.groupBy (fun x -> x)
    |> List.where (fun g -> List.length (snd g) = n)
    |> List.length;
    

let ncount (n : int) (ss : string list) : int =
    ss
    |> List.fold (fun a x -> a + if nples n x > 0 then 1 else 0) 0;

let checksum (ss : string list) : int =
    ncount 2 ss * ncount 3 ss;

let rec compare (ps : char list) (qs : char list) : (char list * int) =
    match ps, qs with
    | [], _ -> ([], 0)
    | _, [] -> ([], 0)
    | x::xs, y::ys ->   let (ms, c) = compare xs ys;
                        if x = y then  
                            (x::ms, c);
                        else
                            (ms, c+1);

let prototype (ss : (string * string) list) : string =
    ss
    |> List.map (fun (x, y) -> compare (Seq.toList x) (Seq.toList y))
    |> List.find(fun (x, y) -> y = 1)
    |> fst
    |> List.fold (fun a x -> a + x.ToString()) "";

let rec pairs lst =
    match lst with
    | [] -> [];
    | h::t -> List.map (fun elem -> (h, elem)) t @ pairs t;


let run (file : string, testMode : bool) =

    let w = new Stopwatch();
    w.Start();

    let input = Seq.toList(File.ReadLines(file));

    let test1 = [ "abcdef";
                "bababc";
                "abbcde";
                "abcccd";
                "aabcdd";
                "abcdee";
                "ababab" ];

    let test2 = [ "abcde";
                "fghij";
                "klmno"; 
                "pqrst"; 
                "fguij"; 
                "axcye"; 
                "wvxyz" ];

    if testMode then test1 else input
    |> checksum
    |> printfn "Day 2, part 1: %d";

    if testMode then test2 else input
    |> pairs
    |> prototype
    |> printfn "Day 2, part 2: %A";

    w.Stop();
    printfn "Time taken: %d ms" w.ElapsedMilliseconds;
