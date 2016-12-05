open System

// empty space to be filled by words
let E = '-'
// crossword backgroud
let B = '+'

let rec genPerm (acc: string list) (rest: string list) =
    match rest with 
    | [] -> printfn "%A" acc
    | _ -> rest |> List.iter(fun e -> genPerm (e::acc) (List.except [e] rest))

let getWordPositionsInLine (line: string) = 
    [0 .. line.Length - 2] |> List.filter (fun i -> i = 0 && line.[0] = E && line.[1] = E ||
                                                    i > 0 && line.[i-1] = B && line.[i] = E && line.[i+1] = E)
    
let (grid: string list, words: string list) = List.init 10 (fun _ -> Console.ReadLine()), Console.ReadLine().Split(';') |> List.ofArray

let hWordPositions =
    grid     
    |> List.mapi (fun i line -> (getWordPositionsInLine line 
                                  |> List.map (fun pos -> (i, pos))))
    |> List.collect (fun a -> a)

let vWordPositions =
    let getColumn grid i = 
        grid 
        |> List.map (fun (line: string) -> line.[i]) 
        |> Array.ofList |> String    
    [0 .. 9] 
    |> List.map (fun i -> getColumn grid i) 
    |> List.mapi (fun i col -> (getWordPositionsInLine col
                                  |> List.map (fun pos -> (pos, i))))
    |> List.collect (fun a -> a)
    
    
printfn "%A" (vWordPositions)