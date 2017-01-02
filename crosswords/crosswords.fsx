open System

type Placeholder = 
     H of row: int * col: int 
   | V of row: int * col: int 

// empty space to be filled by words
let E = '-'
// crossword background
let B = '+'
// crossword size
let N = 10

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
                                  |> List.map (fun pos -> H(i, pos))))
    |> List.collect (fun a -> a)

let getColumn grid i = 
    grid 
    |> List.map (fun (line: string) -> line.[i]) 
    |> Array.ofList 
    |> String

let vWordPositions = 
    [0 .. 9] 
    |> List.map (fun i -> getColumn grid i) 
    |> List.mapi (fun i col -> (getWordPositionsInLine col
                                  |> List.map (fun pos -> V(pos, i))))
    |> List.collect (fun a -> a)

let wordPositions = hWordPositions @ vWordPositions

// todo: this should be an array from the start and previous functions updated accordingly
let grid2d = Array2D.init N N (fun i j -> grid.[i].[j])

let wordFits (word: string) placeholder =     
    Array.zip (Array.ofSeq word) placeholder
                     |> Array.forall (fun (w, p) -> p = E || w = p)

let canInsertHword (grid2d: char [,]) (pos: int*int) (word:string) =
    // todo returs true if word is smaller than placeholder
    let x, y = fst(pos), snd(pos)
    let yEnd = y + word.Length - 1
    // word not too long
    yEnd < N &&
    // and not too short - either spans entire column or space after the word is a background tile
    (yEnd = N - 1 || grid2d.[x, yEnd + 1] = B) &&
    // and matches contents
    grid2d.[x..x, y..yEnd] 
            |> fun arr -> Array.init word.Length (fun i -> arr.[0, i])
            |> wordFits word

let canInsertVword (grid2d: char [,]) (pos: int*int) (word:string) =    
    let x, y = fst(pos), snd(pos)
    let xEnd = x + word.Length - 1
    // word not too long
    xEnd < N &&
    // and not too short - either spans entire column or space after the word is a background tile
    (xEnd = N - 1 || grid2d.[xEnd + 1, y] = B) &&
    // and matches contents
    grid2d.[x..xEnd, y..y]
            |> fun arr -> Array.init word.Length (fun i -> arr.[i, 0])
            |> wordFits word
            
let canInsertWord (grid2d: char [,]) (placeholder: Placeholder) (word:string) = 
    match placeholder with
    | H (r,c) -> canInsertHword grid2d (r,c) word
    | V (r,c) -> canInsertVword grid2d (r,c) word     

let insertHword (grid2d: char [,]) (pos: int*int) (word:string) =
    let len = word.Length
    let x, y = fst(pos), snd(pos)
    Array2D.init N N (fun i j -> if i = x && j >= y && j < y + len then word.[j - y] else grid2d.[i,j])

let insertVword (grid2d: char [,]) (pos: int*int) (word:string) =
    let len = word.Length
    let x, y = fst(pos), snd(pos)
    Array2D.init N N (fun i j -> if i >= x && i < x + len && j = y then word.[i - x] else grid2d.[i,j])

let insertWord (grid2d: char [,]) (placeholder: Placeholder) (word:string) = 
    match placeholder with
    | H (r,c) -> insertHword grid2d (r,c) word
    | V (r,c) -> insertVword grid2d (r,c) word 
          
let rec solveGrid positions words grid =
    if List.isEmpty words then 
        Some grid 
    else
    try 
        words |> List.pick (fun word -> 
                                    if canInsertWord grid (List.head positions) word
                                    then
                                        //printfn "Inserting %A" word
                                        let remainingWords = List.except [word] words
                                        let remainingPositions = List.tail positions
                                        let updatedGrid = insertWord grid (List.head positions) word
                                        //printfn "Updated grid to \n%A" updatedGrid
                                        solveGrid remainingPositions remainingWords updatedGrid
                                    else
                                        //printfn "Not inserting %A" word
                                        None)
        |> Some
    with 
    | :? System.Collections.Generic.KeyNotFoundException as ex -> None

let printGrid (grid2d: char [,]) = 
    //let stringGrid = Array2D.map new System.String (listOfChars |> List.toArray)
    [0..N-1] 
        |> List.map (fun i -> (grid2d.[i,0..N-1] |> List.ofArray))                   
        |> List.map (fun chars -> System.String.Concat(Array.ofList(chars)))    
        |> List.iter (fun str -> (printfn "%s" str))
   

let solution = solveGrid wordPositions words grid2d
match solution with
    | Some grid -> printGrid grid
    | None -> printfn "No solution found!"
