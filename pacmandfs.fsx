open System

// tile defs
let WALL = '%'
let EMPTY = '-'
let PACMAN = 'P'
let FOOD = '.'

// read input board, player and board size
let (pacman, food, (boardHeight, boardWidth), board) = 
    let pacmanLine = Console.ReadLine().Split(' ')
    let pacman = (Int32.Parse(pacmanLine.[0]), Int32.Parse(pacmanLine.[1]))
    let foodLine = Console.ReadLine().Split(' ')
    let food = (Int32.Parse(foodLine.[0]), Int32.Parse(foodLine.[1]))
    let sizeLine = Console.ReadLine().Split(' ')
    let boardSize = (Int32.Parse(sizeLine.[0]), Int32.Parse(sizeLine.[1]))
    let board = List.init (fst(boardSize)) (fun _ -> Console.ReadLine())    
    pacman, food, boardSize, board

// DFS visit
let rec visit (prevs: Map<int*int, int*int>) stack visited = 
    let neighbors (posR, posC) = 
        [(1, 0); (0, 1); (0, -1); (-1, 0)]
        |> List.map (fun (r, c) -> (r + posR, c + posC))
        |> List.filter (fun (r, c) -> (r >= 0 && r < boardHeight && c >=0 && c < boardWidth) && 
                                      (board.[r].[c] = EMPTY || board.[r].[c] = FOOD) &&
                                      (not (Map.containsKey (r, c) prevs)))

    let findPath (prevs: Map<int*int, int*int>) =
        let rec find (prevs: Map<int*int, int*int>) acc element =
            if element = pacman then acc
            else
                let prev = prevs.[element]
                find prevs (prev :: acc) prev
        find prevs [food] food        
    
    match stack with
    | s :: ss -> if s = food then 
                        findPath prevs, List.rev (s :: visited)
                    else
                        let nodeNeighbors = neighbors s
                        let newPrevs = List.fold (fun acc nr -> Map.add nr s acc) prevs nodeNeighbors
                        visit newPrevs (nodeNeighbors @ stack) (s :: visited)
    | [] -> failwith "no path found (search stack empty)!"

let printList xs =     
    List.iter (fun e -> printfn "%d %d" (fst(e)) (snd(e))) xs

let (path, visited) = visit (Map.empty.Add(pacman, pacman)) [pacman] []

printfn "%d" (List.length visited)
printList visited
printfn "%d" ((List.length path) - 1)
printList path
