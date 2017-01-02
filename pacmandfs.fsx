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
let visit (prevs: Map<int*int, int*int>) stack = 
    let neighbors (posR, posC) = 
        //[(-1, 0); (0, -1); (0, 1); (1, 0)]
        [(1, 0); (0, 1); (0, -1); (-1, 0)]
        |> List.map (fun (r, c) -> (r + posR, c + posC))
        |> List.filter (fun (r, c) -> (r >= 0 && r < boardHeight && c >=0 && c < boardWidth) && 
                                      (board.[r].[c] = EMPTY || board.[r].[c] = FOOD) &&
                                      (not (Map.containsKey (r, c) prevs)))    

    let findPath (prevs: Map<int*int, int*int>) stack =
        [(0, 0)]
    
    match stack with
    | s :: ss -> if s = food then 
                        (findPath prevs stack) 
                    else
                        let nodeNeighbors = neighbors s
                        let newPrevs = List.fold (fun acc nr -> Map.add nr s acc) prevs nodeNeighbors
                        findPath (prevs) (nodeNeighbors @ stack)
    | [] -> failwith "no path found (search stack empty)!"


//// grid
//visit prevs stack =
//    if goal(stack.head)
//        Some stack.head, prevs
//    else
//        visit (prevs + ns(stack.head, prevs))
