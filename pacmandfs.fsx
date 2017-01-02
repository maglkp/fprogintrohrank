open System

// tile defs
let WALL = '%'
let EMPTY = '-'
let PACMAN = 'P'
let FOOD = '.'
let NO_MOVE = (-1, -1)


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

let visit prevs stack = 
    let neighbors (posR, posC) = 
        let rMin, rMax = max (posR-1) 0, min (posR+1) (boardHeight-1)
        let cMin, cMax = max (posC-1) 0, min (posC+1) (boardWidth-1)
        seq {
            for i = rMin to rMax do
                for j = cMin to cMax do

        } |> List.ofSeq

    match stack with
    | s :: ss -> Some 1
    | [] -> None


//// grid
//visit prevs stack =
//    if goal(stack.head)
//        Some stack.head, prevs
//    else
//        visit (prevs + ns(stack.head, prevs))
