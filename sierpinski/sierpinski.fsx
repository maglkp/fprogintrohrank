open System

// rows and columns size
let R = 32
let C = 63
// empty cell and full cell
let E = '_'
let F = '1'

let N = Int32.Parse(Console.ReadLine())
//let N = 5

let printGrid grid = 
    let maxRow = (Array2D.length2 grid) - 1
    grid |> Array2D.iteri (fun r c e -> if c = maxRow then printf "%c\n" e else printf "%c" e) 
    
let generateEmptyTriangle () =
    let pyramidTipCol = C/2    
    Array2D.init R C (fun row col -> if (Math.Abs(col - pyramidTipCol) <= row) then F else E)
    
let drillHole (grid:char [,]) lvl pt =
    let lowerTriangleBaseLen = int(float(C) / (2. ** float(lvl)))
    let lowerTriangleH = lowerTriangleBaseLen / 2 + 1
    
    let r0 = fst(pt)
    let c0 = snd(pt) + lowerTriangleBaseLen
    let shouldBeDrilled r c =
        let rOk = r <= r0 && r0 - r < lowerTriangleH
        let cOk = Math.Abs(c0 - c) <= r0 - r
        rOk && cOk

    Array2D.init R C (fun r c -> if shouldBeDrilled r c then E else grid.[r,c])

let combine (g1:char [,]) (g2:char [,]) = 
    Array2D.init R C (fun r c -> if g1.[r,c] = F && g2.[r,c] = F then F else E)
    
let rec generate lvl p =
    if lvl = (N+1) then 
        generateEmptyTriangle() 
    else    
        let triangleBaseLen = int(float(C) / (2. ** float(lvl))) 
        let lowerTriangleH = triangleBaseLen / 2 + 1
        
        let drilledLeftT = generate (lvl + 1) p
        let drilledRightT = generate (lvl + 1) (fst(p), snd(p) + triangleBaseLen + 1)
        let drilledTopT = generate (lvl + 1) (fst(p) - lowerTriangleH, snd(p) + triangleBaseLen/2 + 1)
        let children = combine (combine drilledLeftT drilledRightT) drilledTopT
        drillHole children lvl p


printGrid (generate 1 (R-1, 0))