open System

type Point = {X: double; Y: double;}

let readPoints() = 
    let readPoint() = 
        let nums = Console.ReadLine().Split(' ')
        {X = Double.Parse(nums.[0]); Y = Double.Parse(nums.[1])}
    let cnt = Int32.Parse(Console.ReadLine())
    List.init cnt (fun _ -> readPoint())

// Distance between two points    
let distance (p1, p2) = 
    Math.Sqrt((p1.X - p2.X)**2. + (p1.Y - p2.Y)**2.)

// Radial angle between two points
let getRadialDistance p1 p2 = 
    let y = p1.Y - p2.Y
    let x = p1.X - p2.X
    Math.Atan2(y, x)

let pointsOnCcwTurn p1 p2 p3 =     
    (p2.X - p1.X)*(p3.Y - p1.Y) - (p2.Y - p1.Y)*(p3.X - p1.X) > 0.
    
let pointsOnCcwTurn1 p1 p2 p3 =     
    // special case is p1 and p2 lie on a vertical line to prevent /0
    if Math.Abs(p1.X - p2.X) < 0.00001 then 
        p3.X < p1.X
    else
        // compute the slope and intercept of the line through p1 and p2        
        let a = (p2.Y - p1.Y) / (p2.X - p1.X) 
        let b = p1.Y - a*p1.X

        let p1p2vectorPointsRight = p2.X > p1.X
        // check if the other point is below the line for p1-p2 vector pointing left and above for vector poining right
        let aboveLine = (a * p3.X + b < p3.Y)
        let belowLine = (a * p3.X + b > p3.Y)
        if p1p2vectorPointsRight then
            aboveLine
        else        
            belowLine        
    
let rec reduceCwTurns points = 
    match points with 
    | p1 :: p2 :: p3 :: ps -> if pointsOnCcwTurn p3 p2 p1 then 
                                        points 
                                      else                                    
                                        reduceCwTurns(p1 :: p3 :: ps)
    | _ -> points
    
let rec createHull pointsByAngle constructedHull = 
    match pointsByAngle with
    | p :: ps -> reduceCwTurns (p :: constructedHull)
                    |> createHull ps                 
    | [] -> constructedHull


let points = readPoints()
//let points = [{X=1.;Y=1.}; {X=2.;Y=5.}; {X=5.;Y=3.}; {X=2.;Y=2.}; {X=3.;Y=3.};]
//let points = [{X=3.;Y=2.}; {X=2.;Y=5.}; {X=4.;Y=5.}]

let minYpt = (List.sortBy (fun p -> p.Y) points).[0]

// All points besides the one with smallest Y
let otherPoints = points |> List.filter (fun pt -> not(pt.Equals(minYpt)))

// Add radial angle between point and the point of minimum Y coordinate
let pointsWithDeg = otherPoints |> List.map(fun pt -> (pt, getRadialDistance pt minYpt, distance(minYpt, pt))) 

// Sort it by radial angle and then by distance from from point with smallest Y
let pointsWithDegSorted = pointsWithDeg |> List.sortBy (fun (pt, angle, dist) -> (angle, dist))

// Strip angle and distance
let pointsSorted = pointsWithDegSorted |> List.map (fun (a,b,c) -> a)

// Add original point
let allPointsSorted = minYpt :: pointsSorted

// Create Space Hull
let convexHull = createHull allPointsSorted []

// Add starting point again
let perimeter = minYpt :: convexHull

// sum distances
let perimeterLen = perimeter 
                    |> List.pairwise 
                    |> List.map distance 
                    |> List.reduce (+)

printfn "%f" perimeterLen

// Print 
//perimeter  |> List.iter (fun p -> (printfn "(%f,%f)" p.X p.Y))








