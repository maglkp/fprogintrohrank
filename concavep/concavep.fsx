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
    (p2.X - p1.X)*(p3.Y - p1.Y) - (p2.Y - p1.Y)*(p3.X - p1.X) >= 0.

let points = readPoints()
//let points = [{X=0.;Y=0.}; {X=0.;Y=1.}; {X=1.;Y=1.}; {X=1.;Y=0.};]


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

let concave = allPointsSorted 
                |> Seq.ofList 
                |> Seq.windowed 3
                |> List.ofSeq
                |> List.exists (fun ps -> not (pointsOnCcwTurn ps.[0] ps.[1] ps.[2]))

if concave then printfn "YES" else printfn "NO"