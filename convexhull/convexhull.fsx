open System

type Point = {X: double; Y: double;}

let readPoints() = 
    let readPoint() = 
        let nums = Console.ReadLine().Split(' ')
        {X = Double.Parse(nums.[0]); Y = Double.Parse(nums.[1])}
    let cnt = Int32.Parse(Console.ReadLine())
    //let points: Point array = Array.zeroCreate cnt 
    //points |> Array.map (fun _ -> readPoint())
    Array.init cnt (fun _ -> readPoint())

let points = readPoints()
let minYpt = (Array.sortBy (fun p -> p.Y) points).[0]

let getRadialDistance p1 p2 = 
    let y = Math.Abs(p1.Y - p2.Y)
    let x = Math.Abs(p1.X - p2.X)
    Math.Atan2(y, x)

let otherPoints = points |> Array.filter (fun pt -> not(pt.Equals(minYpt)))
// Add radial angle between point and the point of minimum Y coordinate
let pointsWithDeg = points |> Array.map(fun pt -> (pt, getRadialDistance pt minYpt))



otherPoints |> Array.iter (fun p -> printfn "(%f,%f)" p.X p.Y)
//Array.pairwise
