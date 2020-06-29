open System
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics

let rng = Random()
let rand min max = rng.NextDouble() * (max - min) + min
let randBool prob = rng.NextDouble() <= prob

let initPopulation size dim (min, max) = DenseMatrix.ofRowArrays [| for _ in 1..size -> [| for _ in 1..dim -> double(rand min max) |] |]

let pickThreeDistinct except size = 
    let [|a;b;c;|] = 
        Combinatorics.GeneratePermutation(size, rng) 
        |> Array.filter (fun v -> v <> except) 
        |> Array.take 3

    (a,b,c)

let epoch cr f cost (mat: Matrix<double>) = 
    Matrix.mapRowsInPlace (fun i row ->
        let a, b, c = pickThreeDistinct i mat.RowCount
        let av, bv, cv = (mat.Row(a), mat.Row(b), mat.Row(c))
        
        let newRow = row.Clone()
        for vi in 0..mat.ColumnCount-1 do
            if randBool cr then
                newRow.[vi] <- av.[vi] + f*(bv.[vi] - cv.[vi])

        if cost(newRow) < cost(row) then
            newRow
        else row
    
    ) mat

let randRange = (-5.0, 5.0)
let popSize = 20
let paramSize = 1

let CR = 0.5
let F = 1.5
let cost (row: Vector<double>) = row.[0] * row.[0]

let mutable pop = initPopulation popSize paramSize randRange

let timer = new System.Diagnostics.Stopwatch()
timer.Start()

for i in 1..10 do
    printfn "%A" pop
    epoch CR F cost pop

timer.Stop()
printfn "Elapsed %dms" timer.ElapsedMilliseconds

// TODO: BTFO this mapping
let best = (pop.ToRowArrays()
    |> Array.map DenseVector.ofArray 
    |> Array.map (fun vec -> (vec, abs(cost(vec))) )
    |> Array.map (fun (vec, cost) -> (vec.ToArray(), cost) )
    |> Array.minBy (fun (_, cost) -> cost))
    
printfn "Best %A" best

