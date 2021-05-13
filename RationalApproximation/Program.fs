// Learn more about F# at http://fsharp.org

open System
open XPlot.Plotly
open PolynomialApproximation
open RationalApproximation
open RationalFunctions.Operations
open FunctionUtils.MiscFunctions

type Complex = System.Numerics.Complex

[<EntryPoint>]
let main argv =
    let f x = cos (10. * x)
    let g x = exp x
    (Orthogonals.DecomposeWithFirstKind f 20)
    |> List.zip (Orthogonals.DecomposeWithSecondKind f 20)
    |> List.zip (Orthogonals.DecomposeWithFirstKind g 20)
    |> List.zip (Orthogonals.DecomposeWithSecondKind g 20)
    |> List.iteri (fun i (g2, (g1, (f2, f1))) -> printf "%d & %.5f & %.5f & %.5f & %.5f \\\\\n" i f1 f2 g1 g2)
    let p = Orthogonals.DecomposeWithFirstKind g 20 
            |> Orthogonals.BuildFirstKind
    let q = Orthogonals.DecomposeWithSecondKind g 20
            |> Orthogonals.BuildSecondKind
    //FunctionUtils.Derivatives.DerivativeList sin 20 0.0
    //|> printf "%A\n"
    //let scycle = [ 0.; 1.; 0.; -1.]
    //let sinDevs = [ for i in 0..300 -> scycle.[i % 4]]
    //let maxDegree = 5
    let limit5 = Limit -5. 5.
    //[0..maxDegree]
    //|> List.rev
    //|> List.zip [0..maxDegree]
    //|> List.map (fun (i, j) -> printf "%i %i\n" i j; i, j)
    //|> List.map (fun (n, m) -> Pade.NumericallyDifferentiated exp 0.0 n m)
    //|> List.map (fun (p, q) -> printf "P:%A\nQ:%A\n\n" p q; p,q)
    //|> List.map (fun p -> Chart.Line [for x in -10. .. 0.001 .. 10. -> (x, x |> Evaluate p |> limit5)])
    //|> List.map Chart.Show
    //|> ignore
    //let Tsin = Taylor.NumericallyDifferentiated sin 10 0.0
    //Chart.Line [
    //    for s in [Tsin] -> [
    //        for x in -10.0 .. 0.001 .. 10.0 -> (x, s.Evaluate x |> limit5)]]
    //|> Chart.Show
    Chart.Line [[for x in -1. .. 0.001 .. 1. -> (x, (p.Evaluate x - g x)|> limit5)]]
    |> Chart.Show
    Chart.Line [[for x in -1. .. 0.001 .. 1. -> (x, (q.Evaluate x - g x)|> limit5)]]
    |> Chart.Show
    0
