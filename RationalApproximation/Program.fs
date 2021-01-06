// Learn more about F# at http://fsharp.org

open System
open XPlot.Plotly
open PolynomialApproximation.Taylor

type Complex = System.Numerics.Complex

[<EntryPoint>]
let main argv =
    //Chart.Line [for x in 1.0 ..100.0 -> (x, x**2.0)]
    //|> Chart.Show
    let sinT = FromDerivatives [1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0] 0.0
    let sinT2 = NumericallyDifferentiated exp 7 0.0
    Chart.Line [
        for s in [sinT; sinT2] -> [
            for x in -3.0 .. 0.001 ..3.0 -> (x, s.Evaluate x)]]
    |> Chart.Show
    0
