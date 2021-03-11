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
    let sinT2 = Taylor.NumericallyDifferentiated sin 11 0.0
    // let sinP1 = Pade.FromDerivatives [0.0; 1.0; 0.0; -1.0; 0.0; 1.0; 0.0; -1.0; 0.0; 1.0; 0.0; -1.0; 0.0; 1.0; 0.0; -1.0] 0.0 6 6
    // let sinP3 = Pade.FromDerivatives [0.0; 1.0; 0.0; -1.0; 0.0; 1.0; 0.0; -1.0; 0.0; 1.0; 0.0; -1.0; 0.0; 1.0; 0.0; -1.0] 0.0 11 0
    // let sinP4 = Pade.FromDerivatives [0.0; 1.0; 0.0; -1.0; 0.0; 1.0; 0.0; -1.0; 0.0; 1.0; 0.0; -1.0; 0.0; 1.0; 0.0; -1.0] 0.0 1 5
    let sinP2 = Pade.NumericallyDifferentiated sin 0.0 2 6
    printf "%A\n" sinP2
    let limit5 = Limit -5. 5.
    Chart.Line [
        for s in [sinT2] -> [
            for x in -3.0 .. 0.001 ..3.0 -> (x, s.Evaluate x)]]
    |> Chart.Show
    Chart.Line [
        for s in [sinP2] -> [
            for x in -3.0 .. 0.001 ..3.0 -> (x, Evaluate s x |> limit5)]]
    |> Chart.Show
    0
