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
    FunctionUtils.Derivatives.DerivativeList sin 20 0.0
    |> printf "%A\n"
    let scycle = [ 0.; 1.; 0.; -1.]
    let sinDevs = [ for i in 0..300 -> scycle.[i % 4]]
    let Psin = Pade.NumericallyDifferentiated sin 0.0 0 10
    let Tsin = Taylor.NumericallyDifferentiated sin 10 0.0
    let limit5 = Limit -5. 5.
    Chart.Line [
        for s in [Psin] -> [
            for x in -10.0 .. 0.001 .. 10.0 -> (x, x |> Evaluate s |> limit5)]]
    |> Chart.Show
    Chart.Line [
        for s in [Tsin] -> [
            for x in -10.0 .. 0.001 .. 10.0 -> (x, s.Evaluate x |> limit5)]]
    |> Chart.Show
    printf "%A\n" Psin
    0
