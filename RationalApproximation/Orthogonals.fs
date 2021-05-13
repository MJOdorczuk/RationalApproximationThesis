namespace RationalApproximation

open FunctionUtils.MiscFunctions
open System
open Polynomials.Operations
open RationalFunctions.Chebyshev

module Orthogonals = 

    let Inner (x: float) = (ONE - X).Evaluate x / (ONE + X).Evaluate x

    let Evaluator n x = 
        let x = Inner x
        let x = Math.Pow (2., x)
        if x <= 1. then cos(float n * acos x)
        else cosh (float n * Math.Acosh x)

    let Weight x =
        1. / (sqrt x * (x + 1.))

    let DecomposeWithFirstKind f n =
        match 
            [0..n]
            |> List.map Evaluator
            |> List.map (InnerProduct [0. .. 1.] Weight f)
            |> List.map (fun x -> x * 2. / Math.PI) with
        | head::tail -> (head / 2.)::tail
        | [] -> []

    let Build (l: float list) =
        let n = List.length l - 1
        n
        |> NumeratorList
        |> List.zip l
        |> List.map (fun (a, b) -> a * b)
        |> List.sum, power (X + ONE) n
