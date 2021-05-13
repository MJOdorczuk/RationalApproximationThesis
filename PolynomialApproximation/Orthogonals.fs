namespace PolynomialApproximation

open FunctionUtils.MiscFunctions
open Polynomials.Chebyshev
open System

module Orthogonals = 

    let FirstKind n x =
        cos (float n * acos x)

    let SecondKind n x =
        let theta = acos x
        (sin (float(n + 1) * theta)) / sin theta

    let FirstWeight x =
        1. / sqrt(1. - x * x)

    let SecondWeight x =
        sqrt(1. - x * x)

    let DecomposeWithFirstKind f n =
        match 
            [0..n]
            |> List.map FirstKind
            |> List.map (InnerProduct [-1. .. 0.00001 .. 1.] FirstWeight f)
            |> List.map (fun x -> x * 2. / Math.PI) with
        | head::tail -> (head / 2.)::tail
        | [] -> []

    let DecomposeWithSecondKind f n =
        [0..n]
        |> List.map SecondKind
        |> List.map (InnerProduct [-1. .. 0.00001 .. 1.] SecondWeight f)
        |> List.map (fun x -> x * 2. / Math.PI)

    let Build (chebs: int -> Polynomials.Interfaces.Polynomial list) (l: float list) =
        List.length l - 1
        |> chebs
        |> List.zip l
        |> List.map (fun (a, b) -> a * b)
        |> List.sum

    let BuildFirstKind = Build FirstKindList
    let BuildSecondKind = Build SecondKindList
