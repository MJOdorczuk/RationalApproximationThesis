module Utils

open System
open Polynomials.Operations

let rnd = Random()

let randomPoly maxDegree =
    let degree = rnd.Next(0, maxDegree)
    let rec aux degree acc =
        match degree with
        | 0 -> acc
        | _ -> aux (degree - 1) ((rnd.NextDouble ())::acc)
    aux degree []
    |> fromCoefficients