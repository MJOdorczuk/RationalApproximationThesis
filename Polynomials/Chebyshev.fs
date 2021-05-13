module Polynomials.Chebyshev

open Polynomials.Operations

let FirstKindList n =
    let rec aux prev cur i =
        match i with
        | 0 -> [cur]
        | _ -> cur::(aux cur (2. * X * cur - prev) (i - 1))
    match n with
    | 0 -> [ONE]
    | _ -> ONE::(aux ONE X (n - 1))

let SecondKindList n =
    let rec aux prev cur i =
        match i with
        | 0 -> [cur]
        | _ -> cur::(aux cur (2. * X * cur - prev) (i - 1))
    aux ZERO ONE n