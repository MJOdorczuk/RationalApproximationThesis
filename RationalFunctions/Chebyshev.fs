module RationalFunctions.Chebyshev

open Polynomials.Operations

let NumeratorList n =
    let den = X + ONE
    let num = X - ONE
    let rec aux prev cur i =
        match i with
        | 0 -> [cur]
        | _ -> cur::(aux cur (2. * num * cur - prev * den) (i - 1))
    let numeratorBase = 
        match n with
        | 0 -> [ONE]
        | _ -> ONE::(aux ONE num (n - 1))
    let rec promote nums result promotor =
        match nums with
        | head::tail -> promote tail ((head * promotor)::result) (promotor * den)
        | _ -> result
    promote (List.rev numeratorBase) [] ONE