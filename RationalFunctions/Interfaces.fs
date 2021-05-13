module RationalFunctions.Interfaces

open Polynomials.Interfaces
open Polynomials.Operations
open Polynomials.Chebyshev

type Polynomial = Polynomials.Interfaces.Polynomial

type Rational = Polynomial * Polynomial

type Chebyshev (coeffs: float list) =
    let denBase = X + ONE
    let numBase = X - ONE
    let den =
        power denBase
    let n = List.length coeffs
    let Numerator n =
        let rec aux prev cur n =
            match n with
            | 0 -> cur
            | _ -> aux cur (2. * numBase * cur - denBase * prev) (n - 1)
        match n with
        | 0 -> ONE
        | 1 -> numBase
        | _ -> aux ONE numBase (n - 1)
    member public __.AsRational =
        coeffs
        |> List.mapi (fun i c -> c * Numerator i * den (n - i))
        |> List.sum
    