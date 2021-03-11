module Polynomials.Operations

open Interfaces

let ZERO = Polynomial([0.0])
let ONE = Polynomial([1.0])
let X = Polynomial([0.0; 1.0])

let coefficients (p : Polynomial) : float list = List.ofArray p.Coefficients

let reversedCoefficients : Polynomial -> float list = 
    coefficients >> List.rev

let fromCoefficients (coeffs : float list) : Polynomial = 
    Polynomial (Array.ofList coeffs)

let fromReversedCoefficients : float list -> Polynomial =
    List.rev >> fromCoefficients

let leading : Polynomial -> float =
    reversedCoefficients
    >> List.head

let order (p : Polynomial) : int =
    p.Degree

let equals (p : Polynomial) (q : Polynomial) : bool =
    let rec aux c1 c2 =
        match c1, c2 with
        | [], [] -> true
        | a::ta, b::tb -> if a = b then aux ta tb else false
        | _ -> false
    aux (coefficients p) (coefficients q)

let eval (p : Polynomial) : float -> float = p.Evaluate

let add (p : Polynomial) (q : Polynomial) : Polynomial =
    p + q

let scale (p : Polynomial) (c : float) : Polynomial =
    p * c

let multiply (p : Polynomial) (q : Polynomial) : Polynomial =
    p * q

let power (p : Polynomial) (c : int) : Polynomial =
    let rec aux c result factor =
        match c with
        | 0 -> result
        | _ -> 
            match c % 2 with
            | 0 -> aux (c / 2) result (factor * factor)
            | 1 -> aux (c / 2) (result * factor) (factor * factor)
    aux c ONE p

let powerX = power X

let apply (inner : Polynomial) : Polynomial -> Polynomial =
    coefficients
    >> List.indexed
    >> List.map (fun (i, a) -> a * (power inner i))
    >> List.fold (+) ZERO