module RationalFunctions.Operations

open RationalFunctions.Interfaces

let Evaluate (p: Polynomial, q: Polynomial) (x: float) =
    p.Evaluate x / q.Evaluate x

let EvaluateC (p: Polynomial, q: Polynomial) (z: System.Numerics.Complex) =
    p.Evaluate z / q.Evaluate z