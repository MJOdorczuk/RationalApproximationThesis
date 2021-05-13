namespace PolynomialApproximation

open Polynomials.Operations
open FunctionUtils.Derivatives
open FunctionUtils.MiscFunctions

module Taylor = 

    let Taylorize =
        List.mapi (fun i d -> d / (float (Fact i)))

    let FromDerivatives ds degree x =
        ds
        |> List.take (degree + 1)
        |> Taylorize
        |> List.rev
        |> List.fold (fun acc t -> (acc * X + t)) ZERO
        |> apply (X - ONE * x)

    let NumericallyDifferentiated f degree x =
        let ds = DerivativeList f degree x
        FromDerivatives ds degree x
