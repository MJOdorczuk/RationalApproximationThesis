namespace PolynomialApproximation

open Polynomials.Operations
open Polynomials.Interfaces
open FunctionUtils.Derivatives
open FunctionUtils.MiscFunctions

module Taylor = 

    let FromDerivatives (ds : float list) (x : float) : Polynomial =
        ds
        |> List.indexed
        |> List.map (fun (i, d) -> (power i X) * d / (float (Fact i)))
        |> List.fold (+) ZERO
        |> apply (X - ONE * x)

    let NumericallyDifferentiated f degree x =
        let ds = DerivativeList f degree x
        FromDerivatives ds x
