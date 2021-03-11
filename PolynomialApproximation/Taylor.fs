namespace PolynomialApproximation

open Polynomials.Operations
open Polynomials.Interfaces
open FunctionUtils.Derivatives
open FunctionUtils.MiscFunctions

module Taylor = 

    let Taylorize =
        List.mapi (fun i d -> d / (float (Fact i)))

    let FromDerivatives (ds : float list) (x : float) : Polynomial =
        ds
        |> Taylorize
        |> List.mapi (fun i t -> (powerX i) * t)
        |> List.fold (+) ZERO
        |> apply (X - ONE * x)

    let NumericallyDifferentiated f degree x =
        let ds = DerivativeList f degree x
        FromDerivatives ds x
