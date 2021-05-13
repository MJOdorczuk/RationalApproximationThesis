namespace RationalApproximation

open MathNet.Numerics.LinearAlgebra
open RationalFunctions.Interfaces
open Polynomials.Interfaces
open Polynomials.Operations
open PolynomialApproximation.Taylor
open FunctionUtils.Derivatives
open FunctionUtils.MiscFunctions

module Pade =
    
    let FromDerivatives (ds : float list) (x : float) (m : int) (n : int) : Rational =
        let ts = Taylorize ds
        let t = SafeAccess ts 0.0
        let QM = DenseMatrix.init n n (fun x y -> t (m + x - y))
        let Qx = DenseVector.init n (fun y -> - t (m + y + 1))
        let Q = (Polynomial ((QM.Solve Qx).ToArray ())) * X + ONE
        let qs = Q.Coefficients |> List.ofArray
        let q = SafeAccess qs 0.0
        let p i =
            [0..i]
            |> List.map (fun j -> (t j) * q (i - j))
            |> List.sum
        let ps = [|for i in 0 .. m -> p i|]
        let P = Polynomial ps
        let inner = X - ONE * x
        apply inner P, apply inner Q

    let NumericallyDifferentiated f x n m =
        let ds = DerivativeList f (n + m + 1) x
        FromDerivatives ds x n m