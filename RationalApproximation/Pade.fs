namespace RationalApproximation

open MathNet.Numerics.LinearAlgebra
open RationalFunctions.Interfaces
open Polynomials.Interfaces
open Polynomials.Operations
open PolynomialApproximation.Taylor
open FunctionUtils.Derivatives

module Pade =
    
    let FromDerivatives (ds : float list) (x : float) (n : int) (m : int) : Rational =
        let ts = Taylorize ds
        let a i =
            if i < 0 || i >= ts.Length
            then 0.0
            else ts.[i]
        let QM = DenseMatrix.init m m (fun x y -> a (n - y + x - 1))
        printf "%A\n" QM
        let Qx = DenseVector.init m (fun y -> - a (n + y + 1))
        printf "%A\n" Qx
        let Q = (Polynomial ((QM.Solve Qx).ToArray ())) * X + ONE
        printf "%A\n" Q
        let qs = Q.Coefficients
        let q i =
            if i < 0 || i >= qs.Length then 0.0 else qs.[i]
        let p i =
            [0..i]
            |> List.map (fun j -> (a j) * q (i - j))
            |> List.sum
        let ps = [|for i in 0 .. n -> p i|]
        let P = Polynomial ps
        let inner = X - ONE * x
        apply inner P, apply inner Q

    let NumericallyDifferentiated f x n m =
        let ds = DerivativeList f (n + m + 1) x
        FromDerivatives ds x n m