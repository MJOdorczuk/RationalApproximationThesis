namespace FunctionUtils

open System
open System.Numerics
open MiscFunctions

module DerivativesUtils =
    let circle = [0.0 .. 0.00001 .. (Math.PI * 2.0)]

    let circleLen = Seq.length circle

    let print z =
        printf "%A\n" z
        z

module Derivatives =
    open DerivativesUtils

    type Complex = System.Numerics.Complex

    let Cauchy f n z0 =
        let sum =
            circle
            |> Seq.map (fun a -> Complex(cos a, sin a))
            |> Seq.map (fun z -> z + z0)
            |> Seq.pairwise
            |> Seq.map (fun (z, w) -> 
                (w - z) * f z / Complex.Pow (z - z0, float n + 1.0))
            |> Seq.fold (+) (Complex(0.0, 0.0))
        sum * float (Fact n) / Complex (0.0, 2.0 * Math.PI)
        
    let DerivativeList f n x =
        [0..n]
        |> List.map (fun i -> Cauchy f i (Complex(x, 0.0)))
        |> List.map print
        |> List.map (fun z -> z.Real)