namespace FunctionUtils

open System
open System.Numerics

module MiscFunctions =
    let Fact n =
        let rec aux n result =
            match n with
            | 0 -> result
            | _ -> aux (n - 1) (result * n)
        aux n 1