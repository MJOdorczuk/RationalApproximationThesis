namespace FunctionUtils

module MiscFunctions =
    let Fact n =
        let rec aux n result =
            match n with
            | 0 -> result
            | _ -> aux (n - 1) (result * n)
        aux n 1

    let Limit lower upper v =
        if v < lower 
        then lower 
        elif v > upper 
        then upper 
        else v