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

    let SafeAccess (l: 'a list) def i =
        if i < 0 || i >= l.Length then def else l.[i]

    let SafeSum l =
        let rec aux l acc =
            match l, acc with
            | a::b::tail, _ -> aux tail ((a+b)::acc)
            | [a], b::tail -> aux ((a+b)::tail) []
            | [a], _ -> a
            | [], [] -> 0.
            | [], _ -> aux acc []
        aux l []

    let InnerProduct domain w f g =
        domain
        |> List.pairwise
        |> List.map (fun (a, b) -> ((a + b) / 2.), b - a)
        |> List.map (fun (x, delta) -> (w x) * (f x) * (g x) * delta)
        |> List.sum