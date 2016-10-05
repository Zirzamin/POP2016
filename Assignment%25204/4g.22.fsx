let rec rev i =
    match i with
    |[] -> []
    |[u] -> [u]
    |p::q -> rev q @ [p]
let rm(lst : 'a list) =
    let f h t =
        match t with
        | [] -> [h]
        | _ ->
            if List.exists(fun x -> x = h) t = false then h :: t
            else t
    lst
    |> rev
    |> fun x -> List.foldBack f x []
    |> rev
printf "%A" (rm [1;4;1;1;2;2;3])
printf "%A" (rm [1;2;1;2;3])
printf "%A" (rm [1;2;3;4;5;5;4;3;2;1])
