module Assignment4

//let a = [1;2;1;3;2]

let rec split l xs ys =
    match l with 
    | [] -> (xs, ys) 
    | n :: ns -> split ns ys (n :: xs)


let rec merge (xs,ys) =
    match (xs,ys) with 
    | ([], ys) -> ys 
    | (xs, []) -> xs 
    | (x :: xs, y :: ys) ->
        if x = y then merge (xs, y :: ys)
        elif x < y then x :: merge (xs, y :: ys)
        else y :: merge (x :: xs, ys)


let rec sortList l =
    match l with
    | [] -> [] 
    | [n] -> [n] 
    | l ->
        let (xs, ys) = split l [] [] 
        merge (sortList xs, sortList ys)

let a = ["b";"a";"d";"A";"e"]

let a = [3;5;5;3;9;1]

sortList a