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
//        elif x < y then x :: merge (xs, y :: ys)
        else y :: merge (x :: xs, ys)
 
let rec sortList l =
    match l with
    | [] -> [] 
    | [n] -> [n] 
    | l ->
        let (xs, ys) = split l [] [] 
        merge (sortList xs, sortList ys)


//Balck box testing
let test1 = [1;2;3;4;5;6]
let test2 = [6..1]

let test3 = [5;5;5;5;5;5]



let test4 = []
let test5 = [1]

let test6 = [9;1;1;1;5;5]

let test7 = [9;1;1;5;5;1]

let test8 = [1;2;3;4;4;3;2;1]

let test9 = ["b";"a";"d";"A";"e"] // 

let test1 = [3;-5;'a';3;9;1] // false


sortList test8

let rec rm l  =
    match l with 
    | [] -> []
    | [n] -> [n]
    | x :: x1 ->
        if x = rm(x1).[0] then x::tempos
//        elif x < y then x :: merge (xs, y :: ys)
        else rm x1


let test8 = [1;2;3;4;4;3;2;6]
rm test8




