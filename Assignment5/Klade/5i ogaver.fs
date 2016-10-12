module Assignment5

(*i5.1.*)

let concat a =
    List.fold (fun x xs -> xs@x) [] a

let a = [[2]; [6; 4]; [1]]

printfn "%A" (concat a) 

(*i5.2*)

(*i5.2. Brug funktionerne fra Tabel 5.1 i HR (side 94) til at denere en funktion gennemsnit : float list -> float option, der
finder gennemsnittet af en liste af kommatal, safremt dette er veldeneret,
og None, hvis ikke.*)

let test = [1.0;2.1;3.2;4.3;5.4;6.5;7.6;8.7;9.8;10.9]

let mean l =
    if l = [] then None
    else Some(List.foldBack (fun x xs -> x + xs) l 0.0 / float l.Length)

mean test

(*i5.3*)

(*Lav en funktion arraySort : ('a [] -> 'a []) when 'a : comparison, som givet et array re-
turnerer en sorteret udgave af samme array. Lav din lsning uden brug af <- operatoren (altsa
funktionelt).
For eksempel skal sekvensen
let aa = [|1;7;5;2;1|]
let bb = arraySort aa
printfn "%A %A" aa bb
udskrive
[|1;7;5;2;1|] [|1;1;2;5;7|]*)

//
//let rec arraySort l = 
//    match l with
//    |[||] |[|_|] -> l
//    | _ -> 
//        let low = l |> Array.filter ((>)(Array.head l))
//        let high = l |> Array.filter ((<=)(Array.head l))
//        let t = Array.append (arraySort low) [|Array.head l|]
//        let r = Array.append t ((arraySort ((Array.tail) high)))
//        r


let rec arraySort l = 
    match l with
    |[||] |[|_|] -> l
    | _ -> 
        let t = [|Array.head l|] |> Array.append (l |> Array.filter ((>)(Array.head l)) |> arraySort)
        let r = l |> Array.filter ((<=)(Array.head l)) |> Array.tail |>arraySort |> Array.append t
        r

let aa = [|1;7;5;2;1|]
let bb = arraySort aa //[|1;7;5;2;1|] [|1;1;2;5;7|]
printfn "%A %A" aa bb

let aaa = [|1;7;5;2;1|]
let a2 = [|1;2;3;4;5;4;3;2;1;0|]
let result = arraySort a2 //[|1;7;5;2;1|] [|1;1;2;5;7|]
printfn "%A %A" a2 result

(*
http://theburningmonk.com/2013/04/sorting-algorithms-in-f/

let inline quickSortInPlace (arr : 'a []) =
    let rec loop (arr : 'a [], left, right) =
        // if the array has 2 or more items
        if left < right then
            // use the middle element, and sort the elements according to the value of the pivot
            // and return the new idx of the pivot
            let pivotIdx = (left + right) / 2
            let pivotNewIdx = partition(arr, left, right, pivotIdx)
            
            // recursively sort elements either side of the new pivot
            loop(arr, left, pivotNewIdx - 1)
            loop(arr, pivotNewIdx + 1, right)

    loop(arr, 0, arr.Length - 1)
arr
*)

let l = [| 1 ; 2 ; 3 ; 4 |] 

[| 0 ; 1 ; 2 ; 3 ; 4 |] |> Array.filter ((>)(Array.head l))

(*i5.4*)

let arraySortD a =
    for i in a.length
     
        