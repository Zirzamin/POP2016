(* HR 4.15 *)

///<summary>
///Givet et input som er en liste med lister returnerer listen tilbage
///med listerne og deres elementer i omvendt rækkefølge
///</summary>
///<params name="xs">
///'a list list, liste med lister, i dette tilfælde testes med int-lister
///</params>
///<returns>
///'a list list, den omvendte rækkefølge af input
///</returns>
let rec rev l =
    match l with
    |[] -> []
    |[x] -> [x]
    |h::t -> rev t @ [h]
let rec revrev (xs : 'a list list) =
    match xs with
    |[] -> []
    |[x] -> [rev(x)]
    |h::t -> revrev(t) @ [rev(h)]
printfn "               HR 4.15"
printfn "Test1: %b" (revrev [[1;2];[3;4;5]] = [[5;4;3];[2;1]])
printfn "Test2: %b" (revrev [[1;1;2;3];[1;1;1;2;2];[1;2;3;4;5;6;7;8;9]] = [[9;8;7;6;5;4;3;2;1];[2;2;1;1;1];[3;2;1;1]])
printfn "Test3: %b" (revrev [[1;1;2;3];[1;1;1;2;2];[1;2;3;4;5;6;7;8;19029340]] = [[19029340;8;7;6;5;4;3;2;1];[2;2;1;1;1];[3;2;1;1]])
printfn "Test4: %A" (revrev [[1;1;2;31;1;1;2;21;2;3;4;5;6;7;8;19029340]] = [[19029340;8;7;6;5;4;3;2;21;2;1;1;31;2;1;1]])

(* 4g.2 *)
printfn "             OPGAVE 4g.2"
(* Remove duplicates *)

(* Rettede stavefejl i template *)

///<summary>
///Tager en 'a liste som input og returnerer listen efter at have fjernet
///alle gentagne elementer, samtidig med at første forekomst bevares
///</summary>
///<params name="xs">
///'a list
///</params>
///<returns>
///En liste hvor kun den første forekomst af et element bevares
///</returns>
///<remarks>
///Bruger List.foldBack og List.exists.
///Definerer en funktion f h t først som den så kalder på sit input 'bagfra'
///</remarks>
let rec rev2 i =                 //Funktion til at vende lister om
    match i with
    |[] -> []
    |[u] -> [u]
    |p::q -> rev2 q @ [p]
let rec removeDuplicates (xs : 'a list) =
    let f h t =                 //Definerer funktionen som 'tjekker' og fjerner
        match t with
        | [] -> [h]
        | _ ->
            if List.exists(fun x -> x = h) t = false then h :: t
            else t
    xs                          //Tager inputtet
    |> rev2
    |> fun x -> List.foldBack f x [] //Kalder tjek- og fjernfunktionen på xs
    |> rev2
printfn "             Remove Duplicates"
printfn "Test1: %b" (removeDuplicates [1;2;1;3;2] = [1;2;3])
printfn "Test2: %b" (removeDuplicates [1;4;1;1;2;2;3]=[1;4;2;3])
printfn "Test3: %b" (removeDuplicates [1;2;3;4;5;5;4;3;2;1]=[1;2;3;4;5])
