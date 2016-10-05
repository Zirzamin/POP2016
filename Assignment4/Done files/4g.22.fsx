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
let rec rev i =                 //Funktion til at vende lister om
    match i with
    |[] -> []
    |[u] -> [u]
    |p::q -> rev q @ [p]
let rec removeDuplicates (xs : 'a list) =
    let f h t =                 //Definerer funktionen som 'tjekker' og fjerner
        match t with
        | [] -> [h]
        | _ ->
            if List.exists(fun x -> x = h) t = false then h :: t
            else t
    xs                          //Tager inputtet
    |> rev
    |> fun x -> List.foldBack f x [] //Kalder tjek- og fjernfunktionen på xs
    |> rev
printfn "               Remove Duplicates"
printfn "Test1: %b" (removeDuplicates [1;2;1;3;2] = [1;2;3])
printfn "Test2: %b" (removeDuplicates [1;4;1;1;2;2;3]=[1;4;2;3])
printfn "Test3: %b" (removeDuplicates [1;2;3;4;5;5;4;3;2;1]=[1;2;3;4;5])
