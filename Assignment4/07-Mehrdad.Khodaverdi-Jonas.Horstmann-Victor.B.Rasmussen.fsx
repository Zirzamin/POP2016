(* Opgave 4g
 *
 * Group: 7
 *   Mehrdad Khodaverdi ctm546@alumni.ku.dk
 *   Jonas Horstmann Qzj408@alumni.ku.dk
 *   Victor B. Rasmussen cwv180@alumni.ku.dk
 *)


(* 4g.1 *)
printfn "             OPGAVE 4g.1"
(*HR 4.11 *)
(* 1 *)

///<summary>
///Tager en int liste 'l' og en int 'i' som input og returnerer en int. 
///count tæller i listen 'l' hvor mange gange 'i' forekommer
///og returnere svaret
///</summary>
///<params name="l">
///int list 
///</params>
///<params name="i">
///int 
///</params>
///<returns>
///int number of i in l
///</returns>


let rec count (l:int list, i:int) = 
    match l with
    | [] -> 0
    | h :: t -> 
        if h = i then
            1 + count(t,i)
        else count(t,i)

printfn "    HR 4.11 (4.)     count(int list,int)"
printfn "Test1: %b" (count([1;2;1;3;2],1) = 2)
printfn "Test2: %b" (count([1;4;2;1;2;2;3;2;2],2) = 5)
printfn "Test3: %b" (count([1;2;3;4;5;5;4;3;2;1],6) = 0)
printfn "Test4: %b" (count([],1) = 0)
printfn "Test5: %b" (count([1],1) = 1)


(* 2 *)

(* Kommentarer til løsningen af denne opgave skrives her *)

///<summary>
///Tager en int liste 'l' og en int 'i' som input og returnerer en int liste.
///insert tilføjer en int 'i' på det rigtige plads i den sorteret liste 'l'.
///</summary>
///<params name="l">
///int list der er sorteret
///</params>
///<params name="i">
///int 
///</params>
///<returns>
///returnere en sorteret liste hvor 'i' er tilføjet til listen.
///</returns>

let rec insert (l:int list, i:int) : int list = 
    match l with
    | [] -> [i]
    | h :: t when h < i -> h :: insert(t,i) 
    | h :: t when h > i -> i :: l
    | h :: t -> h :: i :: t

printfn "    HR 4.11 (2.)      insert(int list,int)"
printfn "Test1: %b" (insert([1;2;2;3;3;5;5],0) = [0;1;2;2;3;3;5;5])
printfn "Test2: %b" (insert([1;2;2;3;3;5;5],2) = [1;2;2;2;3;3;5;5])
printfn "Test3: %b" (insert([1;2;2;3;3;5;5],4) = [1;2;2;3;3;4;5;5])
printfn "Test4: %b" (insert([1;2;2;3;3;5;5],6) = [1;2;2;3;3;5;5;6])
printfn "Test5: %b" (insert([],1) = [1])
printfn "Test6: %b" (insert([1;2;2;3;3;5;5],6) = [1;2;2;3;3;5;5;6])


(* 3 *)

///<summary>
///Tager int liste 'l1' og en int liste 'l2' som input og returnerer en int liste.
///intersect sammenligner to sorterede lister og returnere en sorteret liste
///med elementer der kun eksistere i begge inputtede lister.
///</summary>
///<params name="l1">
///int list der er sorteret
///</params>
///<params name="l2">
//int list der er sorteret
///</params>
///<returns>
///returnere en sorteret liste med elementer der kun eksistere i 
///begge inputtede lister.
///</returns>

let rec intersect (l1 : int list, l2 : int list) : int list =
    match (l1,l2) with
    |(h::t,h2::t2) ->
        if h = h2 then h :: intersect(t,t2)
        elif h<h2 then intersect(t, l2)
        else intersect(l1,t2)
    |_,_ -> []

printfn "    HR 4.11 (3.)      intersect(int list,int list)"
printfn "Test1: %b" (intersect([1;1;1;2;2],[1;1;2;3;3;3;3;4]) = [1;1;2])
printfn "Test2: %b" (intersect([1;2],[2;4]) = [2])
printfn "Test3: %b" (intersect([],[1;1;2;3;3;3;3;4]) = [])
printfn "Test4: %b" (intersect([1;1;1;2;2],[]) = [])
printfn "Test5: %b" (intersect([1;1;2;2;3;4],[1;1;2;3;3;3;3;4]) = [1;1;2;3;4])


(* 4 *)

///<summary>
///Tager int liste 'l1' og en int liste 'l2' som input og returnerer en int liste.
///plus konkatenere to sorterede lister og returnere en sorteret liste.
///</summary>
///<params name="l1">
///int list der er sorteret
///</params>
///<params name="l2">
//int list der er sorteret
///</params>
///<returns>
///returnere en sorteret liste med alle elementer der eksistere i 
///inputtede lister.
///</returns>

let rec plus (l1 : int list, l2 : int list) : int list =
  match l1 with
  |[]-> l2
  |h::t ->
    match l2 with
      |[] -> l1
        |h2::t2 ->
          if h=h2 then h::plus(t, l2)
          elif h<h2 then h::plus(t, l2)
          else h2::plus(l1, t2)

printfn "    HR 4.11 (4.)      plus(int list,int list)"
printfn "Test1: %b" (plus([1;1;2],[1;2;4]) = [1;1;1;2;2;4])
printfn "Test2: %b" (plus([3;4],[1;2]) = [1;2;3;4])
printfn "Test3: %b" (plus([1;2],[2;4]) = [1;2;2;4])
printfn "Test4: %b" (plus([],[1;1;2;3]) = [1;1;2;3])
printfn "Test5: %b" (plus([1;1;2],[]) = [1;1;2])
printfn "Test6: %b" (plus([],[]) = [])
printfn "Test7: %b" (plus([7],[2]) = [2;7])

(* 5 *)

///<summary>
///Tager int liste 'l1' og en int liste 'l2' som input og returnerer en int liste.
///minus fjerne elementer fra en sorteret liste hvis de eksistere i den anden
///liste og returnere en sorteret liste med de tilbageblevne elementer.
///</summary>
///<params name="l1">
///int list der er sorteret
///</params>
///<params name="l2">
//int list der er sorteret
///</params>
///<returns>
///returnere en sorteret liste hvor elementer fra l2 er fjernet fra l1 
///</returns>

let rec minus (l1 : int list, l2 : int list) : int list =
  match l1 with
  |[]-> []
  |h::t ->
    match l2 with
      |[] -> l1
        |h2::t2 ->
          if h=h2 then minus(t, t2)
          elif h<h2 then h::minus(t, l2)
          else minus(l1, t2)

printfn "    HR 4.11 (5.)      minus(int list,int list)"
printfn "Test1: %b" (minus([1;1;1;2;2],[1;1;2;3]) = [1;2])
printfn "Test2: %b" (minus([1;1;2;3],[1;1;1;2;2]) = [3])
printfn "Test3: %b" (minus([],[2;4]) = [])
printfn "Test4: %b" (minus([1;1;2;3],[1;1;2;3]) = [])
printfn "Test5: %b" (minus([1;1;2],[]) = [1;1;2])
printfn "Test6: %b" (minus([],[]) = [])
printfn "Test7: %b" (minus([7],[2]) = [7])

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

printfn "          HR 4.15   revrev"
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

printfn "         4g.2    Remove Duplicates"
printfn "Test1: %b" (removeDuplicates [1;2;1;3;2] = [1;2;3])
printfn "Test2: %b" (removeDuplicates [1;4;1;1;2;2;3]=[1;4;2;3])
printfn "Test3: %b" (removeDuplicates [1;2;3;4;5;5;4;3;2;1]=[1;2;3;4;5])