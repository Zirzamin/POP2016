(*i5.3*)

///<summary>
///arraySort takes an array and returns it sorted.
///</summary>
///<params name="l">
///Array
///</params>
///<returns>
///Array
///</returns>

let rec arraySort l = 
    match l with
    |[||] |[|_|] -> l
    | _ -> 
        let temp = [|Array.head l|] |> Array.append (l |> Array.filter ((>)(Array.head l)) |> arraySort)
        l |> Array.filter ((<=)(Array.head l)) |> Array.tail |>arraySort |> Array.append temp

printfn "    5i.3     arraySort array"
printfn "Test1: %b" ((arraySort [|0.0; 0.0; 0.0; 0.0|]) = [|0.0; 0.0; 0.0; 0.0|])
printfn "Test2: %b" ((arraySort [|1.5;8.5;3.5;5.5;4.5|]) = [|1.5; 3.5; 4.5; 5.5; 8.5|])
printfn "Test3: %b" ((arraySort [||]) = [||])
printfn "Test4: %b" ((arraySort [|"a"|]) = [|"a"|])
printfn "Test5: %b" ((arraySort [|5; 4; 3; 2; 1; 1; 2; 3; 4; 5|]) = [|1; 1; 2; 2; 3; 3; 4; 4; 5; 5|])
printfn "Test6: %b" ((arraySort [|'4'; '3'; '2'; '2'; '3'; '4'|]) = [|'2'; '2'; '3'; '3'; '4'; '4'|])
printfn "Test7: %b" ((arraySort [|1;7;5;2;1|]) = [|1;1;2;5;7|])