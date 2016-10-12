(* Opgave i5
 *
 * Group: 7
 *   Mehrdad Khodaverdi ctm546@alumni.ku.dk
 *  
 *)

(*i5.1.*)

///<summary>
///Concatenates a list of lists to a list.
///</summary>
///<params name="a">
///a list list
///</params>
///<returns>
///a list
///</returns>
///<remarks>
///For each element in 'a' concatenate with the result from last iteration.
///Start with concatenate [] with 'a' then feed the result to next iteration.
///</remarks>

let concat a =
    List.fold (fun x xs -> x@xs) [] a

printfn "    5i.1     concat (a list list)"
printfn "Test1: %b" ((concat [[0];[0;0];[0]]) = [0; 0; 0; 0])
printfn "Test2: %b" ((concat [[1]]) = [1])
printfn "Test3: %b" ((concat [[]]) = [])
printfn "Test4: %b" ((concat [["a"];["b"; "c"];["a"]]) = ["a"; "b"; "c"; "a"])
printfn "Test5: %b" ((concat [[true];[true; false];[true]]) = [true; true; false; true])
printfn "Test6: %b" ((concat [[2];[6;4];[1]]) = [2; 6; 4; 1])
printfn "Test7: %b" ((concat [['2'];['a';'c'];['1']]) = ['2'; 'a'; 'c'; '1'])