(*i5.2*)

///<summary>
///Returns the mean of a list of floats if there is a mean,
///otherwise returns None.
///</summary>
///<params name="l">
///float list
///</params>
///<returns>
///float option
///</returns>
///<remark>
///Take the sumof the last element in 'l' with 0.0 and feed the result as
///argument to the second last element, and so on.
///</remark>

let mean l =
    if l = [] then None
    else Some(List.foldBack (fun x xs -> x + xs) l 0.0 / float l.Length)

printfn "    5i.2     mean (float list)"
printfn "Test1: %b" ((mean [0.0; 0.0; 0.0; 0.0]) = Some 0.0)
printfn "Test2: %b" ((mean [1.5; 2.5; 3.5; 4.5; 5.5]) = Some 3.5)
printfn "Test3: %b" ((mean []) = None)