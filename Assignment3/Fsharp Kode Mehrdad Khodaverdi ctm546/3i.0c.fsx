(*3i.9c*)

///<summary>
///Counts by a formula
///</summary>
///<params name="n">
///int
///</params>
///<returns>
///int
///</returns>

let simpleSum n:int = 
    if n >0 then (n*(n+1))/2 //if n is positiv else if negative
    else -(-n*(-n+1))/2

printfn "    HR 3i.0 (c)     simpleSum int"
printfn "Test1: %b" (simpleSum -10 = -55)
printfn "Test2: %b" (simpleSum 10 = 55)
printfn "Test3: %b" (simpleSum 0 = 0)
printfn "Test4: %b" (simpleSum 1 = 1)
printfn "Test5: %b" (simpleSum -1 = -1)