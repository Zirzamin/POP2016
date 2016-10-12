(*3i.0b*)

///<summary>
///Counting Recursivly. and if n is negative it counts downwards 
///</summary>
///<params name="n">
///int 
///</params>
///<returns>
///int number of i in l
///</returns>

let rec recSum (n:int) : int =
    if n = 1 then 1
    elif n = 0 then 0
    elif n < 0 then n+recSum(n+1)
    elif n = -1 then -1
    else n+recSum(n-1)

printfn "    HR 3i.0 (b)     recSum int"
printfn "Test1: %b" (recSum -10 = -55)
printfn "Test2: %b" (recSum 10 = 55)
printfn "Test3: %b" (recSum 0 = 0)
printfn "Test4: %b" (recSum 1 = 1)
printfn "Test5: %b" (recSum -1 = -1)