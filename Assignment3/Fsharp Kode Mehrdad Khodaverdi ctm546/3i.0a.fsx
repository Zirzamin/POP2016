(*3i.0a *)

///<summary>
///As long as counter c i not equal n then keep on counting up if n is positive
///If n is negative count downwards.
///</summary>
///<params name="n">
///int
///</params>
///<returns>
///int
///</returns>

let sum (n:int) : int =
    let mutable r = 0
    let mutable c = 0
    while (c<>n) do
        r <- r+(n-c)
        if n <0 then c <- c-1
        else c <- c+1
    r

printfn "    HR 3i.0 (a)     sum int"
printfn "Test1: %b" (sum -10 = -55)
printfn "Test2: %b" (sum 10 = 55)
printfn "Test3: %b" (sum 0 = 0)
printfn "Test4: %b" (sum 1 = 1)
printfn "Test5: %b" (sum -1 = -1)
