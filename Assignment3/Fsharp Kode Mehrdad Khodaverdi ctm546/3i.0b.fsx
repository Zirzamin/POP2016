//(b) Recursiv.
let rec recSum (n:int) : int =
    if n = 1 then 1
    elif n = 0 then 0
    elif n < 0 then n+recSum(n+1)
    elif n = -1 then -1
    else n+recSum(n-1);;

printfn "%d" (recSum -10);;