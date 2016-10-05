//As long as counter c i not equal n then keep on counting up if n is positive
// If n is negative count downwards.
let sum (n:int) : int =
    let mutable r = 0
    let mutable c = 0
    while (c<>n) do
        r <- r+(n-c)
        if n <0 then c <- c-1
        else c <- c+1
    r;;

printfn "%A" (sum 10);;		//Print summen af n = 10
