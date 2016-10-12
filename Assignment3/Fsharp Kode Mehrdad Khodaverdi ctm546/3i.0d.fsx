(*3i.9d*)

let sum (n:int) : int =
    let mutable r = 0
    let mutable c = 0
    while (c<>n) do
        r <- r+(n-c)
        if n <0 then c <- c-1
        else c <- c+1
    r

let rec recSum (n:int) : int =
    if n = 1 then 1
    elif n = 0 then 0
    elif n < 0 then n+recSum(n+1)
    elif n = -1 then -1
    else n+recSum(n-1)

let simpleSum n:int = 
    if n >0 then (n*(n+1))/2 //if n is positiv else if negative
    else -(-n*(-n+1))/2


///<summary>
///Compares the 3 sum functions by printing it out to console.
///</summary>
///<params name="n">
///int number of n to print out.
///</params>
///<returns>
/// void
///</returns>

let compare (n:int) = 
    printfn "%s \t %s \t %s \t %s" "n" "sum" "recSum" "simpleSum"
    for i = -9 to n do printfn "%d \t %d \t %d \t %d" i (sum i) (recSum i) (simpleSum i)

//Prints out for n = 10
compare 10;;