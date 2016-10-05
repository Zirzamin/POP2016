//(c)

//if n is positiv else if negative

let simpleSum n:int = 
    if n >0 then (n*(n+1))/2
    else -(-n*(-n+1))/2;;

printfn "%d" (simpleSum -10);; //The answer is -55