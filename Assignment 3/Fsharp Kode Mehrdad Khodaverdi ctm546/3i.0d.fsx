//(d)

let compare (n:int) = 
    printfn "%s \t %s \t %s \t %s" "n" "sum" "recSum" "simpleSum"
    for i = -9 to n do printfn "%d \t %d \t %d \t %d" i (sum i) (recSum i) (simpleSum i);;

compare 10;;