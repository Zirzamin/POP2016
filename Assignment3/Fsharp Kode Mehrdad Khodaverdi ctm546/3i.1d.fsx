//(d)

let compare n =
    printfn "\t| n | \t| loopMulTable \t| \t| recMulTable \t|" 
    for i = 0 to 51 do printf "%s" "_"
    for j = 1 to n do    
        printfn "\n\t| %d | \t| %b \t\t| \t| %b \t\t|" (j) (mulTable1 n = loopMulTable n) (mulTable1 n = recMulTable n);;
    
compare 10;;