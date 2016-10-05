//(c)

//Recursiv
//First initiate r. Then go backwards in the string. So we first find 100 and go 10 backwards to insert a newline for the line above. 
    
let mutable r = ""
let rec recMulTable (n:int) : string =
    r <- sprintf "\n" + r
    [n*10..-1*n..n]|> List.iter (fun t -> r <- (sprintf "\t%*d" 3 t) + r)
    r <- sprintf "\t%*d" 3 n + r
    if n > 1 then recMulTable (n-1)
    else 
        r <- sprintf "\n" + r
        [10..-1..1]|> List.iter (fun t -> r <- (sprintf "\t%*d" 3 t) + r)
        r <- " \t\t" + r.[1..]
        r;;


printfn "%s" (mulTable1 10);;