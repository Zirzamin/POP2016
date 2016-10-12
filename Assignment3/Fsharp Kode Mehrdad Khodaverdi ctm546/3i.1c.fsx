(*3i.1c*)

///<summary>
///recMulTable creates a multiplication table from 1 to n and prints it out,
///using recursion. 
///</summary>
///<params name="n">
///int number of n to print out.
///</params>
///<returns>
/// void
///</returns>
///<remarks>
///First initiate r. Then go backwards in the string. So we first find 100 
///and go 10 backwards to insert a newline for the line above. 
///</remarks>
    
let recMulTable (n:int) : string =
    let mutable r = ""
    let rec recMul (n:int) : string =
        r <- sprintf "\n" + r
        [n*10..-1*n..n]|> List.iter (fun t -> r <- (sprintf "\t%*d" 3 t) + r)
        r <- sprintf "\t%*d" 3 n + r
        if n > 1 then recMul (n-1)
        else 
            r <- sprintf "\n" + r
            [10..-1..1]|> List.iter (fun t -> r <- (sprintf "\t%*d" 3 t) + r)
            r <- " \t\t" + r.[1..]
            r
    recMul n


printfn "%s" (recMulTable 10);;