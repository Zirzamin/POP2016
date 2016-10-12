(*3i.1d*)


let mulTable n =
  let mutable r = "    "
  [1..10]|> List.iter (fun t -> r <- r+sprintf "%4d" t)
  r <- r + "\n"
  for i = 1 to n do 
    r <- r + sprintf "%4d" i
    for j = 1 to 10 do
      r <- r + sprintf "%4d" (i*j)
    r <- r + sprintf "\n"
  r.[..(44+(45*n))]


let loopMulTable n =
    let mutable r = ""
    for i = 1 to n do
        r <- r+i.ToString()+"\t"
        for j = 1 to 10 do
            r <- r+sprintf "%*d " 3 (i*j)
        r <- r+"\n"
        if i = 1 then r <- r.[1..] + r
    r

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


///<summary>
///compares functions mulTable abd loopMulTable then mulTable and recMulTable,
///using a forloop. 
///</summary>
///<params name="n">
///int number of n to print out.
///</params>
///<returns>
/// void
///</returns>

let compare n =
    printfn "\t| n | \t| loopMulTable \t| \t| recMulTable \t|" 
    for i = 0 to 51 do printf "%s" "_"
    for j = 1 to n do    
        printfn "\n\t| %d | \t| %b \t\t| \t| %b \t\t|" (j) (mulTable n = loopMulTable n) (mulTable1 n = recMulTable n)

compare 10;;