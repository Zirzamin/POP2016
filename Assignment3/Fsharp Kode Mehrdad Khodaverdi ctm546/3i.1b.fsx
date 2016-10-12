(*3i.1b*)

///<summary>
///loopMultable creates a multiplication table from 1 to n and prints it out,
/// using two for-loops
///</summary>
///<params name="n">
///int number of n to print out.
///</params>
///<returns>
/// void
///</returns>
///<remarks>
///Two for loops and using sprintf
///</remarks>

let loopMulTable n =
    let mutable r = ""
    for i = 1 to n do
        r <- r+i.ToString()+"\t"
        for j = 1 to 10 do
            r <- r+sprintf "%*d " 3 (i*j)
        r <- r+"\n"
        if i = 1 then r <- r.[1..] + r
    r

printfn "%s" (loopMulTable 10) //print 10
