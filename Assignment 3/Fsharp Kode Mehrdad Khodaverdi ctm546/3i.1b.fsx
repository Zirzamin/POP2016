//(b)

//two for loops and using sprintf

let loopMulTable n =
    let mutable r = ""
    for i = 1 to n do
        r <- r+i.ToString()+"\t"
        for j = 1 to 10 do
            r <- r+sprintf "%*d " 3 (i*j)
        r <- r+"\n"
        if i = 1 then r <- r.[1..] + r
    r;;

//Try out all n from 1 to n .
let print n = for i=1 to n do printfn "%s" (loopMulTable i);;

printfn "%s" (loopMulTable 10);;
//print 10;;