//3i.1

//(a)

// The questions dont give any sense to me! So i have made more versions of these functions!


let mulTable1 n =
    let mutable r = ""
    for i=1 to n do
        
        //Add the row number and add a tab
        r <- r+i.ToString()+"\t"

        //create a list of 10 numbers starting from i with step i to the number i*10
        //iterate through the list and for each element with 3 spaceings add it to r

        [i..i..i*10]|> List.iter (fun t -> r <- r+sprintf "%*d\t" 3 t)
        r <- r+"\n"

        //if first line then remove the first char and duplicate the line
        if i = 1 then r <- r.[1..] + r
    r;;

printfn "%s" (mulTable1 10);;


//the same as above just without sprintf but with Tostring. OBS! here the numbers get seperated with a tab
let mulTable2 n =
    let mutable r = ""
    for i=1 to n do
        r <- r+i.ToString()+"\t"
        [i..i..i*10]|> List.iter (fun t -> r <- r+t.ToString()+"\t")
        r <- r+"\n"
        if i = 1 then r <- r.[1..] + r
    r;;

printfn "%s" (mulTable2 10);;


// here using two forloops and not using sprintf 
let mulTable3 n =
    let mutable r = ""
    for i = 1 to n do
        r <- r+i.ToString()+"\t"
        for j = 1 to 10 do
            r <- r+(i*j).ToString()+"\t"
        r <- r+"\n"
        if i = 1 then r <- r.[1..] + r
    r;;

printfn "%s" (mulTable3 10);;

//Try out all the n's one wants from 1 to n .

let print n = for i=1 to n do printfn "%s" (mulTable1 i);;

//print 10;;

// Printing only 1 line each time.
printfn "%s" (mulTable1 5).[..19]        // First 10 numbers in the string
printfn "%s" (mulTable1 4).[21..46]      // next 10 numbers in string. OBS! "\n" are included
printfn "%s" (mulTable1 3).[47..74]      // next 10 numbers in string. OBS! "\n" are included