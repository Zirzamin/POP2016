(*3i.1a*)

///<summary>
///mulTable creates a multiplication table from 1 to n and prints it out
/// to console.
///</summary>
///<params name="n">
///int number of 1 to n multiplication table.
///</params>
///<returns>
/// void
///</returns>

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

printf "%s" (mulTable 10)


