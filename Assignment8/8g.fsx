type codeColor = Red | Green | Yellow | Purple | White | Black
type code = codeColor list
type answer = int * int
type board = (code * answer) list
type player = Human | Computer

(*let makemenu stringlist =
  let whileloop = true
  while whileloop = true do*)


let makeCode (player : player) =
  let mutable mastercode : code = []
  match player with
  |Human ->
    let mutable counter = 0
    printfn "Choose color: Red, Green, Yellow, Purple, White, Black:"
    while counter < 4 do
      let input = System.Console.ReadLine()
      match input with
      |"Red"|"red" ->
          mastercode <- Red :: mastercode
          counter <- counter+1
          printfn "You have chosen red, %A choices left" (4-counter)
      |"Green"|"green" ->
          mastercode <- Green :: mastercode
          counter <- counter+1
          printfn "You have chosen green, %A choices left" (4-counter)
      |"Yellow"|"yellow" ->
          mastercode <- Yellow :: mastercode
          counter <- counter+1
          printfn "You have chosen yellow, %A choices left" (4-counter)
      |"Purple"|"purple" ->
          mastercode <- Purple :: mastercode
          counter <- counter+1
          printfn "You have chosen purple, %A choices left" (4-counter)
      |"White"|"white" ->
          mastercode <- White :: mastercode
          counter <- counter+1
          printfn "You have chosen white, %A choices left" (4-counter)
      |"Black"|"black" ->
          mastercode <- Black :: mastercode
          counter <- counter+1
          printfn "You have chosen black, %A choices left" (4-counter)
      |_ ->
          mastercode <-  mastercode
          printfn "Thats not right"
    printfn "Your mastercode is %A" (List.rev mastercode)
    List.rev mastercode
    //Kode som lader spilleren skifte imellem de 6 farver
    //og overfører hvert af fire valg til en liste
  |Computer ->
    let r = System.Random()
    for i = 1 to 4 do
      match r.Next(1,6) with
      |1 ->
        mastercode <- Red :: mastercode
      |2 ->
        mastercode <- Green :: mastercode
      |3 ->
        mastercode <- Yellow :: mastercode
      |4 ->
        mastercode <- Purple :: mastercode
      |5 ->
        mastercode <- White :: mastercode
      |6 ->
        mastercode <- Black :: mastercode
      |_ ->
        mastercode
    printfn "Your mastercode is %A" (List.rev mastercode)
    List.rev mastercode
    //Laver et random kombination
makeCode Human
makeCode Computer
