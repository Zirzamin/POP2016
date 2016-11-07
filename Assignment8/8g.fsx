type codeColor = Red | Green | Yellow | Purple | White | Black
type code = codeColor list
type answer = int * int
type board = (code * answer) list
type player = Human | Computer

(*let makemenu stringlist =
  let whileloop = true
  while whileloop = true do*)
let player = Human
let mutable board : board = []
let makeCode (player : player) =
  let mutable code : code = []
  match player with
  |Human ->
    let mutable counter = 0
    printfn "Choose color: Red, Green, Yellow, Purple, White, Black:"
    while counter < 4 do
      let input = System.Console.ReadLine()
      match input with
      |"Red"|"red" ->
          code <- Red :: code
          counter <- counter+1
          printfn "You have chosen red, %A choices left" (4-counter)
      |"Green"|"green" ->
          code <- Green :: code
          counter <- counter+1
          printfn "You have chosen green, %A choices left" (4-counter)
      |"Yellow"|"yellow" ->
          code <- Yellow :: code
          counter <- counter+1
          printfn "You have chosen yellow, %A choices left" (4-counter)
      |"Purple"|"purple" ->
          code <- Purple :: code
          counter <- counter+1
          printfn "You have chosen purple, %A choices left" (4-counter)
      |"White"|"white" ->
          code <- White :: code
          counter <- counter+1
          printfn "You have chosen white, %A choices left" (4-counter)
      |"Black"|"black" ->
          code <- Black :: code
          counter <- counter+1
          printfn "You have chosen black, %A choices left" (4-counter)
      |_ ->
          code <-  code
          printfn "Thats not right"
    List.rev code
    //Kode som lader spilleren skifte imellem de 6 farver
    //og overfører hvert af fire valg til en liste
  |Computer ->
    let r = System.Random()
    for i = 1 to 4 do
      match r.Next(1,6) with
      |1 ->
        code <- Red :: code
      |2 ->
        code <- Green :: code
      |3 ->
        code <- Yellow :: code
      |4 ->
        code <- Purple :: code
      |5 ->
        code <- White :: code
      |6 ->
        code <- Black :: code
      |_ ->
        code <- code
    List.rev code
    //Laver et random kombination
let mastercode = makeCode player

let validate (code : code) =
  let mutable whitepin = 0
  let mutable blackpin = 0
  if mastercode = code then
    blackpin <- 4
    (whitepin, blackpin)
  else
    for i = 0 to 3 do
      if code.[i] = mastercode.[i] then
        blackpin <- blackpin + 1
      elif List.exists ((=)code.[i]) mastercode then
        whitepin <- whitepin + 1
    (whitepin, blackpin)

let guess player board =
  printfn "Current board:"
  printfn "%A" board
  match player with
  |Human ->
    let hguess = makeCode Human
    hguess
  |Computer ->
    let cguess = makeCode Computer
    cguess

let addToBoard guess =
  if board.Length - 1 < 12 then
    board <- (guess, (validate guess)) :: board
  else
    printfn "You have no more guesses left"
    board <- board

mastercode
addToBoard (guess Human board)
addToBoard (guess Computer board)
