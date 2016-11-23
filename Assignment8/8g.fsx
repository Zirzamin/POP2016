type codeColor = Red | Green | Yellow | Purple | White | Black
type code = codeColor list
type answer = int * int
type board = (code * answer) list
type player = Human | Computer

let player1 =
  System.Console.Clear()
  printfn "Choose Player 1 (Player 1 designs the mastercode)"
  printfn "Human or computer?"
  let input = System.Console.ReadLine()
  match input with
  |"human"|"Human" ->
    Human
  |"computer"|"Computer" ->
    Computer
  |_ ->
    printfn "Could not recognize answer player1 is computer controlled"
    Computer

let player2 =
  printfn "Choose Player 2 (Player 2 tries to guess the mastercode)"
  printfn "Human or computer?"
  let input = System.Console.ReadLine()
  match input with
  |"human"|"Human" ->
    Human
  |"computer"|"Computer" ->
    Computer
  |_ ->
    printfn "Could not recognize answer player2 is computer controlled"
    Computer

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
      |"Quit"|"quit" ->
          exit(44)
      |_ ->
          code <-  code
          printfn "Thats not right"
    List.rev code
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
let makemastercode =
  makeCode player1
let mastercode = makemastercode

let histogram (code : code) =
  let mutable countred = 0
  let mutable countgreen = 0
  let mutable countyellow = 0
  let mutable countpurple = 0
  let mutable countwhite = 0
  let mutable countblack = 0
  for elm in code do
    match elm with
    |Red ->
      countred <- countred + 1
    |Green ->
      countgreen <- countgreen + 1
    |Yellow ->
      countyellow <- countyellow + 1
    |Purple ->
      countpurple <- countpurple + 1
    |White ->
      countwhite <- countwhite + 1
    |Black ->
      countblack <- countblack + 1
  [countred; countgreen; countyellow; countpurple; countwhite; countblack]

let colortonum  (color : codeColor)=
  match color with
  |Red ->
    0
  |Green ->
    1
  |Yellow ->
    2
  |Purple ->
    3
  |White ->
    4
  |Black ->
    5

let chooselesser a b =
  if a < b then
    a
  else
    b

let rec validate (code : code) =
  let mutable whitepin = 0
  let mutable blackpin = 0
  if mastercode = code then
    blackpin <- 4
    (whitepin, blackpin)
  else
    for i = 0 to 3 do
      let mutable pincounter = 1
      if code.[i] = mastercode.[i] then
        if whitepin + blackpin > 4 then
          whitepin <- whitepin - 1
          blackpin <- blackpin + 1
        else
          blackpin <- blackpin + 1
      else
          for j = 0 to code.Length - 1 do
            if code.[j] <> mastercode.[j] && code.[i] = mastercode.[j] && pincounter < chooselesser ((histogram mastercode).[(colortonum code.[i])]) ((histogram code).[(colortonum code.[i])]) && ((histogram mastercode).[(colortonum code.[i])]) > ((histogram code.[..i]).[(colortonum code.[i])]) then
              whitepin <- whitepin + 1
              pincounter <- pincounter + 1
            elif code.[j] <> mastercode.[j] && code.[i] = mastercode.[j] && pincounter = 1 && ((histogram mastercode).[(colortonum code.[i])]) >= ((histogram code.[i..]).[(colortonum code.[i])]) && ((histogram mastercode).[(colortonum code.[i])])  <= chooselesser pincounter ((histogram code).[(colortonum code.[i])]) then
              whitepin <- whitepin + 1
              pincounter <- pincounter + 1
            elif code.[j] <> mastercode.[j] && code.[i] = mastercode.[j] && pincounter = 1 && ((histogram mastercode).[(colortonum code.[i])]) >= ((histogram code.[i..]).[(colortonum code.[i])]) then
              whitepin <- whitepin + 1
              pincounter <- pincounter + 1
    (whitepin, blackpin)

let guess player (board : board) =
  match player with
  |Human ->
    let hguess = makeCode Human
    hguess
  |Computer ->
    let cguess = makeCode Computer
    cguess

let addToBoard guess =
  if (board.Length - 1) < 30 then
    board <- (guess, (validate guess)) :: board
  else
    printfn "\n You have no more guesses left"
    exit(44)

let play =
  let mutable won = false
  while won = false do
    addToBoard (guess player2 board)
    match board.Head with
    |(_, (0, 4)) ->
      won <- true
    |(_,_) ->
      won <- false
    System.Console.Clear()
    printfn "DEBUG FEATURE %A" mastercode
    printfn "Current board:"
    for j = board.Length - 1 downto 0 do
      printfn "%A" board.[j]
  System.Console.Clear()
  printfn "You have won the game in %A guesses" (board.Length)

play
