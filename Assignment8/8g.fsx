type codeColor = Red | Green | Yellow | Purple | White | Black
type code = codeColor list
type answer = int * int
type board = (code * answer) list
type player = Human | Computer

///player 1
///<summary>
///Afgør om mastercode skal laves af en spiller eller tilfældigt
///ved hjælp af et input fra konsollen
///</summary>
///<params name="input">
///string
///</params>
///<returns>
///Player type for player1
///</returns>
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
  |_ ->          //Hvis man taster forkert er det automatisk computeren
    printfn "Could not recognize answer player1 is computer controlled"
    Computer

///Player2
///<summary>
///Afgør om gættene skal laves af en spiller eller tilfældigt
///ved hjælp af et input fra konsollen
///</summary>
///<params name="input">
///string
///</params>
///<returns>
///Player type for player2
///</returns>

let player2 =
  printfn "Choose Player 2 (Player 2 tries to guess the mastercode)"
  printfn "Human or computer?"
  let input = System.Console.ReadLine()
  match input with
  |"human"|"Human" ->
    Human
  |"computer"|"Computer" ->
    Computer
  |_ ->         //Hvis man taster forkert er det automatisk computeren
    printfn "Could not recognize answer player2 is computer controlled"
    Computer

///Board
///<summary>
///Opstiller en tom oversigt over gæt
///</summary>
///<returns>
///Liste af typen board
///</returns>
let mutable board : board = []

///makeCode
///<summary>
///Sammensætter en liste på 4 codeColors ud fra spillertypen
///enten ved hjælp af input fra konsollen eller tilfældigt
///</summary>
///<params name="player">
///type player
///</params>
///<returns>
///codeColor list
///</returns>

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
      |"Quit"|"quit" ->     //Hvis man vil afslutte spillet
          exit(44)
      |_ ->                 //Hvis man taster forkert
          code <-  code
          printfn "Thats not right"
    List.rev code      //Returnerer koden i reverse da den bliver bygget omvendt
  |Computer ->
    let r = System.Random()   //genererer et tal
    for i = 1 to 4 do
      match r.Next(1,6) with  //Oversætter tal til farve
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

///makemastercode
///<summary>
///Sammensætter en liste på 4 codeColors ud fra spillertypen og makeCode
///enten ved hjælp af input fra konsollen eller tilfældigt
///Dette bliver til koden som skal gættes
///</summary>
///<params name="player1">
///type player
///</params>
///<returns>
///codeColor list
///</returns>

let makemastercode =
  makeCode player1

///mastercode
///<summary>
///Definerer koden som skal gættes
///</summary>
///<returns>
///codeColor list
///</returns>
let mastercode = makemastercode

///histogram
///<summary>
///Sammensætter en liste med seks elementer som beskriver hhv. antallet
///af farverne [rød; grøn; gul; lilla; hvid; sort] i listen som er input
///</summary>
///<params name="code">
///codeColor list
///</params>
///<returns>
///int list
///</returns>

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

///colortonum
///<summary>
///Tager en farve og nummererer den efter sin placering i histogrammet
///</summary>
///<params name="color">
///codeColor
///</params>
///<returns>
///int
///</returns>

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

///chooselesser
///<summary>
///Tager to heltal og returnerer det mindste
///Eller tal nummer to hvis de er ens
///</summary>
///<params name="a">
///int
///</params>
///<params name="b">
///int
///</params>
///<returns>
///int
///</returns>

let chooselesser a b =
  if a < b then
    a
  else
    b

///Validate
///<summary>
///Tager en kode og returnerer et svar baseret på masterkoden
///Hvor mange farver du har placeret rigtigt og hvor mange som indeholdes
///i koden men er placeret forkert
///</summary>
///<params name="code">
///codeColor list
///</params>
///<params name="mastercode">
///codeColor list
///</params>
///<returns>
///int*int (answer)
///</returns>

let validate (code : code) =
  let mutable whitepin = 0
  let mutable blackpin = 0
  if mastercode = code then //Hvis de to koder er ens, returner alle rigtige
    blackpin <- 4
    (whitepin, blackpin)
  else
    for i = 0 to 3 do
      let mutable pincounter = 1
      if code.[i] = mastercode.[i] then //Hvis en farve matches i masterkoden på samme plads giver dette en sort
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

(*Hvis en farve matcher farven på en anden placering som ikke selv giver en sort
 pin, og antallet af hvide givet for denne farve ikke overstiger den mindste af
 hhv. antallet af farven i masterkoden eller koden, og der indtil nu har været flere
 af farven i masterkoden end i gættet så giver dette en hvid pin *)

            elif code.[j] <> mastercode.[j] && code.[i] = mastercode.[j] && pincounter = 1 && ((histogram mastercode).[(colortonum code.[i])]) >= ((histogram code.[i..]).[(colortonum code.[i])]) && ((histogram mastercode).[(colortonum code.[i])])  = 1 then
              whitepin <- whitepin + 1
              pincounter <- pincounter + 1

(*Ellers, hvis en farve matcher farven på en anden placering som ikke selv giver
 en sort pin, og der endnu ikke er givet pins for denne farve, og antallet af farven
 i masterkoden er større eller lig antallet af farven i resten af gættet, og antallet
af farven i masterkoden er lig 1 så giver dette en hvid pind *)

            elif code.[j] <> mastercode.[j] && code.[i] = mastercode.[j] && pincounter = 1 && ((histogram mastercode).[(colortonum code.[i])]) >= ((histogram code.[i..]).[(colortonum code.[i])]) && ((histogram code.[..i]).[(colortonum code.[i])]) <= ((histogram mastercode).[(colortonum code.[i])]) then
              whitepin <- whitepin + 1
              pincounter <- pincounter + 1
    (whitepin, blackpin)

(*Ellers, hvis en farve matcher farven på en anden placering som ikke selv giver
 en sort pin, og der endnu ikke er givet pins for denne farve, og antallet af
 farven i masterkoden er over eller lig antallet af farven i resten af gættet,
 og antallet af farven indtil nu i gættet er mindre eller lig antallet i masterkoden
 så giver dette en hvid pin*)

///guess
///<summary>
///Laver et gæt ud fra spillertypen og returnerer dette
///</summary>
///<params name="player">
///player type
///</params>
///<params name="board">
///board type
///</params>
///<returns>
///codeColor list
///</returns>

let guess player (board : board) =
  match player with
  |Human ->
    let hguess = makeCode Human
    hguess
  |Computer ->
    let cguess = makeCode Computer
    cguess

///addToBoard
///<summary>
///Tilføjer et gæt og dettes validering til boardet
///Hvis du bruger mere end det tilladte antal gæt lukkes programmet
///</summary>
///<params name="guess">
///codeColor list
///</params>
///<returns>
///board
///</returns>

let addToBoard guess =
  if (board.Length - 1) < 30 then
    board <- (guess, (validate guess)) :: board
  else
    printfn "\n You have no more guesses left"
    exit(44)

///Play
///<summary>
///Tilføjer gæt og viser boardet så længe programmet kører og man ikke har vundet
///Hvis man vinder fortæller den hvor mange gæt man har brugt
///</summary>
///<params name="won">
///Boolean
///</params>

let play =
  let mutable won = false
  while won = false do
    addToBoard (guess player2)
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
