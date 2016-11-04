type codeColor = Red | Green | Yellow | Purple | White | Black
type code = codeColor list
type answer = int * int
type board = (code * answer) list
type player = Human | Computer

(*let makemenu stringlist =
  let whileloop = true
  while whileloop = true do*)


let makeCode player =
  let mastercode = []
  match player with
  |Human ->
    let mutable counter = 0
    printf "Choose your color, Red, Green, Yellow, Purple, White, Black"
    while counter <= 4 do
      let mutable colorinput = System.Console.Readline()
      match colorinput with
      |"Red"|"red" ->
        Red :: mastercode
        counter <- counter+1
        printfn "Du har har valgt rød, og har %A valg tilbage" (4-counter)
      |"Green"|"green" ->
        Green :: mastercode
        counter <- counter+1
        printfn "Du har har valgt rød, og har %A valg tilbage" (4-counter)
      |"Yellow"|"yellow" ->
        Yellow :: mastercode
        counter <- counter+1
        printfn "Du har har valgt rød, og har %A valg tilbage" (4-counter)
      |"Purple"|"purple" ->
        Purple :: mastercode
        counter <- counter+1
        printfn "Du har har valgt rød, og har %A valg tilbage" (4-counter)
      |"White"|"white" ->
        White :: mastercode
        counter <- counter+1
        printfn "Du har har valgt rød, og har %A valg tilbage" (4-counter)
      |"Black"|"black" ->
        Black :: mastercode
        counter <- counter+1
        printfn "Du har har valgt rød, og har %A valg tilbage" (4-counter)
    //Kode som lader spilleren skifte imellem de 6 farver
    //og overfører hvert af fire valg til en liste
  |Computer ->
    //Laver et random kombination
