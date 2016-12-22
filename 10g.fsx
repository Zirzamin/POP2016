///Animal
///<summary>
///Tager to argumenter, vægt og max hastighed i km/h
///Det er en class som indeholder tre methods og en variabel for nuværende hastighed
///og en for hvor meget mad dyret skal spise hver dag 
///</summary>
type Animal(weight:float, maxspeed:float) =
  let mutable crrntspeed = 0.0
  let mutable foodneeds = weight * 0.5
  abstract member foodneed: unit -> float
  default x.foodneed() = weight/2.0
  member x.setspeed foodintake = crrntspeed <- foodintake * maxspeed
  member x.currentspeed = crrntspeed

///Carnivore
///<summary>
///Tager to argumenter, vægt og max hastighed i km/h
///Det er en class som først fremmest arver alt fra Animal-klassen
///Dernæst kan den selv angive vægten, hvis brugeren udelukkende angiver 
///hastigheden, den angiver en vægt mellem 70 og 300 kilo.
///Derefter overrider og angiver den det daglige madforbrug med 8% af vægten
///</summary>
type Carnivore(weight:float, maxspeed:float) =
  inherit Animal(weight, maxspeed)
  new (maxspeed) =
    let rnd2 = System.Random()
    let altweight = float (rnd2.Next(70, 301))
    Carnivore(altweight, maxspeed)
  override x.foodneed () =
    (weight * 0.08)
///Herbivore
///<summary>
///Tager to argumenter, vægt og max hastighed i km/h
///Det er en class som først fremmest arver alt fra Animal-klassen
///Dernæst kan den selv angive vægten, hvis brugeren udelukkende angiver 
///hastigheden, den angiver en vægt mellem 70 og 300 kilo.
///Derefter overrider og angiver den det daglige madforbrug med 40% af vægten
///</summary>
type Herbivore(weight:float, maxspeed:float) =
  inherit Animal(weight, maxspeed)
  new (maxspeed) =
    let rnd3 = System.Random()
    let altweight = float (rnd3.Next(70, 301))
    Herbivore(altweight, maxspeed)
  override x.foodneed () =
    (weight * 0.4)

///Cheetah
///Angiver et mutable dyr af subclassen carnivore med vægt 50 kg og max hast. 114km/t
let mutable cheetah = Carnivore(50.0, 114.0)
///Antilope
///Angiver et mutable dyr af subclassen Herbivore med vægt 50kg og max hast. 95km/t
let mutable antelope = Herbivore(50.0, 95.0)
///Wildebeast
///Angiver et mutable dyr af subclassen Herbivore med vægt 200kg og max hast. 80km/t
let mutable wildebeast = Herbivore(200.0, 80.0)
///Foodintake
///Mutable som angiver hvor meget dyret har spist af dets daglige behov i en float (mellem 0.0 og 1.0)
let mutable foodintake = 0.0
///rnd 
///Hjælpefunktion til at udvælge et tilfældigt tal
let rnd = System.Random()
///Cheetahtime
///Angiver den sammenlagte tid geparden har brugt i 'ræset' indtil nu med en float
let mutable cheetahtime = 0.0
///Antelopetime
///Angiver den sammenlagte tid antelopen har brugt i 'ræset' indtil nu med en float
let mutable antelopetime = 0.0
///Antelopetime
///Angiver den sammenlagte tid gnuen har brugt i 'ræset' indtil nu med en float
let mutable wildebeasttime = 0.0

///First Race
///<summary>
///Et for-loop genererer tre kapløb for hvert dyr
///For hvert løb opdateres fødeindtaget (0.0-1.0/0-100%)
///Ud fra dette beregnes dyrets hastighed, tiden det tager at løbe 10 km
///og desuden hvor stor en procentdel af deres daglige behov de har spist.
///-Inklusiv deres daglige behov
///Når det hele er beregnet lægges tiden oven i dyrets samlede tid.
///Der beregnes et nyt foodintake og processen gentages for næste dyr
///Til slut sammenlignes tiderne og vinderen annonceres
///</summary>
printfn "\nFIRST RACE"
for i = 1 to 3 do
  printfn "%A" i
  //Sætter ny fødindtag
  foodintake <- rnd.NextDouble()
  //Printer hvor meget dyret har spist + hvor stor en procentdel af behovet dette dækker
  printfn "The Cheetah ate %.2f %% which is equal to %.2f kg of %.2f kg needed" (foodintake * 100.0) (foodintake * cheetah.foodneed ()) (cheetah.foodneed ())
  //Beregner og sætter dyrets hastighed
  cheetah.setspeed foodintake
  //Printer nuværende hastighed + hvor lang tid det tog at løbe 10 km
  printf "The Cheetahs current speed is %.2f km/h, and it took %.2f minutes to run 10 km" (cheetah.currentspeed) ((10.0/cheetah.currentspeed)*60.0)
  //Lægger tiden oveni den samlede tid for dyret
  cheetahtime <- (cheetahtime + ((10.0/cheetah.currentspeed)*60.0))
  foodintake <- rnd.NextDouble()
  printfn "The Antelope ate %.2f %% which is equal to %.2f kg of %.2f kg needed" (foodintake * 100.0) (foodintake * antelope.foodneed ()) (antelope.foodneed ())
  antelope.setspeed foodintake
  printf "The Antelope current speed is %.2f km/h, and it took %.2f minutes to run 10 km" (antelope.currentspeed) ((10.0/antelope.currentspeed)*60.0)
  antelopetime <- (antelopetime + ((10.0/antelope.currentspeed)*60.0))
  foodintake <- rnd.NextDouble()
  printfn "The Wildebeast ate %.2f %% which is equal to %.2f kg of %.2f kg needed" (foodintake * 100.0) (foodintake * wildebeast.foodneed ()) (wildebeast.foodneed ())
  wildebeasttime <- (wildebeasttime + ((10.0/wildebeast.currentspeed)*60.0))
  wildebeast.setspeed foodintake
  printf "The Wildebeast current speed is %.2f km/h, and it took %.2f minutes to run 10 km" (wildebeast.currentspeed) ((10.0/wildebeast.currentspeed)*60.0)
  printf "\n"
//Afgører hvilket af dyrene som vinder, og printer dette
if cheetahtime < antelopetime && cheetahtime < wildebeasttime then
  printfn "Cheetah won"
elif antelopetime < cheetahtime && antelopetime < wildebeasttime then
  printfn "Antelope won"
else printfn "Wildebeast won"
//Genstarter tiderne
cheetahtime <- 0.0
antelopetime <- 0.0
wildebeasttime <- 0.0

///Random vægt kapløb
///Cheetah fastholder sin max hast. på 114 km/t men får en tilfældig vægt (70.0-300.0kg)
cheetah <- Carnivore(114.0)
///Antelope fastholder sin max hast. på 95 km/t men får en tilfældig vægt (70.0-300.0kg)
antelope <- Herbivore(95.0)
///Wildebeast fastholder sin max hast. på 80 km/t men får en tilfældig vægt (70.0-300.0kg)
wildebeast <- Herbivore(80.0)

///Random Race
///<summary>
///Et for-loop genererer tre kapløb for hvert dyr
///For hvert løb opdateres fødeindtaget (0.0-1.0/0-100%)
///Ud fra dette beregnes dyrets hastighed, tiden det tager at løbe 10 km
///og desuden hvor stor en procentdel af deres daglige behov de har spist.
///-Inklusiv deres daglige behov
///Når det hele er beregnet lægges tiden oven i dyrets samlede tid.
///Der beregnes et nyt foodintake og processen gentages for næste dyr
///Til slut sammenlignes tiderne og vinderen annonceres
///</summary>
printfn "\n\nNow a new race with random weight"
for i = 1 to 3 do
  printfn "%A" i
  foodintake <- rnd.NextDouble()
  printfn "The Cheetah ate %.2f %% which is equal to %.2f kg of %.2f kg needed" (foodintake * 100.0) (foodintake * cheetah.foodneed ()) (cheetah.foodneed ())
  cheetah.setspeed foodintake
  printf "The Cheetahs current speed is %.2f km/h, and it took %.2f minutes to run 10 km" (cheetah.currentspeed) ((10.0/cheetah.currentspeed)*60.0)
  cheetahtime <- (cheetahtime + ((10.0/cheetah.currentspeed)*60.0))
  foodintake <- rnd.NextDouble()
  printfn "The Antelope ate %.2f %% which is equal to %.2f kg of %.2f kg needed" (foodintake * 100.0) (foodintake * antelope.foodneed ()) (antelope.foodneed ())
  antelope.setspeed foodintake
  printf "The Antelope current speed is %.2f km/h, and it took %.2f minutes to run 10 km" (antelope.currentspeed) ((10.0/antelope.currentspeed)*60.0)
  antelopetime <- (antelopetime + ((10.0/antelope.currentspeed)*60.0))
  foodintake <- rnd.NextDouble()
  printfn "The Wildebeast ate %.2f %% which is equal to %.2f kg of %.2f kg needed" (foodintake * 100.0) (foodintake * wildebeast.foodneed ()) (wildebeast.foodneed ())
  wildebeasttime <- (wildebeasttime + ((10.0/wildebeast.currentspeed)*60.0))
  wildebeast.setspeed foodintake
  printf "The Wildebeast current speed is %.2f km/h, and it took %.2f minutes to run 10 km" (wildebeast.currentspeed) ((10.0/wildebeast.currentspeed)*60.0)
  printf "\n"
if cheetahtime < antelopetime && cheetahtime < wildebeasttime then
  printfn "Cheetah won"
elif antelopetime < cheetahtime && antelopetime < wildebeasttime then
  printfn "Antelope won"
else printfn "Wildebeast won"
