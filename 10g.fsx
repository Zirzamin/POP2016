type Animal(weight:float, maxspeed:float) =
  let mutable crrntspeed = 0.0
  let mass = weight
  let mxspd = maxspeed
  let mutable foodneeds = weight * 0.5
  abstract member foodneed: unit -> float
  default x.foodneed() = weight/2.0
  member x.setspeed foodintake = crrntspeed <- foodintake * maxspeed
  member x.currentspeed = crrntspeed

type Carnivore(weight:float, maxspeed:float) =
  inherit Animal(weight, maxspeed)
  new (maxspeed) =
    let rnd2 = System.Random()
    let altweight = float (rnd2.Next(70, 301))
    Carnivore(altweight, maxspeed)
  override x.foodneed () =
    (weight * 0.08)

type Herbivore(weight:float, maxspeed:float) =
  inherit Animal(weight, maxspeed)
  new (maxspeed) =
    let rnd3 = System.Random()
    let altweight = float (rnd3.Next(70, 301))
    Herbivore(altweight, maxspeed)
  override x.foodneed () =
    (weight * 0.4)

let mutable cheetah = Carnivore(50.0, 114.0)
let mutable antelope = Herbivore(50.0, 95.0)
let mutable wildebeast = Herbivore(200.0, 80.0)
let mutable foodintake = 0.0
let rnd = System.Random()
let mutable cheetahtime = 0.0
let mutable antelopetime = 0.0
let mutable wildebeasttime = 0.0

printfn "\nFIRST RACE"
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
cheetahtime <- 0.0
antelopetime <- 0.0
wildebeasttime <- 0.0

cheetah <- Carnivore(114.0)
antelope <- Herbivore(95.0)
wildebeast <- Herbivore(80.0)

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
