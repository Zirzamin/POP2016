type Animal(weight:float, maxspeed:float) =
  let rnd = System.Random()
  let mutable crrntspeed = 0.0
  let mass = weight
  let mxspd = maxspeed
  let mutable foodneeds = weight * 0.5
  new (maxspeed) =
    let rnd2 = System.Random()
    let altweight = float (rnd2.Next(70, 301))
    Animal(altweight, maxspeed)
  abstract member foodneed: unit -> float
  default x.foodneed() = weight/2.0
  member x.setspeed foodintake = crrntspeed <- foodintake * maxspeed

type Carnivore(weight:float, maxspeed:float) =
  inherit Animal(weight, maxspeed)
  override x.foodneed () =
    x.foodneed <- (weight * 0.08)

type Herbivore(weight:float, maxspeed:float) =
  inherit Animal(weight, maxspeed)
  override x.foodneed () =
    x.foodneed <- (weight * 0.4)

let cheetah = Carnivore(50.0, 114.0)
let antelope = Herbivore(50.0, 95.0)
let wildebeast = Herbivore(200.0, 80.0)
let mutable foodintake = 0.0

for i = 1 to 3 do
  foodintake <- float (rnd.NextDouble())
  printfn "The Cheetah ate %A \% of %A kg needed" (foodintake * 100.0) (cheetah.x.foodneed)
