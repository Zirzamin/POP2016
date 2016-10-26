//6g.1
type weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
//Typen weekday som indeholder ugedagene

let numberToDay number = //Funktion som tager et int som input
  match number with      //Matcher int med ugedagende fra et til syv
  |1 -> Some (Monday)
  |2 -> Some (Tuesday)
  |3 -> Some (Wednesday)
  |4 -> Some (Thursday)
  |5 -> Some (Friday)
  |6 -> Some (Saturday)
  |7 -> Some (Sunday)
  |_ -> None           //Returnerer None hvis inputtet ikke svarer til en ugedag

printf "%A" (numberToDay 3) //Test inden for inputtets krav
printf "%A" (numberToDay 8) //Test uden for inputtets krav

//6g.2 og frem
type point = int * int // a point (x, y) in the plane
type colour = int * int * int // (red, green, blue), 0..255 each
type figure =
    | Circle of point * int * colour
    // defined by center, radius, and colour
    | Rectangle of point * point * colour
    // defined by bottom-left corner, top-right corner, and colour
    | Mix of figure * figure
    // combine figures with mixed colour at overlap
    // finds colour of figure at point
    | Twice of figure * (int * int)
    // Takes the combined figures with mixed colours and copies them to a new location
    // finds colour the figures at the new point.
let rec move figure vector =
  let (|+|) (x0, y0) (x1, y1) = (x0 + x1, y0 + y1) // vector addition
  match figure with
    | Circle (point, r, col)      -> Circle (point |+| vector, r, col)
    | Rectangle (p0, p1, col) -> Rectangle (p0 |+| vector, p1 |+| vector, col)
    | Mix (f0, f1)      -> Mix (move f0 vector, move f1 vector)
    | Twice (b0, vector2)     -> Mix (b0, move b0 vector2)
let rec colourAt (x,y) figure =
    match figure with
    | Circle ((cx,cy), r, col) ->
        if (x-cx)*(x-cx)+(y-cy)*(y-cy) <= r*r
        // bruger Pythagoras sætning til at finde afstand til centrum
        then Some col else None
    | Rectangle ((x0,y0), (x1,y1), col) ->
        if x0<=x && x <= x1 && y0 <= y && y <= y1 // indenfor hjørnerne
        then Some col else None
    | Mix (f1, f2) ->
        match (colourAt (x,y) f1, colourAt (x,y) f2) with
        | (None, c) -> c // overlapper ikke
        | (c, None) -> c // ditto
        | (Some (r1,g1,b1), Some (r2,g2,b2)) ->
            Some ((r1+r2)/2, (g1+g2)/2, (b1+b2)/2)// gennemsnitsfarve
    | Twice (fig, (vx, vy)) ->
        match  (colourAt (x,y) figure, colourAt (x,y) (move fig (vx,vy))) with
        | (None, c) -> c
        | (c, None) -> c
        | (c, k) -> k

let g61 : figure = move (Twice ((Mix (Circle((50,50),45,(255,0,0)), Rectangle((40,40),(90,110),(0,0,256)))),(50,70))) (0,0)
let drawModel (x,y) =
    match colourAt (x,y) g61 with
    | Some(x) -> x
    | None -> (128,128,128)
makeBMP.makeBMP "g63.bmp" 150 200 drawModel
