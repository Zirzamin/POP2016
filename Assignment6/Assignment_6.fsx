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

printfn "Test af numberToDay"
printfn "%A" (numberToDay 3) //Test inden for inputtets krav
printfn "%A" (numberToDay 8) //Test uden for inputtets krav

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
    | Twice (b0, vector2)     -> move (Mix (b0, move b0 vector2)) vector

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
        match  (colourAt (x,y) fig, colourAt (x,y) (move fig (vx,vy))) with
        | (None, c) -> c
        | (c, None) -> c
        | (c, k) -> k

let g61 : figure = Twice ((Mix (Circle((50,50),45,(255,0,0)), Rectangle((40,40),(90,110),(0,0,256)))),(50,70))

let drawModel (x,y) =
    match colourAt (x,y) g61 with
    | Some(x) -> x
    | None -> (128,128,128)
makeBMP.makeBMP "g63.bmp" 150 200 drawModel

let checkColour col =
  match col with
  |(r,g,b) ->
      if 0 <= r && r <= 256 && 0 <= g && g <= 256 && 0 <= b && b <= 256
        then true
      else false

let rec checkFigure figure =
  match figure with
  | Circle ((cx,cy), r, col) ->
      if r > 0 && checkColour col = true
      then true else false
  | Rectangle ((x0,y0), (x1,y1), col) ->
      if x0 < x1 && y0 < y1 && checkColour col = true
      then true else false
  | Mix (f1, f2) ->
      checkFigure f1 && checkFigure f2
  | Twice (fig, (vx, vy)) ->
      if checkFigure fig = true then true
      else false

let makecoord coord1 coord2 =
  match (coord1,coord2) with
  |(((minx1,miny1),(maxx1,maxy1)),((minx2,miny2),(maxx2,maxy2)))->
    let xmax =
      if (maxx1) < (maxx2) then
        maxx2
      else maxx1
    let xmin =
      if (minx1) > (minx2) then
        minx2
      else minx1
    let ymax =
      if (maxy1) < (maxy2) then
        maxy2
      else maxy1
    let ymin =
      if (miny1) > (minx2) then
        miny2
      else miny1
    ((xmin,ymin),(xmax,ymax))

let rec boundingBox figure =
  match figure with
  | Circle ((cx,cy), r, col) ->
    let min = ((cx-r),(cy-r))
    let max = ((cx+r),(cy+r))
    (min,max)
  | Rectangle ((x0,y0), (x1,y1), col) ->
    let min = (x0,y0)
    let max = (x1,y1)
    (min,max)
  |Mix (f1, f2) ->
    let mixcoords = (makecoord (boundingBox f1) (boundingBox f2))
    mixcoords
  | Twice (fig, (vx, vy)) ->
    match fig with
    |Mix (f1, f2) ->
        let coords = (makecoord (boundingBox f1) (boundingBox f2))
        match coords with
        |((xmin,ymin),(xmax,ymax)) ->
          if (0 + vx) > 0 && (0+vy) > 0 then
            ((xmin,ymin),((xmax+vx),(ymax+vy)))
          elif (0 + vx) < 0 && (0+vy) > 0 then
            (((xmin-vx),ymin),(xmax,(ymax+vy)))
          elif (0 + vx) > 0 && (0+vy) < 0 then
            ((xmin,(ymin-vy)),(xmax,(ymax+vy)))
          else (((xmin-vx),(ymin-vy)),(xmax,ymax))
    |_ ->
      match boundingBox fig with
      |((figminx, figminy),(figmaxx, figmaxy)) ->
        if ((figminx+vx),(figminy+vy)) < (figminx, figminy) then (((figminx+vx),(figminy+vy)),(figmaxx, figmaxy))
        else ((figminx, figminy),((figmaxx+vx), (figmaxy+vy)))

let g62 : figure = Rectangle((40,40),(90,110),(0,0,256))
let g63 : figure = Circle((50,50),45,(255,0,0))
let g64 : figure = Mix (Circle((50,50),45,(255,0,0)), Rectangle((40,40),(90,110),(0,0,256)))
let g65 : figure = Twice (Twice ((Mix (Circle((50,50),45,(255,0,0)), Rectangle((40,40),(90,110),(0,0,256)))),(50,70)),(10,10))
let g66 : figure = Twice (Circle((50,50),45,(255,0,0)), (10,10))
let g67 : figure = Twice (Circle((50,50),-45,(255,0,0)), (10,10))
let g68 : figure = Rectangle((90,110),(40,40),(0,0,256))

printfn "\n Test af Move"
printfn "g61: %A" ((boundingBox (move g61 (10,10)))=((15,15),(155,190)))
printfn "g62: %A" ((boundingBox (move g62 (10,10)))=((50,50),(100,120)))
printfn "g63: %A" ((boundingBox (move g63 (10,10)))=((15,15),(105,105)))
printfn "g64: %A" ((boundingBox (move g64 (10,10)))=((15,15),(105,120)))
printfn "g65: %A" ((boundingBox (move g65 (10,10)))=((15,15),(165,200)))
printfn "g66: %A" ((boundingBox (move g66 (10,10)))=((15,15),(115,115)))

printfn "\n Test af checkFigure"
printfn "g61: %A" (checkFigure g61)
printfn "g62: %A" (checkFigure g62)
printfn "g63: %A" (checkFigure g63)
printfn "g64: %A" (checkFigure g64)
printfn "g65: %A" (checkFigure g65)
printfn "g66: %A" (checkFigure g66)
printfn "g67: %A" (checkFigure g67)
printfn "g68: %A" (checkFigure g68)

printfn "\n Test af boundingBox"
printfn "g61: %A" (boundingBox g61)
printfn "g62: %A" (boundingBox g62)
printfn "g63: %A" (boundingBox g63)
printfn "g64: %A" (boundingBox g64)
printfn "g65: %A" (boundingBox g65)
printfn "g66: %A" (boundingBox g66)
