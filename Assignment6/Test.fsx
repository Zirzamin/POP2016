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
let rec colourAt (x,y) figure =
    match figure with
    | Circle ((50,50), r, col) ->
        if (x-50)*(x-50)+(y-50)*(y-50) <= r*r
        // bruger Pythagoras sætning til at finde afstand til centrum
        then Some col else None
    | Rectangle ((40,40), (90,110), col) ->
        if 40<=x && x <= 90 && 40 <= y && y <= 110 // indenfor hjørnerne
        then Some col else None
    | Mix (f1, f2) ->
        match (colourAt (x,y) f1, colourAt (x,y) f2) with
        | (None, c) -> c // overlapper ikke
        | (c, None) -> c // ditto
        | (Some (r1,g1,b1), Some (r2,g2,b2)) ->
            Some ((r1+r2)/2, (g1+g2)/2, (b1+b2)/2)// gennemsnitsfarve

let o61 : figure = Mix (Circle((50,50),45,(255,0,0)), Rectangle((40,40),(90,110),(0,0,256)));;
let drawCircle (x,y) = 
    match colourAt (x,y) o61 with
    | Some(x) -> x
    | None -> (128,128,128);;
makeBMP.makeBMP "test" 256 256 drawCircle;;