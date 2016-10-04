let rec minus (l1 : int list, l2 : int list) : int list =
  match l1 with
  |[]-> []
  |h::t ->
    match l2 with
      |[] -> l1
        |h2::t2 ->
          if h=h2 then minus(t, t2)
          elif h<h2 then h::minus(t, h2::t2)
          else minus(h::t, t2)


let a = [1;1;1;2;2]
let b = [1;1;2;3]
printf "%A" (minus (a,b))
printf "%A" (minus (b,a))
