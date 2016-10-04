let rec plus (l1 : int list, l2 : int list) : int list =
  match l1 with
  |[]-> l2
  |h::t ->
    match l2 with
      |[] -> l1
        |h2::t2 ->
          if h=h2 then h::plus(t, h2::t2)
          elif h<h2 then h::plus(t, h2::t2)
          else h2::plus(h::t, t2)

let a = [1;2;3;4;6;6;7]
let b = [2;3;4;5;5;5;6;7;8]
printf "%A" (plus (a,b))
