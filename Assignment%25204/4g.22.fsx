let rec removeDuplicates l =
  match l with
  |[]->[]
  |[n]->[n]
  |(x::x1) ->
    let y = (removeDuplicates x1)
    if x=y then l
    else do (removeDuplicates x1)
  l
printf "%A" (removeDuplicates [1;2;1;3;1;1;4;5;2;1])
