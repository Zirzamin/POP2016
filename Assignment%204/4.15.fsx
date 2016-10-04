let a = [1;1;1;2;2]
let b = [1;1;2;3]
let rec rev (l : int list, ys : int list)=                //Det ligner sgu da en gyldig funktion
  match (l,ys) with
  |([], ys) -> ys
  |(h::t, ys) -> rev(t, h::ys)

let rec revrev [l1 : int list; l2 : int list] =
  match (l2, l1) with
  |(h::t, h2::t2) -> (rev (l2, []), (rev (l1, [])))          //Fejl her siger at det ikke er en gyldig funktion :(
  |(h::t, [])-> (rev (l2, []),[])
  |([], h2::t2)-> ([], (rev (l1, [])))

printf "%A" (revrev [a;b])
