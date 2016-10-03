module Assignment4
/// 4.11
// Del 1

let rec count (l:int list, i:int) = 
    match l with
    | [] -> 0
    | h :: t -> 
        if h = i then
            1 + count(t,i)
        else count(t,i)


// Del 2

let rec insert (l:int list, i:int) : int list = 
    match l with
    | [] -> [i]
    | h :: t when h < i -> h :: insert(t,i) 
    | h :: t when h > i -> i :: h :: t
    | h :: t -> h :: i :: t


// Del 3

let rec intersect (l1 : int list, l2 : int list) : int list =
    match l1 with
    |[] -> []
    |h::t -> 
        match l2 with
            |[] -> []
            |h2::t2 ->
                if h=h2 then h::intersect(t,t2)
                elif h<h2 then intersect(t, h2::t2)
                else intersect(l2,t2)
             
    


intersect([1;1;1;2;2],[1;1;2;3;3;3;3;4]) //= [1;1;2]

let ib = [1;2;2;3;4;5;6;7;7;7;7;7;9;9;9;10;11]
insert(ib,0)
insert(ib,1)
insert(ib,6)
insert(ib,8)
insert(ib,11)
insert(ib,12)
insert(ib,20)
