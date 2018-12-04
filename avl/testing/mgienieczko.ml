(* Autor: Mateusz Gienieczko *)
open ISet;;

let info = false;;

let simple l =
  let (e, res) =
    List.fold_left (fun ((px, py), la) (x, y) ->
      if py + 1 >= x then ((px, max py y), la)
      else ((x, y), (px, py)::la)) ((List.hd l), []) (List.tl l)
  in
  List.rev (e::res);;

let long l =
  let rec add_inter acc (x, y) =
    if x == y then x::acc
    else add_inter (x::acc) (x + 1, y)
  in
  List.rev (List.fold_left (fun acc inter -> (add_inter [] inter) @ acc) [] l);;
  
let add_list =
  List.fold_left (fun s x -> add x s);;

let mem_all a l1 =
  List.filter (fun x -> not (mem x a)) l1 = []

let mem_none a l1 =
  List.filter (fun x -> mem x a) l1 = []

(* Small correctness tests *)

let l1 = [(-10, -8); (-7, -7); (-4, -1); (1, 1); (3, 7); (10, 15); (100, 1000)];;
let a = add_list empty l1;;

assert(elements a = simple l1);;
assert(mem_all a (long l1));;
assert(below 1000 a = 921);;

let (a1, b, a2) = split 4 a;;
assert(b);;
assert(simple (elements a1 @ [(4, 4)] @ elements a2) = simple l1);;
assert(List.filter (fun (x, y) -> y >= 4) (elements a1) = []);;
assert(List.filter (fun (x, y) -> x <= 4) (elements a2) = []);;

let (a1, b, a2) = split 3 a;;
assert(b);;
assert(simple (elements a1 @ [(3, 3)] @ elements a2) = simple l1);;
assert(List.filter (fun (x, y) -> y >= 3) (elements a1) = []);;
assert(List.filter (fun (x, y) -> x <= 3) (elements a2) = []);;

let (a1, b, a2) = split 2 a;;
assert(not b);;
assert(simple(elements a1 @ elements a2) = simple l1);;
assert(List.filter (fun (x, y) -> y >= 2) (elements a1) = []);;
assert(List.filter (fun (x, y) -> x <= 2) (elements a2) = []);;

let b = add (1, 10) a;;
let l2 = List.sort (fun (x1, _) (x2, _) -> compare x1 x2) ((1, 10)::l1);;

assert(elements b = simple l2);;

let c = remove (1, 10) a;;
let d = remove (1, 10) b;;

assert(elements c = elements d);;

let e = add (min_int, max_int) a;;
assert(elements e = [(min_int, max_int)]);;
assert(below 1 e = max_int);;

let f = remove (min_int, max_int) a;;
assert(elements f = []);;

let l3 = [(16, 99); (2, 2); (8, 9); (-6, -5)];;
let g = add_list a l3;;
assert(elements g = [(-10, -1); (1, 1000)]);;
assert(not (mem 0 g));;
let h = remove (420, 690) g;;
assert(not (mem 500 h));;
assert(elements h = [(-10, -1); (1, 419); (691, 1000)]);;
let i = add (0, 0) g;;
assert(elements i = [(-10, 1000)]);;
let j = remove (-9, -1) i;;
assert(elements j = [(-10, -10); (0, 1000)]);;
let k = remove (500, 999) j;;
assert(elements k = [(-10, -10); (0, 499); (1000, 1000)]);;

(* Performance tests *)

let rec aux l i =
  if i = 0 then l
  else aux (i::l) (i - 1);;

let l1 = snd (List.fold_left (fun (i, l) _ -> (i + 3, (i, i + 1)::l)) (min_int, [])
    (aux [] 100000));;

let l2 = snd (List.fold_left (fun (i, l) _ -> (i - 3, (i, i + 1)::l)) (max_int - 3, [])
	(aux [] 100000));;

let l3 = snd (List.fold_left (fun (i, l) _ -> (i + 3, (i, i + 1)::l)) (0, [])
	(aux [] 100000));;

let l4 = snd (List.fold_left (fun (i, l) _ -> (i - 3, (i, i + 1)::l)) (0, [])
	(aux [] 100000));;

if info then Pervasives.print_endline "Starting performence";;
let a = add_list empty l1;;
if info then Pervasives.print_endline "Added l1";;
let a = add_list a l1;;
if info then Pervasives.print_endline "Added l1";;
let a = add_list a l2;;
if info then Pervasives.print_endline "Added l2";;
let a = add_list a l2;;
if info then Pervasives.print_endline "Added l2";;
let a = add_list a l3;;
if info then Pervasives.print_endline "Added l3";;
let a = add_list a l3;;
if info then Pervasives.print_endline "Added l3";;
let a = add_list a l4;;
if info then Pervasives.print_endline "Added l4";;
let a = add_list a l4;;
if info then Pervasives.print_endline "Added l4";;

let test s (a, b) step i =
  let rec aux s (x, y) i =
    if i = 0 then s
    else aux (remove (x, y) s) (x + step, y + step) (i - 1)
  in
  aux s (a, b) i;;
    
test a (min_int + 1, min_int + 10000) 2 100000;;
if info then Pervasives.print_endline "Test 1";;
test a (max_int / 2, max_int / 2 + 10000) 2 100000;;
if info then Pervasives.print_endline "Test 2";;
test a (min_int + 10000, max_int / 2) 2 100000;;
if info then Pervasives.print_endline "Test 3";;
test a (max_int / 2, max_int - 1000000) 2 100000;;
if info then Pervasives.print_endline "Test 4";;
test a (max_int - 10000000, max_int - 1000000) 2 100000;;
if info then Pervasives.print_endline "Test 5";;

remove (min_int, max_int) a;;
if info then Pervasives.print_endline "Starting add";;
for i = 0 to 10000 do
  (fun _ -> ()) (add (min_int + i, max_int - i) a);
done;;

if info then Pervasives.print_endline "Starting remove";;
for i = 0 to 10000 do
  (fun _ -> ()) (remove (min_int + i, max_int - i) a)
done;;

if info then Pervasives.print_endline "Starting split";;
for i = 0 to 10000 do
  (fun _ -> ()) (split (min_int + i) a)
done;;

if info then Pervasives.print_endline "Starting below";;
for i = 0 to 10000 do
  (fun _ -> ()) (below (min_int + i) a)
done;;
