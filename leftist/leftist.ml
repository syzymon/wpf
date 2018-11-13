(*Autor: Szymon Tworkowski 406386 Reviewer: Damian Werpachowski 407214*)
type 'a queue = 
	| Leaf (* pusty wierzcholek *)
	| Node of ('a) * int * ('a queue) * ('a queue)  
(*wartosc wierzcholka, dl prawej sciezki numerowana od 1, lewy syn, prawy syn*)

let empty = Leaf

let is_empty q = (q = Leaf)


let right_path q = match q with (*selektor zwracajacy dlugosc prawej sciezki*)
	| Leaf -> 0
	| Node(_, d, _, _) -> d


let normalize q = (*funkcja ustala glebokosc drzewa oraz rotacje poddrzew po polaczeniu*)
	match q with (value, depth, left, right) ->
		if (right_path left < right_path right) then (*mniejsza prawa sciezka musi byc po prawej stronie wynikowego drzewa*)
			Node(value, right_path right + 1, right, left)
		else
			Node(value, right_path left + 1, left, right) 

let rec join q1 q2 = match (q1, q2) with
	| (Leaf, _) -> q2
	| (_, Leaf) -> q1
	| (Node(val1, depth1, left1, right1), Node(val2, depth2, left2, right2)) -> 
			if val1 > val2 then join q2 q1 else (*chcemy miec mniejsza wartosc w pierwszym drzewie*)
				normalize (val1, 0, left1, join right1 q2)  
					
let add x q = join (Node(x, 1, Leaf, Leaf)) q


exception Empty (*rzucamy gdy klient probuje usunac z pustej kolejki*)

let delete_min q = match q with
	| Leaf -> raise Empty
	| Node(value, _, q1, q2) -> (value, join q1 q2)

(* 
ZBIOR TESTOW:

let b = join a b ;;
assert (is_empty b <> true);;

let (x,y) = delete_min b;;

assert (x = 1);;
assert (is_empty y = true);;
assert (try let _=delete_min y in false with Empty -> true);;



let a = empty;;
let b = add 1 empty;;

assert (is_empty a = true);;
assert (try let _=delete_min a in false with Empty -> true);;
assert (is_empty b <> true);;



let a0 = empty;; 

assert (is_empty a0);;

let a1 = empty;;

let a2 = add 997 a0;;

let a3 = add 94 a0;;

let (v, a4) = delete_min a2;;

assert (v == 997);;

let a5 = join a1 a3;;

assert (is_empty a4);;

let a6 = empty;;

let a7 = add 880 a3;;

assert (is_empty a0);;



let s = ( join ( Node (0, Node ( 0, Node ( 0, Node ( 0, Node (0, Leaf, 9, Leaf), 7, Leaf), 5, Leaf), 3, Leaf), 1, Leaf) ) 
               ( Node (0, Node ( 0, Node ( 0, Node ( 0, Node (0, Leaf, 10, Leaf), 8, Leaf), 6, Leaf), 4, Leaf), 2, Leaf) ) ) ;;

let (e, s) = delete_min s ;;
let (e, s) = delete_min s ;;
let (e, s) = delete_min s ;;
let (e, s) = delete_min s ;;
let (e, s) = delete_min s ;;
let (e, s) = delete_min s ;;
let (e, s) = delete_min s ;;
let (e, s) = delete_min s ;;
let (e, s) = delete_min s ;;
let (e, s) = delete_min s ;;
let (e, s) = delete_min s ;;
*)