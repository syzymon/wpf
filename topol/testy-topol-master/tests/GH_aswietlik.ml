(* Autor: Agnieszka Świetlik
 * Licence: Unlicensed
 * Original repo: https://github.com/aswietlik/wpf *)

let zle = ref 0
let test n b =
  if not b then begin
    Printf.printf "Zly wynik testu %d!!\n" n;
    incr zle
  end

open Topol;;

let isOk input output =
	let rec where a = function
		| [] -> []
		| h::t -> if h = a then h::t else (where a t) in 
	let rec length used = function
		| [] -> List.length used
		| (h, l1)::t -> let newOne used a = 
					if (where a used) = [] then a::used else used
				in length (newOne (List.fold_left newOne used l1) h) t in
	let size = length [] input in
	let rec find acc wh = function
		| [] -> acc
		| h::t -> acc && ((where h wh) <> []) && (find acc wh t) in
	let rec pom acc = function
		| [] -> acc
		| (a, l)::t -> acc && find acc (where a output) l && pom acc t
	in (size = List.length output) && (pom true input);;

let genTest n =
	let res = ref [] in
	for i = 1 to (n-1) do
		if Random.int 10 < 6 then
		let l = ref [] and m = Random.int (n - i) in
		let used = Array.make (n+1) false in
		let unUsed _ =
			let getInt n = min n ((Random.int (max (n - i - 1) 1)) + i + 1) in
			let a = ref (getInt n) in
			while used.(!a) = true do
				if (!a) = n then a := (i + 1) else a := (!a) + 1;
			done; used.(!a) <- true; !a in
		for j = 0 to m do
			l := (unUsed j)::(!l)
		done;
		res := (i, !l)::(!res)
	done; (!res);;


(* Printf.printf "=== Podstawowe...\n";; *)

let a = [];;
let b = [(1, [2;3]);(3, [6;7]);(6, [4;7]);(7, [5]);(2, [5]);(8, [])];;
let c = [(5, []);(2, [])];;
let d = [];;

test 1 (isOk a (topol a));;
test 2 (isOk b (topol b));;
test 3 (isOk c (topol c));;
test 4 (isOk d (topol d));;

(* Printf.printf "=== Inne typy...\n";; *)

let a = [("a", ["c"]);("e", ["g"]);("f", ["a";"e"]);("g", ["c";"a"])];;
let b = [(false, [true])];;
let c = [("z", ["c"; "f"; "a"]);("f", ["x"; "a"]);("g", ["h"])];;
let d = [("xx", ["aa"; "gg"]);("ab", ["uw"; "mim"]);("mim", ["uw";"xx"])];;
let e = [("d", ["c"]);("c", ["b"]);("b", ["a"])];;

test 25 (isOk a (topol a));;
test 26 (isOk b (topol b));;
test 27 (isOk c (topol c));;
test 28 (isOk d (topol d));;
test 29 (isOk e (topol e));;


(* Printf.printf "=== Cykliczne..\n";; *)

let a = [("a", ["b"]);("b", ["a"]);("c", ["a"])];;
let a = try topol a with 
	| Cykliczne -> [];;

let b = [("a", ["a"])];;
let b = try topol b with
	| Cykliczne -> [];;

let c = [(1, [4; 5]);(3, [2]);(2, [3])];;
let c = try topol c with
	| Cykliczne -> [];;

let d = [(1, [2]);(2, [3; 4]);(3, [5; 6]);(6, [2])];;
let d = try topol d with
	| Cykliczne -> [];;

test 50 (a = []);;
test 51 (b = []);;
test 52 (c = []);;
test 53 (d = []);;

(* Printf.printf "=== Losowe..\n";; *)

for i = 100 to 500 do
	let a = genTest 30 in
	test i (isOk a (topol a));
done;;

let _ = 
  if !zle <> 0 then Printf.printf "\nBlednych testow: %d...\n" !zle
;;

