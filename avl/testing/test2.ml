open ISet;;
let swap a i j =
    let t = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- t;;

let shuffle a =
    Array.iteri (fun i _ -> swap a i (Random.int (i+1))) a;;

let createSet input =
	let p = ref empty in
		Array.iteri (fun _ x -> p := add x !p) input;
		!p;;

let test_belows n s =
	let tab = Array.make (n + 1) 0 in
		for i = 1 to n do
			tab.(i) <- below i s;
		done;
		tab;;

let sample = [| (7, 20); (1, 25); (2, 10); (17, 21); |];;
let s = createSet sample;;
elements s;;
let s = add (1, 40) s;;
elements s;;

let sample = [|(10, 10); (7, 7); (8, 8); (9, 9); (4, 4); (5, 6); (1, 1) |];;
let s = createSet sample;;
elements s;;
let s = add (2, 3) s;;
elements s;;

"Koniec prostych, pora na sensowne testy:"
let input = [| (1, 1); (3, 3); (5, 5); (7, 7); (9, 9); (11, 11); (13, 13); (15, 15) |];;
Random.self_init ();;
shuffle input;;

let s = createSet input;;

test_belows 15 s;;
let s = add (2, 4) s;;
test_belows 15 s;;

let s = empty;;
let s = add (10, max_int - 1) s;;
below max_int s;;
let s = add (1, 9) s;;
below max_int s;;
let s = add (max_int, max_int) s;;
elements s;;
let s = add (min_int, -1) s;;
