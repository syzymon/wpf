open ISet;;

let test n b =
	if not b then
		Printf.printf "Zły wynik testu %i\n" n
	else  Printf.printf "test %n ok!\n" n
;;

let s = empty;;
let s = add (-1,1) s;;
let s = add (3,4) s;;
let s = add (6,7) s;;
let s = add (9,10) s;;
let s = add (14,15) s;;
let s = add (17,18) s;;
let s = add (20,21) s;;


test 61 (elements (add (-10,-5) s) = [(-10, -5); (-1, 1); (3, 4); (6,7); (9, 10); (14, 15); (17, 18); (20, 21)]);;
test 62 (elements (add (-10,-2) s) = [(-10, 1); (3, 4); (6, 7); (9,10); (14, 15); (17, 18); (20, 21)]);;
test 63 (elements (add (-10,-1) s) = [(-10, 1); (3, 4); (6, 7); (9,10); (14, 15); (17, 18); (20, 21)]);;
test 64 (elements (add (-10,0) s) = [(-10, 1); (3, 4); (6, 7); (9,10); (14, 15); (17, 18); (20, 21)]);;
test 65 (elements (add (-10,1) s) = [(-10, 1); (3, 4); (6, 7); (9,10); (14, 15); (17, 18); (20, 21)]);;
test 66 (elements (add (-10,2) s) = [(-10, 4); (6, 7); (9, 10); (14,15); (17, 18); (20, 21)]);;
test 67 (elements (add (-10,8) s) = [(-10, 10); (14, 15); (17, 18); (20, 21)]);;
test 68 (elements (add (-10,12) s) = [(-10, 12); (14, 15); (17, 18); (20, 21)]);;
test 69 (elements (add (-10,17) s) = [(-10, 18); (20, 21)]);;
test 70 (elements (add (-10,19) s) = [(-10, 21)]);;
test 71 (elements (add (-10,21) s) = [(-10, 21)]);;
test 72 (elements (add (-10,25) s) = [(-10, 25)]);;
test 73 (elements (add (-1,25) s) = [(-1, 25)]);;
test 74 (elements (add (0,25) s) = [(-1, 25)]);;
test 75 (elements (add (1,25) s) = [(-1, 25)]);;
test 76 (elements (add (2,25) s) = [(-1, 25)]);;
test 77 (elements (add (3,25) s) = [(-1, 1); (3, 25)]);;
test 78 (elements (add (11,25) s) = [(-1, 1); (3, 4); (6, 7); (9, 25)]);;
test 79 (elements (add (12,25) s) = [(-1, 1); (3, 4); (6, 7); (9, 10); (12, 25)]);;
test 80 (elements (add (15,25) s) = [(-1, 1); (3, 4); (6, 7); (9, 10); (14, 25)]);;
test 81 (elements (add (19,25) s) = [(-1, 1); (3, 4); (6, 7); (9, 10); (14, 15); (17, 25)]);;
test 82 (elements (add (20,25) s) = [(-1, 1); (3, 4); (6, 7); (9, 10); (14, 15); (17, 18); (20, 25)]);;
