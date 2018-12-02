(* moje testy zadanie modyfikacje *)
open ISet;;

let good = ref 0 and bad = ref 0

let check nr warunek wartosc =
  if warunek = wartosc then
    begin
      Printf.printf "OK - TEST nr %d \n" nr;
      incr good
    end
  else
    begin
      Printf.printf "BLAD !!! TEST nr %d \n" nr;
      incr bad
    end;;



let liczba a = List.length (elements a)

(* Testy na add i remove *)

let a = empty
let a = add (17, 20) a
let a = add (5, 8) a
let a = add (1, 2) a
let a = add (10, 12) a
let a = add (28, 35) a
let a = add (22, 23) a
let a = add (40, 43) a
let a = add (37, 37) a;;

check 1 (is_empty a) false;;
check 2 (mem 29 a) true;;
check 3 (mem 21 a) false;;
check 4 (mem 38 a) false;;
check 5 (mem 37 a) true;;
check 6 (below 8 a = below 9 a) true;;
check 7 (below 29 a) 17;;
check 8 (liczba a) 8;;

let a = add (37, 42) a;;

check 9 (liczba a) 7;;
check 10 (mem 37 a) true;;
check 11 (mem 38 a) true;;
check 12 (mem 39 a) true;;
check 13 (mem 40 a) true;;
check 14 (mem 41 a) true;;
check 15 (mem 42 a) true;;
check 16 (mem 44 a) false;;
check 17 (below 38 a = below 39 a) false;;

let tmp = remove (8, 22) a;;
let tmp = add (8, 22) tmp;;

check 18 (elements tmp = elements a) false;;

(* Testy na split *)

let (l, exists, p) = split 9 a;;

check 19 exists false;;
check 20 (liczba l) 2;;
check 21 (liczba p) 5;;
check 22 (mem 10 l) false;;
check 23 (mem 9 l) false;;
check 24 (mem 8 l) true;;
check 25 (mem 1 l) true;;
check 26 (mem 9 p) false;;
check 27 (mem 10 p) true;;
check 28 (mem 17 p) true;;
check 29 (mem 29 p) true;;
check 30 (mem 24 p) false;;
check 31 (mem 38 p) true;;
check 32 ((elements l @ elements p) = elements a) true;;

let (l, exists, p) = split 21 a;;

check 33 exists false;;
check 34 ((elements l @ elements p) = elements a) true;;

let (l, exists, p) = split 15 a;;
check 35 exists false;;
check 36 ((elements l @ elements p) = elements a) true;;


let b = empty
let b = add (5, 10) b
let b = add (40, 50) b
let b = add (20, 25) b
let b = add (12, 14) b
let b = add (17, 18) b
let b = add (52, 60) b
let b = add (62, 80) b
let b = add (83, 100) b;;

check 37 (mem 41 b) true;;
check 38 (mem 11 b) false;;

let d = empty;;
let (l, ex, p) = split 0 d;;

check 39 (is_empty l) true;;
check 40 (is_empty p) true;;

let d = add (17, 30) d;;
let d = add (1, 3) d;;
let d = add (10, 10) d;;
let d = remove (11, 11) d;;
let d = add (12, 14) d;;
let d = add (32, 35) d;;
let d = add (38, 40) d;;

check 41 (below 36 d = below 37 d) true;;

let d = add (36, 37) d;;

check 42 (below 36 d = below 37 d) false;;

let d = remove (37, 37) d;;
check 43 (below 36 d = below 37 d) true;;

let d = remove (20, 21) d;;

check 44 (elements d) [(1, 3); (10, 10); (12, 14); (17, 19); (22, 30); (32, 36); (38, 40)];;

let (l, ex, p) = split 15 d;;
check 144 (elements l) [(1, 3); (10, 10); (12, 14)];;
check 145 (elements p) [(17, 19); (22, 30); (32, 36); (38, 40)];;

check 45 ((elements l @ elements p) = elements d) true;;
check 46 (liczba l, liczba p) (3, 4);;

check 47 (mem 13 l) true;;
check 48 (mem 14 l) true;;
check 49 ex false;;

let (l, ex, p) = split 25 d;;

check 50 ex true;;
check 51 (elements l) [(1, 3); (10, 10); (12, 14); (17, 19); (22, 24)];;
check 52 (elements p) [(26, 30); (32, 36); (38, 40)];;



Printf.printf "Poprawnie : %d , blednie : %d \n" !good !bad
