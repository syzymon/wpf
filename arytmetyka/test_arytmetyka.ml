open Arytmetyka;;
(* Warsaw University, Faculty of Mathematics, Informatics and Mechanics,          *)
(* first year of Computer Science, Introduction to Functional Programming         *)
(* Tests for arytmetyka.ml (implementation of arytmetyka.mli, task no 1)          *)
(* Author: Franciszek Hnatów, 406161                                              *)

let jeden = wartosc_dokladna 1.0;;
let zero = wartosc_dokladna 0.0;;
let duzo = podzielic jeden (wartosc_od_do 0.0 1.0);;

assert(in_wartosc (razy jeden zero) 0.0);;
assert(in_wartosc (razy zero (wartosc_od_do 1.0 10.0)) 0.0);;
assert(in_wartosc (razy zero (wartosc_od_do 0.0 1.0)) 0.0);;
assert(in_wartosc (razy zero (wartosc_od_do 0.0 1.0)) 0.0);;
assert(sr_wartosc duzo = infinity);;
assert(in_wartosc (razy zero duzo) 0.0);;



(**
let a = wartosc_od_do 9.0 27.0;; (* [9.0, 27.0] *)

assert(not(in_wartosc a 8.999));;
assert(max_wartosc a = 27.0);;
assert(min_wartosc a = (9.0));;
assert(sr_wartosc a = 18.0);;

let b = wartosc_od_do (-10.0) 10.0;; (* [-10.0, 10.0]*)

assert(in_wartosc b (-8.2));;
assert(max_wartosc b = 10.0);;
assert(min_wartosc b = (-10.0));;
assert(sr_wartosc b = 0.0);;

let c = podzielic a b;; (* (-inf, -0.9] u [0.9, inf) *)

(*assert(not(in_wartosc c 0.5));;*)
assert(max_wartosc c = infinity);;
assert(min_wartosc c = neg_infinity);;
assert(classify_float (sr_wartosc c) = FP_nan);;

let d = plus b (wartosc_dokladna (-20.0));; (* [-30.0, -10.0] *)

assert(in_wartosc d (-15.0));;
assert(not (max_wartosc d = (-30.0)));;
assert(min_wartosc d = (-30.0));;
assert(not (sr_wartosc d = (-19.0)));;

let e = razy c d;; (* (-inf, -9.0] u [9.0, inf)) *)

assert(not (in_wartosc e 0.0));;
assert(not (max_wartosc e = 9.0));;
assert(min_wartosc e = neg_infinity);;
assert(classify_float (sr_wartosc e) = FP_nan);;

let f = plus e (wartosc_od_do (-100.0) 100.0);;  (* (-inf, inf) *)

assert(not (in_wartosc f nan));;
assert(in_wartosc f 1000000000.0);;

let g = razy f (wartosc_dokladna 0.0);;  (* = 0.0 *)
let h = wartosc_dokladna 0.0;;

assert(g = h);;
assert(in_wartosc g 0.0);;
assert(in_wartosc h 0.0);;

let i = podzielic f g;; (* empty interval *)

assert(compare (razy i a) i = 0);;
assert(compare (podzielic i b) i = 0);;
assert(compare (plus i c) i = 0);;
assert(compare (minus i d) i = 0);;

Printf.printf "Debug: All tests passed%!"
**)
