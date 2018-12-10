(* Author: Krzysztof Pszeniczny
 * Licence: unlicensed
 * Oryginal repo: https://github.com/amharc/WPF *)

open Origami
open Printf
open List

let zle = ref 0 and ok = ref 0

let test n = function
    | true -> incr ok (*; printf "Test %d OK\n" n*)
    | false -> incr zle ; printf "Test %d ZLE\n" n

(* proste testy ;) *)

let k = prostokat (-10., -10.) (-10., 5.);;

test 1 (1= (k (-10., -10.)));;
test 2 (1= (k (-10., 0.)));;
test 3 (1= (k (-10., 5.)));;
test 4 (0= (k (-10., 5.5)));;
test 5 (0= (k (-10., -10.5)));;
test 6 (0= (k (-11., 3.)));;

let k = kolko (3., 0.) 5.;;

test 7 (1= (k (3., 0.)));;
test 8 (1= (k (3., 5.)));;
test 9 (1= (k (0., 4.)));;
test 10 (0= (k(-2., 2.)));;
test 11 (0= (k(2., 5.)));;
test 12 (0= (k(8., 1.)));;

(* pasek *)

let duzo = 5000;;

let k = prostokat (0., 0.) (float duzo, 1.);;
let rec range a b = if a = b then [a] else a::range (a+1) b;;
let k = fold_left (fun acc x -> let x = float x in zloz (x, 0.) (x,2.) acc) k (rev (range 1 (duzo-1))) ;;

test 13 (duzo = k (0.5, 0.5));;
(*
let _ =
    printf "\nTestow OK: %d\nTestow ZLE: %d\n" !ok !zle *)
