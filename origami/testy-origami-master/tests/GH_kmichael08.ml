(* Author: Michał Kuźba
 * Licence: unlicensed
 * Oryginal repo: https://github.com/kmichael08/wpf-OCaml *)

let zle = ref 0
let test n b =
  if not b then begin
    Printf.printf "Zly wynik testu %d!!\n" n;
    incr zle
  end
open Origami;;

let pr = prostokat (2.0, 2.0) (4.0, 5.0);;
test 1 ((pr (3.0,3.0)) =  1);;
test 2  ((pr (2.0,2.0)) =  1);;
test 3  ((pr (4.0,5.0)) =  1);;
test 4  ((pr (4.0,3.0)) =  1);;
test 5  ((pr (10.0, 0.0)) = 0);;
 
(*Prostokat zlozony na pol*)
let pr = (zloz (3.0,0.0) (3.0,10.0) pr);;

test 10 ((pr (3.0,5.0)) =  1);;
test 11 ((pr (4.0,5.0)) =  0);;
test 12 ((pr (2.5, 2.5)) = 2);;
test 13 ((pr (3.5, 2.5)) = 0);;
test 14 ((pr (3.0, 2.5)) = 1);;
test 15 ((pr (2.0, 5.0)) = 2);;
 
(*Kwadrat zlozony po przekatnej*)
let pr = prostokat (0.0, 0.0) (10.0, 10.0);;
let pr = zloz (-50.0, -50.0) (-49.0, -49.0) pr;;
test 20 ((pr (-1.0, -1.0)) = 0);;
test 21 ((pr (0.0, 0.0)) = 1);;
test 22 ((pr (10.0, 10.0)) = 1);;
test 23 ((pr (10.1, 10.1)) = 0);;
test 24 ((pr (5.0, 6.0)) = 2);;
test 25 ((pr (5.0, 10.1)) = 0);;
test 26 ((pr (0.0, 10.0)) = 2);;
test 27 ((pr (6.0, 5.0)) = 0);;
test 28 ((pr (10.0, 0.0)) = 0);;

let k = prostokat (1., 1.) (5., 4.) ;;
test 29 ((k (3., 3.)) = 1);;
test 30 ((k (1., 1.)) = 1);;
test 31 ((k (5., 4.)) = 1);;
test 32 ((k (1., 4.)) = 1);;
test 33 ((k (5., 1.)) = 1);;

let k = prostokat (0., 0.) (42., 29.);;
let k = skladaj [((15., 2.), (27., 26.)); ((15., 15.), (1., 27.)); ((4., 18.), (3., 18.))] k;;

test 34 ((k (11., 3.)) = 1);;
test 35 ((k (7., 1.)) = 1);;
test 36 ((k (-4., 6.)) = 1);;
test 37 ((k (2., 3.)) = 2);;
test 38 ((k (14., 6.)) = 2);;
test 39 ((k (-1., 14.)) = 2);;
test 40 ((k (5., 2.)) = 3);;
test 41 ((k (13., 8.)) = 4);;
test 42 ((k (1., 10.)) = 4);;
test 43 ((k (6., 15.)) = 8);;
test 44 ((k (3., 17.)) = 8);; 


let k = prostokat (-21., -14.5) (21., 14.5);;
let k = zloz (11., -17.) (11., 2.) k;;
let k = zloz (-3., 3.5) (-7., 8.5) k ;;
let k = zloz (-14., -9.5) (-4., -7.5) k;;

test 45 ((k (0., 10.)) = 0);;
test 46 ((k (-6., 6.5)) = 2);;
test 47 ((k (-16., -9.5)) = 2);;
test 48 ((k (-14., -1.5)) = 3);;
test 49 ((k (-13., -6.5)) = 4);;
test 50 ((k (3., 0.)) = 4);;
test 51 ((k (-1.5, -1.)) = 4);;
test 52 ((k (-3., -5.)) = 6);;
test 53 ((k (-0.7, -1.9)) = 7);;
test 54 ((k (2., -4.5)) = 8);;



let k = kolko (174., -63.) 132.;;
let k = zloz (218., -107.) (262., -19.) k;;
let k = skladaj [(174., 58.), (97., -30.); (86., -129.), (174., -151.); (141., -8.), (119., -96.); (119., -178.), (240., -62.)] k;;
let k = zloz (240., -40.) (218., 13.) k;;

test 55 ((k (170., 70.)) = 0);;
test 56 ((k (130., -30.)) = 0);;
test 57 ((k (220., -95.)) = 0);;
test 58 ((k (175., -30.)) = 1);;
test 59 ((k (180., 20.)) = 3);;
test 60 ((k (140., -74.)) = 3);;
test 61 ((k (149., -102.)) = 4);;
test 62 ((k (154., -100.)) = 5);;
test 63 ((k (165., -99.)) = 6);;
test 64 ((k (165., -104.)) = 7);;
test 65 ((k (182., -106.)) = 8);;
test 66 ((k (165., -108.)) = 9);;
test 67 ((k (166., -123.)) = 9);;
test 68 ((k (171., -115.)) = 10);;
let _ =
  if !zle <> 0 then  
    Printf.printf "\nBlednych testow: %d...\n" !zle
;;
