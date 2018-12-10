(* Author: Marcin Wawerka
 * Licence: unlicensed
 * Oryginal repo: https://github.com/Mucosolvan/Origami *)
let zle = ref 0
let test n b =
  if not b then begin
    Printf.printf "Zly wynik testu %d!!\n" n;
    incr zle
  end

let epsilon = 0.0001

let (=.) x y = (x-.epsilon <= y) && (y <= x+.epsilon)

open Origami;;


(*Printf.printf "==== Testy obowiazkowe...\n";;*)

let a = (1., 1.);;
let b = (10., 10.);;
let c = (5., 5.);;
let d = (5., 1.);;
let e = (5., 10.);;
let x = (5., 11.);;
let y = (0., 10.);;

let f = prostokat a b;;

test 1 (f a = 1);;
test 2 (f b = 1);;
test 3 (f c = 1);;
test 4 (f d = 1);;
test 5 (f e = 1);;
test 6 (f x = 0);;
test 7 (f y = 0);;

let g = zloz d c f;;

test 8  (g a = 2);;
test 9  (g b = 0);;
test 10 (g c = 1);;
test 11 (g d = 1);;
test 12 (g e = 1);;
test 13 (g x = 0);;
test 14 (g y = 1);;

let h = zloz (1., 3.) (6., 3.) g;;

test 15 (h a = 0);;
test 16 (h b = 0);;
test 17 (h c = 2);;
test 18 (h d = 0);;
test 19 (h e = 1);;
test 20 (h x = 0);;
test 21 (h y = 1);;
test 22 (h (1., 5.) = 4);;
test 23 (h (0., 4.) = 2);;
test 24 (h (3., 4.) = 4);;
test 25 (h (5., 3.) = 1);;


(* .... i pewnie sporo innych przykladow ... *)

(*Printf.printf "==== Moje przyklady...\n";;*)

let pom = [(d, c);((1., 3.),(6., 3.))];;
let h = skladaj pom f;;

test 35 (h a = 0);;
test 36 (h b = 0);;
test 37 (h c = 2);;
test 38 (h d = 0);;
test 39 (h e = 1);;
test 40 (h x = 0);;
test 41 (h y = 1);;
test 42 (h (1., 5.) = 4);;
test 43 (h (0., 4.) = 2);;
test 44 (h (3., 4.) = 4);;
test 45 (h (5., 3.) = 1);;

let a = (1., 1.);;
let b = (5., 2.);;
let c = (6., 5.);;
let d = (9., 5.);;
let e = (3., 4.);;

let f = prostokat (0., 0.) (9., 5.);;

test 46 (f a = 1);;
test 47 (f b = 1);;
test 48 (f c = 1);;
test 49 (f d = 1);;
test 50 (f e = 1);;

let f = skladaj [((3.,10.),(3.,0.));((6.,1.),(6.,4.))] f;;

test 51 (f a = 0);;
test 52 (f b = 3);;
test 53 (f c = 2);;
test 54 (f d = 0);;
test 55 (f e = 2);;

let g = kolko (0., 0.) 6.;;

test 56 (g (0., 0.) = 1);;
test 57 (g (6., 0.) = 1);;
test 58 (g (0., 6.1) = 0);;
test 59 (g (3., 5.196) = 1);;
test 60 (g (-3., 5.2) = 0);;

let g = zloz (5.,0.) (10.,0.) g;;

test 61 (g (0., 0.) = 1);;
test 62 (g (6., 0.) = 1);;
test 63 (g (0., 6.1) = 0);;
test 64 (g (3., 5.196) = 2);;
test 65 (g (-3., 5.2) = 0);;

(*Printf.printf "==== Pieklo niebo...\n";;*)

let x = prostokat (-16., -16.) (16., 16.);;

let a = (0., -16.);;
let b = (0., 16.);;
let c = (-16., 0.);;
let d = (16., 0.);;

let x = skladaj [(a,d);(d,b);(b,c);(c,a)] x;;

test 66 (x (0., 0.) = 5);;
test 67 (x (6., 0.) = 3);;
test 68 (x a = 1);;
test 69 (x (-16., -16.) = 0);;
test 70 (x (-8., 8.) = 1);;

let a = (-8., -8.);;
let b = (8., 8.);;
let c = (-8., 8.);;
let d = (8., -8.);;

let x = skladaj [(a,d);(d,b);(b,c);(c,a)] x;;

test 66 (x (0., 0.) = 9);;
test 67 (x (6., 0.) = 6);;
test 68 (x a = 1);;
test 69 (x (-16., -16.) = 0);;
test 70 (x (0., 8.) = 3);;

let _ = 
  if !zle <> 0 then  
    Printf.printf "\nBlednych testow: %d...\n" !zle
;;
