(* Autor: Tomasz Kanas 
 * Licence: Unspecified *)

open Origami;;

let test a b msg = if a<>b then (print_int a; print_string "<>"; print_int b; print_string " test: "; print_endline msg);;


let p1 = prostokat (0., 0.) (10., 10.)
let k1 = kolko (5., 5.) 5.
let l1 = [((0., 0.), (10., 10.));
	  ((5., 0.), (10., 5.));
	  ((10., 0.), (0., 10.));
	  ((2.5, 0.), (2.5, 10.))];;
let l2 = [((8., 0.), (10., 2.));
	  ((6., 0.), (10., 4.));
	  ((4., 0.), (10., 6.));
	  ((2., 0.), (10., 8.));
	  ((0., 0.), (10., 10.));
	  ((0., 2.), (8., 10.));
	  ((0., 4.), (6., 10.));
	  ((0., 6.), (4., 10.));
	  ((0., 8.), (2., 10.))];;

let construct a b n =
  let rec pom (x1, y1) (x2, y2) n a =
    let sx = (x1 +. x2) /. 2. and sy = (y1 +. y2) /. 2. in
    if n=0 then ((sx, sy),a) else
      let (w,wyn) = pom (x1, y1) (sx, sy) (n - 1) a in
	(w,((sx, y1), (sx, y2))::(((sx, sy), (x1, sy))::wyn)) in
  pom a b n [];;
    
let p2 = skladaj l1 p1
let p3 = skladaj l2 p1
let k2 = skladaj l1 k1;;

test (p2 (7., 3.)) 0 "0.1: p2";;
test (p2 (5., 8.)) 0 "0.2: p2";;
test (p2 (3., 5.)) 0 "0.3: p2";;
test (p2 (5., 5.)) 0 "0.4: p2";;
test (p2 (0., 0.)) 2 "1: p2";;
test (p2 (0., 10.)) 2  "2: p2";;
test (p2 (2.5, 2.5)) 2 "3: p2";;
test (p2 (2.5, 7.5)) 2 "4: p2";;
test (p2 (2.5, 5.)) 4 "5: p2";;
test (p2 (0., 5.)) 5 "6: p2";;
test (p2 (1., 2.)) 4 "7: p2";;
test (p2 (1., 5.)) 8 "8: p2";;
test (p2 (1., 8.)) 4 "9: p2";;

test (k2 (7., 3.)) 0 "0.1: k2";;
test (k2 (5., 8.)) 0 "0.2: k2";;
test (k2 (3., 5.)) 0 "0.3: k2";;
test (k2 (5., 5.)) 0 "0.4: k2";;
test (k2 (2.5, 2.5)) 2 "1: k2";;
test (k2 (2.5, 7.5)) 2 "2: k2";;
test (k2 (2.5, 5.)) 4 "3: k2";;
test (k2 (0., 5.)) 5 "4: k2";;
test (k2 (1., 3.)) 4 "5: k2";;
test (k2 (1., 5.)) 8 "6: k2";;
test (k2 (1., 7.)) 4 "7: k2";;

test (p3 ((-4.), 6.)) 2 "1: p3";;
test (p3 ((-3.), 5.)) 1 "2: p3";;
test (p3 ((-3.), 7.)) 2 "3: p3";;
test (p3 ((-2.), 6.)) 3 "4: p3";;
test (p3 ((-2.5), 6.5)) 4 "5: p3";;
test (p3 ((-2.), 8.)) 4 "6: p3";;
test (p3 ((-1.), 7.)) 3 "7: p3";;
test (p3 ((-1.5), 7.5)) 6 "8: p3";;
test (p3 (0., 8.)) 5 "9: p3";;
test (p3 ((-1.), 9.)) 4 "10: p3";;
test (p3 ((-0.5), 8.5)) 8 "11: p3";;
test (p3 (0., 10.)) 6 "12: p3";;
test (p3 (1., 9.)) 5 "13: p3";;
test (p3 (0.5, 9.5)) 10 "14: p3";;

let (point, l) = construct (0., 0.) (10., 10.) 1;;
let p = skladaj l p1;;
test (p point) 4 "big 1";;

let (point, l) = construct (0., 0.) (10., 10.) 2;;
let p = skladaj l p1;;
test (p point) 16 "big 2";;

let (point, l) = construct (0., 0.) (10., 10.) 4;;
let p = skladaj l p1;;
test (p point)  (1 lsl 8) "big 3";;

let (point, l) = construct (0., 0.) (10., 10.) 10;;
let p = skladaj l p1;;
test (p point) (1 lsl 20) "big 4";;

let (point, l) = construct (0., 0.) (10., 10.) 15;;
let p = skladaj l p1;;
test (p point) (1 lsl 30) "big 5";;
