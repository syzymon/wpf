(*Autor: Szymon Tworkowski 406386 Reviewer: Damian Werpachowski 407214*)
type point = float * float
type kartka = point -> int
 
let eps = 1e-12 (*porownywanie liczb rzeczywistych*)
let (=) a b = abs_float(a -. b) <= eps (*w zadaniu porownujemy tylko floaty*)
let (<=) a b = a -. eps < b
let (>=) a b = a +. eps > b


let prostokat (xA, yA) (xB, yB) (x, y) = if(x >= xA && x <= xB && y >= yA && y <= yB) then 1 else 0 

let kwa x = x *. x 

let kolko (xA, yA) r (x, y) = 
  if(kwa(abs_float(x -. xA)) +. kwa(abs_float(y -. yA)) <= kwa(r)) then 1 else 0


let sym (xA, yA) (xB, yB) (x, y) = (*wyznacza punkt symetryczny do P(x,y) wzgl. prostej AB*)
  let a = xB -. xA and b = yB -. yA in (*wspolczynniki prostej prostopadlej do AB*)
  let c = a *. x +. b *.y and d = (-.b) *. xA +. (a) *. yA in (*wyrazy wolne prostych AB i prostopadlej przez P*)
  let dis = kwa a +. kwa b in (*wyznaczanie punktu przeciecia za pomoca wyznacznika 2x2*)
  let (xS, yS) = ((a *. c -. b *. d) /. dis, (a *. d +. b *. c) /. dis) in
  (x +. 2. *. (xS -. x), y +. 2. *. (yS -. y)) (*wynik: punkt P przesuniety o podwojony wektor PS*)

let det (xA, yA) (xB, yB) (x, y) = (*po ktorej stronie prostej AB jest punkt P(x, y)*)
  let sgn z = 
    if z = 0. then 0
    else if z >= 0. then 1 else -1; 
  in
  sgn ((x -. xA) *. (yB -. yA) -. (xB -. xA) *. (y -. yA)) (*iloczyn wektorowy AP i AB*)


let zloz a b f p =
  match det a b p with
    |0 -> f p (*jesli punkt lezy na prostej liczymy go dokladnie raz*)
    |1 -> 0 (*jesli punkt lezy po prawej stronie to nie przebijamy kartki*)
    |_ -> f p + f (sym a b p) (*jesli punkt lezy po lewej stronie liczymy go 2 razy*)

let skladaj l k = List.fold_left (fun acc (p1, p2) -> zloz p1 p2 acc) k l

(*ZBIOR TESTOW*)
(*
let op=[((3.0,7.0),(1.0,1.0));((3.0,10.0),(2.0,3.0));((1.0,8.0),(4.0,7.0));((7.0,3.0),(3.0,8.0))];;
let kartka=kolko (5.,5.) 4. ;;
let test4=skladaj op kartka;;
if(test4(3.0,7.0) <> 4) then assert false;;

let kolo = kolko (0.,0.) 10.;;
if(kolo (1000., 0.) <> 0) then assert false;;
let poziomo = zloz (0.,0.) (1.,0.) kolo;;
if(poziomo (0.,0.) <> 1) then assert false;;
if(poziomo (0.,1.) <> 2) then assert false;;
if(poziomo (0.,-1.) <> 0) then assert false;;
let pionowo = zloz (0.,0.) (0.,1.) kolo;;
if(pionowo (0.,0.) <> 1) then assert false;;
if (pionowo (-1.,0.) <> 2) then assert false;;
if (pionowo (1.,0.) <> 0) then assert false;;
let cwiartka = zloz (0.,0.) (0.,1.) poziomo;;
if (cwiartka (0.,0.) <> 1) then assert false;;
if (cwiartka (-1.,1.) <> 4) then assert false;;
if (cwiartka (-1.,0.) <> 2) then assert false;;
*)