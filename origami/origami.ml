type point = float * float
type kartka = point -> int
 
let eps = 1e-12
let (=) a b = abs_float(a -. b) <= eps
let (<=) a b = a -. eps < b 
let (>=) a b = a +. eps > b

let prostokat (xA, yA) (xB, yB) (x, y) = if(x >= xA && x <= xB && y >= yA && y <= yB) then 1 else 0 

let kwa x = x *. x

let kolko (xA, yA) r (x, y) = 
  if(kwa(abs_float(x -. xA)) +. kwa(abs_float(y -. yA)) <= kwa(r)) then 1 else 0

let sym (xA, yA) (xB, yB) (x, y) = 
  let a = xB -. xA and b = yB -. yA in
  let c = a *. x +. b *.y and d = (-.b) *. xA +. (a) *. yA in
  let dis = kwa a +. kwa b in 
  let (xS, yS) = ((a *. c -. b *. d) /. dis, (a *. d +. b *. c) /. dis) in
  (x +. 2. *. (xS -. x), y +. 2. *. (yS -. y)) 

let det (xA, yA) (xB, yB) (x, y) = 
  let sgn z = 
    if z = 0. then 0
    else if z >= 0. then 1 else -1; 
  in
  sgn ((x -. xA) *. (yB -. yA) -. (xB -. xA) *. (y -. yA)) 

let zloz a b f p =
  match det a b p with
    |0 -> f p
    |1 -> 0
    |_ -> f p + f (sym a b p)

let skladaj l k = List.fold_left (fun acc (p1, p2) -> zloz p1 p2 acc) k l
