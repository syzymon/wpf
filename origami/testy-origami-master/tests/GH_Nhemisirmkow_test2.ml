(* Autor: Marcin Michorzewski 
 * Licence: GNU GPL v3 
 * Original repo: https://github.com/Nhemisirmkow/Origami *)

open Origami;;
let op=[((10.0,8.0),(2.0,4.0));((0.0,8.0),(6.0,1.0));((7.0,0.0),(9.0,2.0));((8.0,3.0),(0.0,8.0))];;
let kartka=prostokat (0.,0.) (10.,10.) ;;
let test0=skladaj op kartka;;
assert (test0 (4.0,0.0)=0);;
let op=[((8.0,6.0),(10.0,7.0));((7.0,2.0),(3.0,2.0));((9.0,0.0),(1.0,9.0));((2.0,5.0),(9.0,10.0))];;
let kartka=prostokat (0.,0.) (10.,10.) ;;
let test1=skladaj op kartka;;
assert (test1 (9.0,7.0)=0);;
let op=[((8.0,2.0),(2.0,5.0));((5.0,9.0),(5.0,1.0));((8.0,5.0),(7.0,8.0));((8.0,4.0),(4.0,0.0))];;
let kartka=kolko (5.,5.) 4. ;;
let test2=skladaj op kartka;;
assert (test2 (3.0,5.0)=0);;
let op=[((1.0,3.0),(0.0,8.0));((1.0,5.0),(7.0,7.0));((8.0,7.0),(9.0,5.0));((2.0,4.0),(10.0,10.0))];;
let kartka=kolko (5.,5.) 4. ;;
let test3=skladaj op kartka;;
assert (test3 (3.0,8.0)=0);;
let op=[((3.0,7.0),(1.0,1.0));((3.0,10.0),(2.0,3.0));((1.0,8.0),(4.0,7.0));((7.0,3.0),(3.0,8.0))];;
let kartka=kolko (5.,5.) 4. ;;
let test4=skladaj op kartka;;
assert (test4 (3.0,7.0)=4);;
