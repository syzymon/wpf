(* Author: Artur Puzio
 * Licence: Do What the Fuck You Want to Public License *)

open Topol

exception WA;;

let test graph order =
  let hashtbl = Hashtbl.create (List.length order)
  in
  List.iteri (fun i x -> Hashtbl.add hashtbl x i) order;
  let check_one (v, l) =
    List.iter (fun u ->
      if (Hashtbl.find hashtbl v) > (Hashtbl.find hashtbl u)
      then raise WA;) l
  in
  try (List.iter check_one graph; true)
  with WA -> false;;

(* The important test case *)
let g = [(1, [2]); (1, [3])];;

assert (test g (topol g))

(* Other variations *)
let g = [(1, [2]); (2, [5]); (1, [3]); (1, [4]); (2, [4]); (2, [6])];;

assert (test g (topol g))

let g = [(1, [2]); (3, [5]); (1, [3]); (3, [4]); (1, [4]); (3, [6])];;

assert (test g (topol g))
