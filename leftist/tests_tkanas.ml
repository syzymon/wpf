(* Author: Tomasz Kanas 
 * Licence: unspecified *)
open Leftist;;
                  (* num jest numerem operacji (delete_min) na której wystąpił błąd *)
let test a b num msg =
  if a = b then print_endline "ok"
  else (print_int num; print_endline msg);; 

let rec zwin l q num msg =
  try
    match l with
    | [] -> test q empty num msg
    | h::t -> let (mn,r) = delete_min q in test mn h num msg; zwin t r (num+1) msg
  with Empty -> (print_int num; print_string "Empty"; print_endline msg);;

let a = add 0. empty;;        (* 0.*)
let b = add 1. empty;;        (* 1. *)
let c = add (-0.1) empty;;    (* -0.1 *)
let d = add 7. a;;            (* 0., 7. *)
let e = add (-3.) d;;         (* -3., 0., 7. *)
let f = add (-0.5) c;;        (* -0.5, -0.1 *)
let g = join b c;;            (* -0.1, 1.*)
let h = join d e;;            (* -3., 0., 0., 7., 7. *)
let i = join f e;;            (* -3., -0.5, -0.1, 0., 7. *)
let j = join h i;;            (* -3., -3., -0.5, -0.1, 0., 0., 0., 7., 7., 7. *)

let la = [0.];;
let lb = [1.];;
let lc = [-0.1];;
let ld = la @ [7.];;
let le = -3.::ld;;
let lf = -0.5::lc;;
let lg = lc @ lb;;
let lh = [-3.; 0.; 0.; 7.; 7.];;
let li = [-3.; -0.5; -0.1; 0.; 7.];;
let lj = [-3.; -3.; -0.5; -0.1; 0.; 0.; 0.; 7.; 7.; 7.];;

test (join empty empty) empty (-1) ": empty + empty";;
zwin la a 0 ": a";;
zwin lb b 0 ": b";;
zwin lc c 0 ": c";;
zwin ld d 0 ": d";;
zwin le e 0 ": e";;
zwin lf f 0 ": f";;
zwin lg g 0 ": g";;
zwin lh h 0 ": h";;
zwin li i 0 ": i";;
zwin lj j 0 ": j";;
