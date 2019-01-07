(* Autor: Szymon Pajzert 
 * Licence: GNU GPL v3 
 * Original repo: https://github.com/Nhemisirmkow/ *)
let string_of_list ~s_conv l =
  List.fold_left (fun x y -> x  ^ (s_conv y) ^ "; ") "[" l ^ "]"

(*with given s_conv from 'a type to string creates printable data*)
let string_of_data ~s_conv data =
  let string_creator acc (value, dependencies) =
    acc ^ (s_conv value ^ ", " ^ (string_of_list ~s_conv dependencies) ^ "; ")
  in
  List.fold_left string_creator "[" data ^ "]"

let string_of_intlist = string_of_list ~s_conv:string_of_int
let string_of_intdata = string_of_data ~s_conv:string_of_int

let validate input =
  let time = Sys.time() in
  let output = input |> Topol.topol in
  let computation = Sys.time() -. time in
  (*graph of connections for given value returns values that should be behind*)
  let graph = List.fold_left (fun map (v, dep) -> PMap.add v dep map) PMap.empty input in
  let rec folder ~setvisited (result, visited) value =
    let newresult = result && (not (PMap.mem value visited)) in
    if setvisited && newresult then
      let dependencies = try PMap.find value graph with | Not_found -> [] in
      let newvisited = PMap.add value true visited in
      List.fold_left (folder ~setvisited:false) (newresult, newvisited) dependencies
    else
      (newresult, visited)
  in
  computation, fst (List.fold_left (folder ~setvisited:true) (true, PMap.empty) output)

let () =
  Random.init 1234;
  Random.self_init ()
(*let () = Printf.printf "\nPodaj liczbę testów\n%!"*)
let test_num = 5_00
(*let () = Printf.printf "\nPodaj maksymalny rozmiar testów\n%!"*)
let test_max = 1_000
(*let () = Printf.printf "\nPoczątek testów bez cykli z liczbami\n%!"*)
let bad = ref 0

(*creates random list containing numbers in increasing order from min_val including
to max_val excluding*)
let rec random_list min_val max_val =
  if min_val >= max_val then []
  else
    let n = Random.int (max_val - min_val) + min_val in
    n :: random_list (n + 1) max_val

let create_input max_val =
  let rec temp acc i =
    if i = max_val - 1 then (i, []) :: acc
    else
      let new_dep = (i+1) :: random_list (i+2) max_val in
      temp ((i, new_dep) :: acc) (i+1)
  in
    temp [] 0

let test number max_val =
  let input = create_input max_val in
  let comptime, result = input |> validate in
  (*Printf.printf "Dla testu o rozmiarze %d czas %f\n" max_val comptime;*)
  if not result then begin
    incr bad;
    let oc = open_out_gen [Open_append; Open_creat] 511 ("wrong_output.out") in
    Printf.fprintf oc "Wrong input number %d\n" number;
    Printf.fprintf oc "%s" (string_of_intdata input);
    close_out oc;
    Printf.printf "Wrong input, saved as %d\n" number
  end
;;

for i = 1 to test_num do
  test i test_max
done
