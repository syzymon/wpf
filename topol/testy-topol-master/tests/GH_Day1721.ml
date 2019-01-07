(* Autor: Marek Puzyna
 * Licence: Unlicensed
 * Original repo: https://github.com/Day1721/UW *)

open Topol;;

let czy_cykliczne l =
   match (try (topol l) with
      Cykliczne -> []) with
         | [] -> true
         | _ -> false
let test input output =
   let rec loop a b f = function
      | [] -> false
      | h::t -> 
         if f then 
            if h = b then true 
            else loop a b f t
         else if h = a then loop a b true t 
            else loop a b f t
   and pom i a = function
      | [] -> (match i with
         | [] -> true
         | g::o -> pom o (fst g) (snd g))
      | h::t -> match (loop a h false output) with
         | true -> pom i a t
         | false -> false in
   pom (List.tl input) (fst (List.hd input)) (snd (List.hd input))
let a = [(1, [2]); (2, [3]); (3, [4]); (4, [1])]
let b = [(1, [2]); (2, [3]); (3, [4])]
let c = [('A', ['B'; 'C'; 'E']); ('D', ['F'; 'E'; 'G']); ('B', ['C'; 'D']);
   ('C', ['D'; 'F']); ('F', ['G'; 'H'])]
let d = [("zolty", ["niebieski"; "bialy"; "czarny"]); ("bialy", ["czarny"]); 
   ("czarny", []); ("czerwony", ["zielony"; "zolty"; "niebieski"; "czarny"])]
let e = [(1, [2; 5; 8; 3]); (5, [8; 6; 4; 7]); (7, [6; 9; 2]); (8, [6; 9; 3])]
let _ = assert(czy_cykliczne a);
        assert(not (czy_cykliczne b));
        assert(test b (topol b));
        assert(test c (topol c));
        assert(test (List.tl c) (topol (List.tl c)));
        assert(test d (topol d));
        assert(test e (topol e));
        assert(test (List.tl e) (topol (List.tl e)));
        assert(test (b @ e) (topol (b @ e)));
        assert(test (List.tl b @ e) (topol (List.tl b @ e)))
