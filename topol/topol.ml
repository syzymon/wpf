(*Autor: Szymon Tworkowski 406386 Reviewer: Damian Werpachowski 407214*)
open PMap;;

exception Cykliczne

let topol l =
  let mapa = ref empty in (*mapa w ktorej przypisujemy wierzcholkom liczby naturalne*)
  let odwrotnosc = ref empty in (*odwrotna mapa z liczb naturalnych w oryginalne wierzcholki*)
  let n = ref 0 in (*aktualna liczba przetworzonych roznych wierzcholkow*)

  let dodaj x = (*nadaje numer wierzcholkowi x*)
    if mem x !mapa = false then
    begin
      mapa := add x !n !mapa;
      odwrotnosc := add !n x !odwrotnosc;
      incr n;
    end;
  in

  let mapuj l = (*przenumerowuje cala liste z wejscia*)
    List.iter (fun (a, b) -> dodaj a; List.iter (fun x -> dodaj x;) b;) l;
  in
  mapuj l;

  let rozmiar = !n in (*ostateczna liczba wierzcholkow grafu*)
  let sasiedzi = Array.make rozmiar [] in (*tablica sasiedztwa przenumerowanego grafu*)

  let tworz_liste l = (*zamienia wejscie na poprawna liste sasiedztwa przenumerowanego grafu*)
    List.iter (
      fun (a, b) -> 
        let nrA = find a !mapa in 
        List.iter (
          fun x -> 
            sasiedzi.(nrA) <- (find x !mapa) :: sasiedzi.(nrA)
        ) b
    ) l 
  in
  tworz_liste l;

  
  let stan = Array.make rozmiar 0 in (*stan przetworzenia wierzcholka*)
  let stos = ref [] in (*wynikowy porzadek topologiczny*)

  let rec dfs u =
    stan.(u) <- 1; (*oznaczamy wierzcholek jako odwiedzony, ale nie przetworzony*)
    List.iter (
      fun v -> 
        if stan.(v) = 1 then raise Cykliczne
        else if stan.(v) = 0 then dfs v;
    ) sasiedzi.(u);
    stan.(u) <- 2; (*oznaczamy wierzcholek jako przetworzony*)
    stos := (find u !odwrotnosc) :: !stos;
  in

  for i = 0 to rozmiar - 1 do (*przeszukujemy od kazdego wierzcholka*)
    if stan.(i) = 0 then dfs i;
  done;

  !stos;;



(*ZBIOR TESTOW*)
(*
let sprawdz_cykl l =
  match (try (topol l) with
     Cykliczne -> []) with
        | [] -> true
        | _ -> false

let testuj we wy =
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
      | h::t -> match (loop a h false wy) with
        | true -> pom i a t
        | false -> false in
  pom (List.tl we) (fst (List.hd we)) (snd (List.hd we))

let a = [(1, [2]); (2, [3]); (3, [4]); (4, [1])]
let b = [(1, [2]); (2, [3]); (3, [4])]
let c = [('A', ['B'; 'C'; 'E']); ('D', ['F'; 'E'; 'G']); ('B', ['C'; 'D']);
   ('C', ['D'; 'F']); ('F', ['G'; 'H'])]
let d = [("blep", ["abc"; "eee"; "nghu"]); ("eee", ["nghu"]); 
   ("nghu", []); ("42", ["2137"; "blep"; "abc"; "nghu"])]
let e = [(1, [2; 5; 8; 3]); (5, [8; 6; 4; 7]); (7, [6; 9; 2]); (8, [6; 9; 3])];;

assert(sprawdz_cykl a);
assert(not (sprawdz_cykl b));
assert(testuj b (topol b));
assert(testuj c (topol c));
assert(testuj (List.tl c) (topol (List.tl c)));
assert(testuj d (topol d));
assert(testuj e (topol e));
assert(testuj (List.tl e) (topol (List.tl e)));
assert(testuj (b @ e) (topol (b @ e)));
assert(testuj (List.tl b @ e) (topol (List.tl b @ e)))
*)