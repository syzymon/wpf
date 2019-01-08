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
