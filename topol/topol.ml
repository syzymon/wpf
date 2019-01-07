open PMap;;

exception Cykliczne

let rec topol l =
  let mapa = ref empty in
  let odwrotnosc = ref empty in
  let n = ref 0 in

  let dodaj x =
    if mem x !mapa = false then
    begin
      mapa := add x !n !mapa;
      odwrotnosc := add !n x !odwrotnosc;
      incr n;
    end
    else ();
  in

  let rec mapuj l = 
    List.iter (fun (a, b) -> dodaj a; List.iter (fun x -> dodaj x;) b;) l;
  in
  mapuj l;

  let rozmiar = !n in
  let sasiedzi = Array.make rozmiar [] in

  let tworz_liste l = 
    List.iter (
      fun (a, b) -> 
        let nrA = find a !mapa in 
        List.iter (fun x -> sasiedzi.(nrA) <- (find x !mapa) :: sasiedzi.(nrA)) b) l 
  in
  tworz_liste l;

  
  let stan = Array.make rozmiar 0 in
  let stos = ref [] in

  let rec dfs u = 
    stan.(u) <- 1;
    List.iter (
      fun v -> 
        if stan.(v) = 1 then raise Cykliczne
        else if stan.(v) = 0 then dfs v 
        else ();
    ) sasiedzi.(u);
    stan.(u) <- 2;
    stos := (find u !odwrotnosc) :: !stos;
  in

  for i = 0 to rozmiar - 1 do
    if stan.(i) = 0 then dfs i 
    else ();
  done;

  !stos;;
