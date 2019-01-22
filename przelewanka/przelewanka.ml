(*Autor: Szymon Tworkowski 406386 Reviewer: Damian Werpachowski 407214*)
let rec nwd a b =
  if b = 0 then a else nwd b (a mod b);;

let przelewanka szkl = 
  let n = Array.length szkl in
  let gcd = Array.fold_left (fun x y -> nwd x (fst y)) 0 szkl in (*nwd wszystkich pojemnosci*)
  if ((Array.map (fun x -> snd x) szkl) = (Array.make n 0)) then 0 else (*jesli juz mamy to co chcemy to wynik = 0*)
  if (Array.exists (fun x -> (snd x) mod gcd <> 0) szkl) || (*jezeli jakas koncowa wartosc nie jest podzielna przez nwd, nie da sie*)
     (Array.for_all (fun x -> fst x = 0 || ((snd x <> fst x) && (snd x <> 0))) szkl) (*na koncu jakas szlanka musi byc pusta lub pelna*)
  then (-1) else begin
    let mapa = Hashtbl.create 1234567 in (*losowy rozmiar poczatkowy mapy*)
    let q = Queue.create() in
    let wzorzec = List.map (fun x -> snd x) (Array.to_list szkl) in (*oczekiwany stan*)
    let wyn = ref (-1) in
    
    let wstaw x d = (*odwiedza stan x z odlegloscia d+1*)
      if (Hashtbl.mem mapa x = false) then begin
        if x = wzorzec then wyn := d + 1 else begin 
          Hashtbl.add mapa x (d + 1); Queue.push x q;
        end
      end;
    in
    wstaw (Array.to_list(Array.make n 0)) (-1); (*stan poczatkowy (0, 0, ..., 0) - uzywamy list jako kluczy bo sa immutable*)

    while !wyn = (-1) do (*warunek z nwd jest konieczny i wystarczajacy, wiec wystarczy sprawdzac czy bfs znalazl juz wynik*)
      let u = Queue.take q in
      let tabU = Array.of_list(u) in
      let odl = Hashtbl.find mapa u in

      let dolej x = (*probuje napelnic szklanki woda - przyjmuje aktualna kopie tablicy u*)
        for i = 0 to n - 1 do
          x.(i) <- fst (szkl.(i));
          wstaw (Array.to_list x) odl;
          x.(i) <- tabU.(i); (* w kazdym wywolaniu x jest kopia tablicy u - cofamy zmiany*)
        done
      in

      let wylej x = (*oproznia szklanki*)
        for i = 0 to n - 1 do
          x.(i) <- 0;
          wstaw (Array.to_list x) odl;
          x.(i) <- tabU.(i);
        done
      in

      let przelej x = (*wszystkie n^2 kombinacji przelewania pomiedzy szklankami*)
        for i = 0 to n - 1 do 
          for j = 0 to n - 1 do
            if i <> j then begin
              let zmiana = min x.(i) (fst szkl.(j) - x.(j)) in (*ile wlejemy do drugiej szklanki*)
              x.(i) <- x.(i) - zmiana;
              x.(j) <- x.(j) + zmiana;
              wstaw (Array.to_list x) odl;
              x.(i) <- tabU.(i); x.(j) <- tabU.(j);
            end;
          done;
        done;
      in

      dolej (Array.copy tabU);
      wylej (Array.copy tabU);
      przelej (Array.copy tabU);
    done;
    !wyn
  end;
;;

(*ZBIOR TESTOW
assert (przelewanka [| (1,1); (3,2); (6,5) |] = (5));;
assert (przelewanka [| (557,349); (73,49) |] = (-1));;
assert (przelewanka [| (48,9); (12,0); (1,1); (65,64) |] = (10));;
assert (przelewanka [| (1,1); (0,0); (2,2); (0,0); (2,0); (0,0); (0,0);
  (1,0); (2,0); (1,0) |] = (2));;
assert (przelewanka [| (9,9); (13,12) |] = (10));;
assert (przelewanka [| (11,2); (11,10); (4,0); (10,8); (21,16) |] = (12));;
assert (przelewanka [| (3,2); (3,3); (1,1); (2,0) |] = (3));;
assert (przelewanka [| (0,0); (2,2); (1,0); (1,1); (1,0); (2,2); (1,0);
  (0,0); (0,0) |] = (3));;
assert (przelewanka [| (20,7); (12,11) |] = (-1));;
assert (przelewanka [| (50,1); (7,3); (78,64) |] = (-1));;
*)