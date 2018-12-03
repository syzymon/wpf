(*  
    Autor: Szymon Tworkowski, 406386
    Review'er: Damian Werpachowski, 
*)

(*
 * ISet - Interval sets
 * Copyright (C) 1996-2018 Xavier Leroy, Nicolas Cannasse, Markus Mottl, Jacek Chrzaszcz, Przemyslaw Podlesny
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

 (*
 *  Autor najnowszej wersji programu: Przemysław Podleśny
 *  Code review: Piotr Nawrot
 *)


(*  Typ drzewa binarnego mającego przechowywać zbiór rozłącznych przedziałów.
    W węźle trzymane są:
    - wskaźnik na lewe poddrzewo
    - klucz (para (int * int) przechowująca przedział)
    - wskaźnik na prawe poddrzewo
    - wysokość drzewa zakorzenionego w tym węźle
    - minimum z łącznej liczby elementów w całym poddrzewie i max_int
    Operacje wykonywane na t bedą gwarantowały, że będzie ono zbalansowane.
    Zbalansowane - to znaczy, dla dowolnego węzła k = (l, (_, _), r, height, _)
    spełniony będzie warunenk |(height of l) - (height of r)| <= 2.           *)
type t =
  | Empty
  | Node of t * (int * int) * t * int * int

exception Non_disjoint_ranges

(*  Otrzymuje jako argument dwa rozłączne przedziały, zwraca:
    |  -1  gdy koniec pierwszego leży na lewo od początku drugiego
    |   1  w przeciwnym razie
    Jej zadaniem będzie służenie jako komparator w zbiorze
    rozłącznych przedziałów                                       *)
let range_comp (x1, y1) (x2, y2) =
  if y1 < x2 then -1
  else if x1 > y2 then 1
  else raise Non_disjoint_ranges

(*  Zbiór pusty  *)
let empty = Empty

(*  Zwraca true, gdy zbiór jest pusty  *)
let is_empty x =
  x = Empty

(*  Zwraca wysokość drzewa ukorzenionego w danym wierzchołku.
    Przyjmujemy, że wysokość drzewa pustego jest równa 0        *)
let height = function
  | Node (_, _, _, h, _) -> h
  | Empty -> 0

(*  Zwraca łączną liczbę elementów w przedziałach
    znajdujących się w drzewie                                  *)
let elem_count = function
  | Node (_, _, _, _, e) -> e
  | Empty -> 0

(*  Zwraca wynik dodawania dodatnich liczb a, b
    lub max_int, jeśli wynik ten jest od niego większy          *)
let safe_add a b =
  if max_int - b <= a (* równoważnie a+b >= max_int *)
    then max_int
  else a + b

(*  Oblicza liczbę liczb całkowitych w przedziale (a, b),
    zwraca max_int, gdy przedział jest dłuższy od max_int       *)
let in_range (a, b) =
  if a >= 0 then safe_add (b - a) 1 (* b-a nie większe od max_int *)
  else if max_int + a <= b then max_int (* równoważne b-a >= max_int *)
  else safe_add (b - a) 1

(*  Zwraca węzeł o lewym poddrzewie l i prawym poddrzewie r,
    przechowujący przedział k                                   *)
let make l k r =
  let sum_elems =
    let self = in_range k
    and sub = safe_add (elem_count l) (elem_count r) in
    safe_add self sub
  in
  Node ( l, k, r, max (height l) (height r) + 1, sum_elems )

(*  Zwraca zbalansowane drzewo o korzeniu w węźle z przedziałem k
    i poddrzewach w l, r (danych jako zbalansowane drzewa AVL).
    Różnica wysokości l i r musi być nie większa niż 3.
    Wszystkie klucze w l są mniejsze niż k, wszystkie klucze w r
    są wieksze niż k.                                           *)
let bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          (match lr with
          | Node (lrl, lrk, lrr, _, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else make l k r

(*  Zwraca najmniejszy co do wielkości przechowywanych wartości
    przedział w zbiorze lub zwraca wyjątek Not_found jeśli zbiór
    jest pusty                                                  *)
let rec min_rng = function
  | Node (Empty, k, _, _, _) -> k
  | Node (l, _, _, _, _) -> min_rng l
  | Empty -> raise Not_found

(*  Usuwa najmniejszy co do wielkości przechowywanych wartości
    przedział w zbiorze. Może to zmniejszyć głębokość drzewa,
    z którego usunięto przedział, ale o nie więcej niż 1, więc
    można je naprawić, przekazując węzeł-ojca procedurze bal.
    Zwrócone drzewo będzie zbalansowane.                        *)
let rec remove_min_rng = function
  | Node (Empty, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (remove_min_rng l) k r
  | Empty -> invalid_arg "ISet.remove_min_rng"

(*  Funkcje analogiczne dla tych powyżej, tyle że dla maksimów  *)
let rec max_rng = function
  | Node (_, k, Empty, _, _) -> k
  | Node (_, _, r, _, _) -> max_rng r
  | Empty -> raise Not_found

let rec remove_max_rng = function
  | Node (l, _, Empty, _, _) -> l
  | Node (l, k, r, _, _) -> bal l k (remove_max_rng r)
  | Empty -> invalid_arg "ISet.remove_max_rng"

exception Non_disjoint

(*  Zwraca zbiór powiększony o przedział x (rozłączny z resztą zbioru).
    Jeśli wartość jest większa, można dodać tylko w lewym poddrzewie.
    Jeśli jest mniejsza, można dodać tylko w prawym poddrzewie.
    Na koniec trzeba użyć procedury bal (mogło się okazać,
    że po dodaniu x wysokość któregoś z poddrzew wzrosła -
    ale nie więcej niż o 1 - i równowaga została zaburzona).
    Zwracany zbiór ma postać drzewa zbalansowanego.                   *)
let rec add_one x = function
  | Empty -> make Empty x Empty
  | Node (l, k, r, h, _) ->
      let c = range_comp x k in
      if c < 0 then
        let nl = add_one x l in
        bal nl k r
      else if c > 0 then
        let nr = add_one x r in
        bal l k nr
      else raise Non_disjoint

(*  Próbuje połączyć drzewa l i r w wierzchołku v według komparatora.
    Gdyby miało to spowodować niezbalansowanie drzewa wynikowego,
    próbuje połączyć v z mniejszym z poddrzew jako poddrzewo
    większego. Spełnione jest, że l i r same są zbalansowane.
    Wszystkie przedziały w l są na lewo, a w r - na prawo od v.
    Ponieważ l i r są zbalansowane, a łączenie dwóch drzew zbalansowanych
    w jednym wierzchołku daje drzewo o wysokości nie większej
    o więcej niż 1 od wysokości początkowej wyższego z drzew,
    można zastosować na koniec procedurę bal,
    otrzymując w wyniku drzewo zbalansowane.                      *)
let rec join l v r =
  match (l, r) with
  | (Empty, _) -> add_one v r
  | (_, Empty) -> add_one v l
  | (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
      if rh > lh + 2 then bal (join l v rl) rv rr else
      make l v r

(*  -1 jeśli x jest na lewo od (a, b)
     0 jeśli x należy do (a, b)
     1 jeśli x jest na prawo od (a, b)                            *)
let belong_comp x (a, b) =
  if( x < a ) then -1
  else if ( x > b ) then 1
  else 0

(*  Zwraca trójkę (l, p, r), gdzie l zawiera wszystkie elementy
    zbioru mniejsze niż x, p = true jeśli x jest w zbiorze, false
    w przeciwnym przeypadku, r zawiera wszystkie elementy zbioru
    większe niż x                                                 *)
let split x t =
  let rec loop x = function
    | Empty -> (Empty, false, Empty)
    | Node (l, v, r, _, _) ->
        let c = belong_comp x v in
        if c = 0 then (* trzeba podzielić przedział v względem x *)
          let new_l =
            if x > fst v then add_one (fst v, x-1) l
            else l
          in let new_r =
            if x < snd v then add_one (x+1, snd v) r
            else r
          in (new_l, true, new_r)
        else if c < 0 then
          let (ll, pres, rl) = loop x l in (ll, pres, join rl v r)
        else
          let (lr, pres, rr) = loop x r in (join l v lr, pres, rr)
  in loop x t


(*  true wtedy i tylko wtedy, gdy x znajduje się w zbiorze,
    w przeciwnym razie false                                  *)
let mem x t =
  let rec loop = function
    | Node (l, k, r, _, _) ->
        let c = belong_comp x k in
        c = 0 || loop (if c < 0 then l else r)
    | Empty -> false in
  loop t

let exists = mem

(*  iter stosuje funkcję f do wszystkich elementów zbioru,
    zwraca unit                                               *)
let iter f t =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _, _) -> loop l; f k; loop r in
  loop t

(*  [fold f s a] = [(f xN ... (f x2 (f x1 a))...)], gdzie
    x1, x2, ..., xN to przedziały zbioru w kolejności rosnącej
    co do wielkości przechowywanych elementów                    *)
let fold f t acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) ->
          loop (f k (loop acc l)) r in
  loop acc t

(*  Wypisuje wszystkie maksymalne (tj. niemożliwe do rozszerzenia)
    ciągłe przedziały z elementami zbioru, od najmniejszych
    do największych (według komparatora)                         *)
let elements t =
  let rec loop acc = function
    | Empty -> acc
    | Node(l, k, r, _, _) -> loop (k :: loop acc r) l in
  loop [] t

(*  Dzieli drzewo t na dwie części - jedną zawierającą elementy
    na lewo, a drugą - na prawo od końca przedziału (a, b)       *)
let split_range (a, b) t =
  let (l, _, _) = split a t
  and (_, _, r) = split b t
  in (l, r)

(*  Dzieli zbiór elementów na te leżące na lewo i te leżące na prawo
    od przedziału [a, b], sprawdza, czy jeszcze nie znajdą się
    elementy sąsiadujące z tym przedziałem i w tym wypadku powiększa
    go o te elementy, a następnie scala mniejsze poddrzewo l, większe
    poddrzewo r i przedział (teraz nazywany (left_bound, right_bound))
    w jedno drzewo. Zakładamy, że a <= b                            *)
let add (a, b) t =
  let lower, greater = split_range (a, b) t in
  let l, left_bound =
    if a >= min_int && (is_empty lower = false) then
      let max_lower as x, y = max_rng lower in
        if y + 1 = a then (remove_max_rng lower, x)
        else lower, a
    else lower, a
  in
  let right_bound, r =
    if b <= max_int && (is_empty greater = false) then
      let min_greater as x, y = min_rng greater in
        if x - 1 = b then (y, remove_min_rng greater)
        else b, greater
    else b, greater
  in join l (left_bound, right_bound) r

(*  Scala dwa zbiory, wszystkie elementy w przedziałach w t1
    są mniejsze od każdego elementu w przedziałach w t2.
    Ich wysokości nie muszą spełniać żadnych dodatkowych założeń.
    Drzewo wynikowe jest zbalansowane.                          *)
let merge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let k = min_rng t2 in
      join t1 k (remove_min_rng t2)

(*  Usuwa wszystkie elementy z przedziału (a, b) ze zbioru.
    Zakładamy a <= b                                            *)
let remove (a, b) t =
  let(lower, greater) = split_range (a, b) t
  in merge lower greater

(*  Zwraca liczbę elementów mniejszych lub równych x w drzewie
    (lub max_int, gdy liczba ta jest za duża)                   *)
let below x t =
  let (l, is_there, _) = split x t in
  let smaller = elem_count l in
  if is_there then safe_add smaller 1
  else smaller

(*
(****** TESTY *******)

(* Kilka małych testów *)
let a = empty;;
assert (below max_int a = 0);;

let a = add (1, 1) a;;
assert (below 0 a = 0);;
assert (below 1 a = 1);;
assert (below 2 a = 1);;
assert (mem 1 a);;

let a = add (3, 5) a;;
assert (mem 1 a);;
assert (mem 2 a = false);;
assert (mem 3 a);;
assert (mem 5 a);;
assert (below 5 a = 4);
assert (below 3 a = 2);;
assert (below 2 a = 1);;
assert (elements a = [(1, 1); (3, 5)]);;

(* Minimum po elementach a *)
assert (fold (fun (x, _) acc -> min x acc) a max_int = 1);;
(* Suma elementów a; zakładamy, że wszystkie są większe od 0 *)
assert (fold (fun (x, y) acc -> acc + (y*(y+1)/2 - x*(x-1)/2)) a 0 = 1+3+4+5);;

let a = add (max_int-2, max_int) a;;
assert (mem min_int a = false);;
assert (mem max_int a);;
assert (below max_int a = 7);;
assert (mem (max_int-3) a = false);;

let a = remove (max_int-10, max_int-1) a;;
assert (mem max_int a);;
assert (mem (max_int-1) a = false);;
assert (elements a = [(1, 1); (3, 5); (max_int, max_int)]);;
assert (below max_int a = 5);;

let a = add (-20, -10) a;;
assert (below (-21) a = 0);;
assert (below 0 a = 11);;
assert (mem (-10) a);;
assert (mem (-21) a = false);;

let a = add (2, 4) a;;
assert (mem 2 a);;
assert (elements a = [(-20, -10); (1, 5); (max_int, max_int)]);;
assert (below 5 a = 16);;

let a = add (5, 100) a;;
let a = remove (3, 15) a;;
assert (elements a = [(-20, -10); (1, 2); (16, 100); (max_int, max_int)]);;
assert (below 3 a = 13);;
assert (below 15 a = 13);;

let b = add (min_int, min_int) empty;;
assert (below 0 b = 1);;

let b = add (min_int, -1) b;;
assert (below 0 b = max_int);;

let b = add (0, 0) b;;
assert (below 0 b = max_int);;

let b = add (10, max_int) b;;
let b = remove (min_int, -1) b;;
assert (mem 0 b);;
assert (below max_int b = max_int - 8);;

(**** WYDAJNOŚĆ CZASOWA BEZ SPRAWDZANIA POPRAWNOŚCI ****)

(*  Wykonanie się wszystkich testów z tego zestawu
    w rozsądnym czasie pozwoli przypuścić, że program
    ma prawidłową złożoność czasową.                    *)

(*  Mając dane a, b i seed, generuje nowe a i b;
    zakładamy, że b jest resztą 7 modulo 10,
    a a - różne od 0, zakładamy, że seed jest możliwie
    losowy i nie ma żadnych lub prawie żadnych dzielników
    wspólnych z max_int; a określa początek przedziału,
    a b - długość minus 1                               *)
let new_range (a, b) seed =
  min (a*seed) (max_int-10), (b*7 mod 10)

(*  Generuje niewielkie przedziały i wrzuca je do t;
    s1: początek przedziału
    s2: koniec przedziału minus początek przedziału     *)
let rec wypelnij (s1, s2) seed t n =
  if n = 0 then t
  else
    let new_t = add (s1, s1+s2) t
    in wypelnij (new_range (s1, s2) seed) seed new_t (n-1)

let b_10k = wypelnij (1, 7) 18124781472871 empty 10000;;
let b_100k = wypelnij (1,(* Kilka małych testów *)
let a = empty;;
assert (below max_int a = 0);;

let a = add (1, 1) a;;
assert (below 0 a = 0);;
assert (below 1 a = 1);;
assert (below 2 a = 1);;
assert (mem 1 a);;

let a = add (3, 5) a;;
assert (mem 1 a);;
assert (mem 2 a = false);;
assert (mem 3 a);;
assert (mem 5 a);;
assert (below 5 a = 4);
assert (below 3 a = 2);;
assert (below 2 a = 1);;
assert (elements a = [(1, 1); (3, 5)]);;

(* Minimum po elementach a *)
assert (fold (fun (x, _) acc -> min x acc) a max_int = 1);;
(* Suma elementów a; zakładamy, że wszystkie są większe od 0 *)
assert (fold (fun (x, y) acc -> acc + (y*(y+1)/2 - x*(x-1)/2)) a 0 = 1+3+4+5);;

let a = add (max_int-2, max_int) a;;
assert (mem min_int a = false);;
assert (mem max_int a);;
assert (below max_int a = 7);;
assert (mem (max_int-3) a = false);;

let a = remove (max_int-10, max_int-1) a;;
assert (mem max_int a);;
assert (mem (max_int-1) a = false);;
assert (elements a = [(1, 1); (3, 5); (max_int, max_int)]);;
assert (below max_int a = 5);;

let a = add (-20, -10) a;;
assert (below (-21) a = 0);;
assert (below 0 a = 11);;
assert (mem (-10) a);;
assert (mem (-21) a = false);;

let a = add (2, 4) a;;
assert (mem 2 a);;
assert (elements a = [(-20, -10); (1, 5); (max_int, max_int)]);;
assert (below 5 a = 16);;

let a = add (5, 100) a;;
let a = remove (3, 15) a;;
assert (elements a = [(-20, -10); (1, 2); (16, 100); (max_int, max_int)]);;
assert (below 3 a = 13);;
assert (below 15 a = 13);;

let b = add (min_int, min_int) empty;;
assert (below 0 b = 1);;

let b = add (min_int, -1) b;;
assert (below 0 b = max_int);;

let b = add (0, 0) b;;
assert (below 0 b = max_int);;

let b = add (10, max_int) b;;
let b = remove (min_int, -1) b;;
assert (mem 0 b);;
assert (below max_int b = max_int - 8);;

(**** WYDAJNOŚĆ CZASOWA BEZ SPRAWDZANIA POPRAWNOŚCI ****)

(*  Wykonanie się wszystkich testów z tego zestawu
    w rozsądnym czasie pozwoli przypuścić, że program
    ma prawidłową złożoność czasową.                    *)

(*  Mając dane a, b i seed, generuje nowe a i b;
    zakładamy, że b jest resztą 7 modulo 10,
    a a - różne od 0, zakładamy, że seed jest możliwie
    losowy i nie ma żadnych lub prawie żadnych dzielników
    wspólnych z max_int; a określa początek przedziału,
    a b - długość minus 1                               *)
let new_range (a, b) seed =
  min (a*seed) (max_int-10), (b*7 mod 10)

(*  Generuje niewielkie przedziały i wrzuca je do t;
    s1: początek przedziału
    s2: koniec przedziału minus początek przedziału     *)
let rec wypelnij (s1, s2) seed t n =
  if n = 0 then t
  else
    let new_t = add (s1, 7) 132138213 empty 100000;;
assert (below max_int b_1(* Kilka małych testów *)
let a = empty;;
assert (below max_int a = 0);;

let a = add (1, 1) a;;
assert (below 0 a = 0);;
assert (below 1 a = 1);;
assert (below 2 a = 1);;
assert (mem 1 a);;

let a = add (3, 5) a;;
assert (mem 1 a);;
assert (mem 2 a = false);;
assert (mem 3 a);;
assert (mem 5 a);;
assert (below 5 a = 4);
assert (below 3 a = 2);;
assert (below 2 a = 1);;
assert (elements a = [(1, 1); (3, 5)]);;

(* Minimum po elementach a *)
assert (fold (fun (x, _) acc -> min x acc) a max_int = 1);;
(* Suma elementów a; zakładamy, że wszystkie są większe od 0 *)
assert (fold (fun (x, y) acc -> acc + (y*(y+1)/2 - x*(x-1)/2)) a 0 = 1+3+4+5);;

let a = add (max_int-2, max_int) a;;
assert (mem min_int a = false);;
assert (mem max_int a);;
assert (below max_int a = 7);;
assert (mem (max_int-3) a = false);;

let a = remove (max_int-10, max_int-1) a;;
assert (mem max_int a);;
assert (mem (max_int-1) a = false);;
assert (elements a = [(1, 1); (3, 5); (max_int, max_int)]);;
assert (below max_int a = 5);;

let a = add (-20, -10) a;;
assert (below (-21) a = 0);;
assert (below 0 a = 11);;
assert (mem (-10) a);;
assert (mem (-21) a = false);;

let a = add (2, 4) a;;
assert (mem 2 a);;
assert (elements a = [(-20, -10); (1, 5); (max_int, max_int)]);;
assert (below 5 a = 16);;

let a = add (5, 100) a;;
let a = remove (3, 15) a;;
assert (elements a = [(-20, -10); (1, 2); (16, 100); (max_int, max_int)]);;
assert (below 3 a = 13);;
assert (below 15 a = 13);;

let b = add (min_int, min_int) empty;;
assert (below 0 b = 1);;

let b = add (min_int, -1) b;;
assert (below 0 b = max_int);;

let b = add (0, 0) b;;
assert (below 0 b = max_int);;

let b = add (10, max_int) b;;
let b = remove (min_int, -1) b;;
assert (mem 0 b);;
assert (below max_int b = max_int - 8);;

(**** WYDAJNOŚĆ CZASOWA BEZ SPRAWDZANIA POPRAWNOŚCI ****)

(*  Wykonanie się wszystkich testów z tego zestawu
    w rozsądnym czasie pozwoli przypuścić, że program
    ma prawidłową złożoność czasową.                    *)

(*  Mając dane a, b i seed, generuje nowe a i b;
    zakładamy, że b jest resztą 7 modulo 10,
    a a - różne od 0, zakładamy, że seed jest możliwie
    losowy i nie ma żadnych lub prawie żadnych dzielników
    wspólnych z max_int; a określa początek przedziału,
    a b - długość minus 1                               *)
let new_range (a, b) seed =
  min (a*seed) (max_int-10), (b*7 mod 10)

(*  Generuje niewielkie przedziały i wrzuca je do t;
    s1: początek przedziału
    s2: koniec przedziału minus początek przedziału     *)
let rec wypelnij (s1, s2) seed t n =
  if n = 0 then t
  else
    let new_t = add (s1, 0k = 600000);; (* włożone przedziały były różne *)

(*  Generuje niewielkie p(* Kilka małych testów *)
let a = empty;;
assert (below max_int a = 0);;

let a = add (1, 1) a;;
assert (below 0 a = 0);;
assert (below 1 a = 1);;
assert (below 2 a = 1);;
assert (mem 1 a);;

let a = add (3, 5) a;;
assert (mem 1 a);;
assert (mem 2 a = false);;
assert (mem 3 a);;
assert (mem 5 a);;
assert (below 5 a = 4);
assert (below 3 a = 2);;
assert (below 2 a = 1);;
assert (elements a = [(1, 1); (3, 5)]);;

(* Minimum po elementach a *)
assert (fold (fun (x, _) acc -> min x acc) a max_int = 1);;
(* Suma elementów a; zakładamy, że wszystkie są większe od 0 *)
assert (fold (fun (x, y) acc -> acc + (y*(y+1)/2 - x*(x-1)/2)) a 0 = 1+3+4+5);;

let a = add (max_int-2, max_int) a;;
assert (mem min_int a = false);;
assert (mem max_int a);;
assert (below max_int a = 7);;
assert (mem (max_int-3) a = false);;

let a = remove (max_int-10, max_int-1) a;;
assert (mem max_int a);;
assert (mem (max_int-1) a = false);;
assert (elements a = [(1, 1); (3, 5); (max_int, max_int)]);;
assert (below max_int a = 5);;

let a = add (-20, -10) a;;
assert (below (-21) a = 0);;
assert (below 0 a = 11);;
assert (mem (-10) a);;
assert (mem (-21) a = false);;

let a = add (2, 4) a;;
assert (mem 2 a);;
assert (elements a = [(-20, -10); (1, 5); (max_int, max_int)]);;
assert (below 5 a = 16);;

let a = add (5, 100) a;;
let a = remove (3, 15) a;;
assert (elements a = [(-20, -10); (1, 2); (16, 100); (max_int, max_int)]);;
assert (below 3 a = 13);;
assert (below 15 a = 13);;

let b = add (min_int, min_int) empty;;
assert (below 0 b = 1);;

let b = add (min_int, -1) b;;
assert (below 0 b = max_int);;

let b = add (0, 0) b;;
assert (below 0 b = max_int);;

let b = add (10, max_int) b;;
let b = remove (min_int, -1) b;;
assert (mem 0 b);;
assert (below max_int b = max_int - 8);;

(**** WYDAJNOŚĆ CZASOWA BEZ SPRAWDZANIA POPRAWNOŚCI ****)

(*  Wykonanie się wszystkich testów z tego zestawu
    w rozsądnym czasie pozwoli przypuścić, że program
    ma prawidłową złożoność czasową.                    *)

(*  Mając dane a, b i seed, generuje nowe a i b;
    zakładamy, że b jest resztą 7 modulo 10,
    a a - różne od 0, zakładamy, że seed jest możliwie
    losowy i nie ma żadnych lub prawie żadnych dzielników
    wspólnych z max_int; a określa początek przedziału,
    a b - długość minus 1                               *)
let new_range (a, b) seed =
  min (a*seed) (max_int-10), (b*7 mod 10)

(*  Generuje niewielkie przedziały i wrzuca je do t;
    s1: początek przedziału
    s2: koniec przedziału minus początek przedziału     *)
let rec wypelnij (s1, s2) seed t n =
  if n = 0 then t
  else
    let new_t = add (s1, zedziały i na zmianę dodaje je
    i usuwa, dzięki czemu(* Kilka małych testów *)
let a = empty;;
assert (below max_int a = 0);;

let a = add (1, 1) a;;
assert (below 0 a = 0);;
assert (below 1 a = 1);;
assert (below 2 a = 1);;
assert (mem 1 a);;

let a = add (3, 5) a;;
assert (mem 1 a);;
assert (mem 2 a = false);;
assert (mem 3 a);;
assert (mem 5 a);;
assert (below 5 a = 4);
assert (below 3 a = 2);;
assert (below 2 a = 1);;
assert (elements a = [(1, 1); (3, 5)]);;

(* Minimum po elementach a *)
assert (fold (fun (x, _) acc -> min x acc) a max_int = 1);;
(* Suma elementów a; zakładamy, że wszystkie są większe od 0 *)
assert (fold (fun (x, y) acc -> acc + (y*(y+1)/2 - x*(x-1)/2)) a 0 = 1+3+4+5);;

let a = add (max_int-2, max_int) a;;
assert (mem min_int a = false);;
assert (mem max_int a);;
assert (below max_int a = 7);;
assert (mem (max_int-3) a = false);;

let a = remove (max_int-10, max_int-1) a;;
assert (mem max_int a);;
assert (mem (max_int-1) a = false);;
assert (elements a = [(1, 1); (3, 5); (max_int, max_int)]);;
assert (below max_int a = 5);;

let a = add (-20, -10) a;;
assert (below (-21) a = 0);;
assert (below 0 a = 11);;
assert (mem (-10) a);;
assert (mem (-21) a = false);;

let a = add (2, 4) a;;
assert (mem 2 a);;
assert (elements a = [(-20, -10); (1, 5); (max_int, max_int)]);;
assert (below 5 a = 16);;

let a = add (5, 100) a;;
let a = remove (3, 15) a;;
assert (elements a = [(-20, -10); (1, 2); (16, 100); (max_int, max_int)]);;
assert (below 3 a = 13);;
assert (below 15 a = 13);;

let b = add (min_int, min_int) empty;;
assert (below 0 b = 1);;

let b = add (min_int, -1) b;;
assert (below 0 b = max_int);;

let b = add (0, 0) b;;
assert (below 0 b = max_int);;

let b = add (10, max_int) b;;
let b = remove (min_int, -1) b;;
assert (mem 0 b);;
assert (below max_int b = max_int - 8);;

(**** WYDAJNOŚĆ CZASOWA BEZ SPRAWDZANIA POPRAWNOŚCI ****)

(*  Wykonanie się wszystkich testów z tego zestawu
    w rozsądnym czasie pozwoli przypuścić, że program
    ma prawidłową złożoność czasową.                    *)

(*  Mając dane a, b i seed, generuje nowe a i b;
    zakładamy, że b jest resztą 7 modulo 10,
    a a - różne od 0, zakładamy, że seed jest możliwie
    losowy i nie ma żadnych lub prawie żadnych dzielników
    wspólnych z max_int; a określa początek przedziału,
    a b - długość minus 1                               *)
let new_range (a, b) seed =
  min (a*seed) (max_int-10), (b*7 mod 10)

(*  Generuje niewielkie przedziały i wrzuca je do t;
    s1: początek przedziału
    s2: koniec przedziału minus początek przedziału     *)
let rec wypelnij (s1, s2) seed t n =
  if n = 0 then t
  else
    let new_t = add (s1, rozmiar struktury pozostaje
    w przybliżeniu stały,(* Kilka małych testów *)
let a = empty;;
assert (below max_int a = 0);;

let a = add (1, 1) a;;
assert (below 0 a = 0);;
assert (below 1 a = 1);;
assert (below 2 a = 1);;
assert (mem 1 a);;

let a = add (3, 5) a;;
assert (mem 1 a);;
assert (mem 2 a = false);;
assert (mem 3 a);;
assert (mem 5 a);;
assert (below 5 a = 4);
assert (below 3 a = 2);;
assert (below 2 a = 1);;
assert (elements a = [(1, 1); (3, 5)]);;

(* Minimum po elementach a *)
assert (fold (fun (x, _) acc -> min x acc) a max_int = 1);;
(* Suma elementów a; zakładamy, że wszystkie są większe od 0 *)
assert (fold (fun (x, y) acc -> acc + (y*(y+1)/2 - x*(x-1)/2)) a 0 = 1+3+4+5);;

let a = add (max_int-2, max_int) a;;
assert (mem min_int a = false);;
assert (mem max_int a);;
assert (below max_int a = 7);;
assert (mem (max_int-3) a = false);;

let a = remove (max_int-10, max_int-1) a;;
assert (mem max_int a);;
assert (mem (max_int-1) a = false);;
assert (elements a = [(1, 1); (3, 5); (max_int, max_int)]);;
assert (below max_int a = 5);;

let a = add (-20, -10) a;;
assert (below (-21) a = 0);;
assert (below 0 a = 11);;
assert (mem (-10) a);;
assert (mem (-21) a = false);;

let a = add (2, 4) a;;
assert (mem 2 a);;
assert (elements a = [(-20, -10); (1, 5); (max_int, max_int)]);;
assert (below 5 a = 16);;

let a = add (5, 100) a;;
let a = remove (3, 15) a;;
assert (elements a = [(-20, -10); (1, 2); (16, 100); (max_int, max_int)]);;
assert (below 3 a = 13);;
assert (below 15 a = 13);;

let b = add (min_int, min_int) empty;;
assert (below 0 b = 1);;

let b = add (min_int, -1) b;;
assert (below 0 b = max_int);;

let b = add (0, 0) b;;
assert (below 0 b = max_int);;

let b = add (10, max_int) b;;
let b = remove (min_int, -1) b;;
assert (mem 0 b);;
assert (below max_int b = max_int - 8);;

(**** WYDAJNOŚĆ CZASOWA BEZ SPRAWDZANIA POPRAWNOŚCI ****)

(*  Wykonanie się wszystkich testów z tego zestawu
    w rozsądnym czasie pozwoli przypuścić, że program
    ma prawidłową złożoność czasową.                    *)

(*  Mając dane a, b i seed, generuje nowe a i b;
    zakładamy, że b jest resztą 7 modulo 10,
    a a - różne od 0, zakładamy, że seed jest możliwie
    losowy i nie ma żadnych lub prawie żadnych dzielników
    wspólnych z max_int; a określa początek przedziału,
    a b - długość minus 1                               *)
let new_range (a, b) seed =
  min (a*seed) (max_int-10), (b*7 mod 10)

(*  Generuje niewielkie przedziały i wrzuca je do t;
    s1: początek przedziału
    s2: koniec przedziału minus początek przedziału     *)
let rec wypelnij (s1, s2) seed t n =
  if n = 0 then t
  else
    let new_t = add (s1, w założeniu piszącego testy
    - duży, tak aby spraw(* Kilka małych testów *)
let a = empty;;
assert (below max_int a = 0);;

let a = add (1, 1) a;;
assert (below 0 a = 0);;
assert (below 1 a = 1);;
assert (below 2 a = 1);;
assert (mem 1 a);;

let a = add (3, 5) a;;
assert (mem 1 a);;
assert (mem 2 a = false);;
assert (mem 3 a);;
assert (mem 5 a);;
assert (below 5 a = 4);
assert (below 3 a = 2);;
assert (below 2 a = 1);;
assert (elements a = [(1, 1); (3, 5)]);;

(* Minimum po elementach a *)
assert (fold (fun (x, _) acc -> min x acc) a max_int = 1);;
(* Suma elementów a; zakładamy, że wszystkie są większe od 0 *)
assert (fold (fun (x, y) acc -> acc + (y*(y+1)/2 - x*(x-1)/2)) a 0 = 1+3+4+5);;

let a = add (max_int-2, max_int) a;;
assert (mem min_int a = false);;
assert (mem max_int a);;
assert (below max_int a = 7);;
assert (mem (max_int-3) a = false);;

let a = remove (max_int-10, max_int-1) a;;
assert (mem max_int a);;
assert (mem (max_int-1) a = false);;
assert (elements a = [(1, 1); (3, 5); (max_int, max_int)]);;
assert (below max_int a = 5);;

let a = add (-20, -10) a;;
assert (below (-21) a = 0);;
assert (below 0 a = 11);;
assert (mem (-10) a);;
assert (mem (-21) a = false);;

let a = add (2, 4) a;;
assert (mem 2 a);;
assert (elements a = [(-20, -10); (1, 5); (max_int, max_int)]);;
assert (below 5 a = 16);;

let a = add (5, 100) a;;
let a = remove (3, 15) a;;
assert (elements a = [(-20, -10); (1, 2); (16, 100); (max_int, max_int)]);;
assert (below 3 a = 13);;
assert (below 15 a = 13);;

let b = add (min_int, min_int) empty;;
assert (below 0 b = 1);;

let b = add (min_int, -1) b;;
assert (below 0 b = max_int);;

let b = add (0, 0) b;;
assert (below 0 b = max_int);;

let b = add (10, max_int) b;;
let b = remove (min_int, -1) b;;
assert (mem 0 b);;
assert (below max_int b = max_int - 8);;

(**** WYDAJNOŚĆ CZASOWA BEZ SPRAWDZANIA POPRAWNOŚCI ****)

(*  Wykonanie się wszystkich testów z tego zestawu
    w rozsądnym czasie pozwoli przypuścić, że program
    ma prawidłową złożoność czasową.                    *)

(*  Mając dane a, b i seed, generuje nowe a i b;
    zakładamy, że b jest resztą 7 modulo 10,
    a a - różne od 0, zakładamy, że seed jest możliwie
    losowy i nie ma żadnych lub prawie żadnych dzielników
    wspólnych z max_int; a określa początek przedziału,
    a b - długość minus 1                               *)
let new_range (a, b) seed =
  min (a*seed) (max_int-10), (b*7 mod 10)

(*  Generuje niewielkie przedziały i wrzuca je do t;
    s1: początek przedziału
    s2: koniec przedziału minus początek przedziału     *)
let rec wypelnij (s1, s2) seed t n =
  if n = 0 then t
  else
    let new_t = add (s1, zić wydajność czasową struktury *)
let rec dodawaj_usuwaj (s(* Kilka małych testów *)
let a = empty;;
assert (below max_int a = 0);;

let a = add (1, 1) a;;
assert (below 0 a = 0);;
assert (below 1 a = 1);;
assert (below 2 a = 1);;
assert (mem 1 a);;

let a = add (3, 5) a;;
assert (mem 1 a);;
assert (mem 2 a = false);;
assert (mem 3 a);;
assert (mem 5 a);;
assert (below 5 a = 4);
assert (below 3 a = 2);;
assert (below 2 a = 1);;
assert (elements a = [(1, 1); (3, 5)]);;

(* Minimum po elementach a *)
assert (fold (fun (x, _) acc -> min x acc) a max_int = 1);;
(* Suma elementów a; zakładamy, że wszystkie są większe od 0 *)
assert (fold (fun (x, y) acc -> acc + (y*(y+1)/2 - x*(x-1)/2)) a 0 = 1+3+4+5);;

let a = add (max_int-2, max_int) a;;
assert (mem min_int a = false);;
assert (mem max_int a);;
assert (below max_int a = 7);;
assert (mem (max_int-3) a = false);;

let a = remove (max_int-10, max_int-1) a;;
assert (mem max_int a);;
assert (mem (max_int-1) a = false);;
assert (elements a = [(1, 1); (3, 5); (max_int, max_int)]);;
assert (below max_int a = 5);;

let a = add (-20, -10) a;;
assert (below (-21) a = 0);;
assert (below 0 a = 11);;
assert (mem (-10) a);;
assert (mem (-21) a = false);;

let a = add (2, 4) a;;
assert (mem 2 a);;
assert (elements a = [(-20, -10); (1, 5); (max_int, max_int)]);;
assert (below 5 a = 16);;

let a = add (5, 100) a;;
let a = remove (3, 15) a;;
assert (elements a = [(-20, -10); (1, 2); (16, 100); (max_int, max_int)]);;
assert (below 3 a = 13);;
assert (below 15 a = 13);;

let b = add (min_int, min_int) empty;;
assert (below 0 b = 1);;

let b = add (min_int, -1) b;;
assert (below 0 b = max_int);;

let b = add (0, 0) b;;
assert (below 0 b = max_int);;

let b = add (10, max_int) b;;
let b = remove (min_int, -1) b;;
assert (mem 0 b);;
assert (below max_int b = max_int - 8);;

(**** WYDAJNOŚĆ CZASOWA BEZ SPRAWDZANIA POPRAWNOŚCI ****)

(*  Wykonanie się wszystkich testów z tego zestawu
    w rozsądnym czasie pozwoli przypuścić, że program
    ma prawidłową złożoność czasową.                    *)

(*  Mając dane a, b i seed, generuje nowe a i b;
    zakładamy, że b jest resztą 7 modulo 10,
    a a - różne od 0, zakładamy, że seed jest możliwie
    losowy i nie ma żadnych lub prawie żadnych dzielników
    wspólnych z max_int; a określa początek przedziału,
    a b - długość minus 1                               *)
let new_range (a, b) seed =
  min (a*seed) (max_int-10), (b*7 mod 10)

(*  Generuje niewielkie przedziały i wrzuca je do t;
    s1: początek przedziału
    s2: koniec przedziału minus początek przedziału     *)
let rec wypelnij (s1, s2) seed t n =
  if n = 0 then t
  else
    let new_t = add (s1, , s2) seed t n =
  if n = 0 then t
  else
    let new_t =
      if n mod 2 = 1 then(* Kilka małych testów *)
let a = empty;;
assert (below max_int a = 0);;

let a = add (1, 1) a;;
assert (below 0 a = 0);;
assert (below 1 a = 1);;
assert (below 2 a = 1);;
assert (mem 1 a);;

let a = add (3, 5) a;;
assert (mem 1 a);;
assert (mem 2 a = false);;
assert (mem 3 a);;
assert (mem 5 a);;
assert (below 5 a = 4);
assert (below 3 a = 2);;
assert (below 2 a = 1);;
assert (elements a = [(1, 1); (3, 5)]);;

(* Minimum po elementach a *)
assert (fold (fun (x, _) acc -> min x acc) a max_int = 1);;
(* Suma elementów a; zakładamy, że wszystkie są większe od 0 *)
assert (fold (fun (x, y) acc -> acc + (y*(y+1)/2 - x*(x-1)/2)) a 0 = 1+3+4+5);;

let a = add (max_int-2, max_int) a;;
assert (mem min_int a = false);;
assert (mem max_int a);;
assert (below max_int a = 7);;
assert (mem (max_int-3) a = false);;

let a = remove (max_int-10, max_int-1) a;;
assert (mem max_int a);;
assert (mem (max_int-1) a = false);;
assert (elements a = [(1, 1); (3, 5); (max_int, max_int)]);;
assert (below max_int a = 5);;

let a = add (-20, -10) a;;
assert (below (-21) a = 0);;
assert (below 0 a = 11);;
assert (mem (-10) a);;
assert (mem (-21) a = false);;

let a = add (2, 4) a;;
assert (mem 2 a);;
assert (elements a = [(-20, -10); (1, 5); (max_int, max_int)]);;
assert (below 5 a = 16);;

let a = add (5, 100) a;;
let a = remove (3, 15) a;;
assert (elements a = [(-20, -10); (1, 2); (16, 100); (max_int, max_int)]);;
assert (below 3 a = 13);;
assert (below 15 a = 13);;

let b = add (min_int, min_int) empty;;
assert (below 0 b = 1);;

let b = add (min_int, -1) b;;
assert (below 0 b = max_int);;

let b = add (0, 0) b;;
assert (below 0 b = max_int);;

let b = add (10, max_int) b;;
let b = remove (min_int, -1) b;;
assert (mem 0 b);;
assert (below max_int b = max_int - 8);;

(**** WYDAJNOŚĆ CZASOWA BEZ SPRAWDZANIA POPRAWNOŚCI ****)

(*  Wykonanie się wszystkich testów z tego zestawu
    w rozsądnym czasie pozwoli przypuścić, że program
    ma prawidłową złożoność czasową.                    *)

(*  Mając dane a, b i seed, generuje nowe a i b;
    zakładamy, że b jest resztą 7 modulo 10,
    a a - różne od 0, zakładamy, że seed jest możliwie
    losowy i nie ma żadnych lub prawie żadnych dzielników
    wspólnych z max_int; a określa początek przedziału,
    a b - długość minus 1                               *)
let new_range (a, b) seed =
  min (a*seed) (max_int-10), (b*7 mod 10)

(*  Generuje niewielkie przedziały i wrzuca je do t;
    s1: początek przedziału
    s2: koniec przedziału minus początek przedziału     *)
let rec wypelnij (s1, s2) seed t n =
  if n = 0 then t
  else
    let new_t = add (s1, remove (s1, s1+s2) t
      else add (s1, s1+s2) t
    in dodawaj_usuwaj (new_range (s1, s2) seed) seed new_t (n-1);;

let c_100k = dodawaj_usuwaj (1, 7) 1312412442211 b_100k 100000;;

(*  Wykonuje fold prostą funkcją po zbiorze z wieloma elementami  *)
let d_100k = fold (fun (a, b) acc -> min a acc) b_100k max_int;;
let elem_100k = elements b_100k;;

let b_1mln = wypelnij (1, 7) 2124213 empty 1000000;;
assert (below max_int b_1mln = 6000000);; (* Włożone przedziały były różne *)
let c_1mln = dodawaj_usuwaj (1, 7) 1312412442211 b_1mln 1000000;;
let d_1mln = fold (fun (a, b) acc -> min a acc) b_1mln max_int;;
let elem_1mln = elements b_1mln;;

iter (fun x -> ()) b_1mln;;

(*  Sprawdzamy, czy policzenie elementów "na piechotę"
    da ten sam wynik, co below (sprawdzenie find i below);  *)
let b_100k = remove (min_int, min_int) b_100k;;
let b_1mln = remove (min_int, min_int) b_1mln;;
let add_range_length = fun (a, b) acc -> acc + (b - (a-1));;
assert (fold add_range_length b_100k 0 = below max_int b_100k);;
assert (fold add_range_length b_1mln 0 = below max_int b_1mln);;
*)
