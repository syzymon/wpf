(*Autor: Szymon Tworkowski 406386 Reviewer: Damian Werpachowski 407214*)

(*
 * ISet - Interval sets
 * Copyright (C) 1996-2018 Xavier Leroy, Nicolas Cannasse, Markus Mottl, Jacek Chrzaszcz, Szymon Tworkowski
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
Drzewo AVL: dla kazdego wierzcholka roznica glebokosci lewego i prawego poddrzewa <= 2
W drzewie trzymamy kolejno:
lewe poddrzewo, przedzial w wierzcholku, prawe poddrzewo, wysokosc drzewa,
suma rozmiarow przedzialow w poddrzewie - trzymane jako liczba ujemna ze wzgledu na zakres
poniewaz -(max_int - min_int + 1) >= Int64.min_int 

Zakladamy, ze kazdy wierzcholek drzewa zawiera jeden przedzial, oraz ze przedzialy 
w dowolnym poddrzewie sa rozlaczne, ale moga sie stykac np. [1,2] oraz [3,4] 
moga byc przedzialami roznych wierzcholkow ale [1,3] oraz [2,4] juz nie

Na operacje elementarne skladaja sie funkcje add_one, join oraz split. 
Ich zlozonosc wynosi O(log n). Wszystkie pozostale operacje (poza
wypisywaniem calej struktury) wykonujemy za pomoca op. elementarnych lub 
w widocznym czasie log n lub stalym.
*)
type 'k set = 
  | Empty
  | Node of 'k set * 'k * 'k set * int * int64 


type t = (*typ calego zbioru z komparatorem*)
{
  cmp : (int * int) -> (int * int) -> int;
  set : (int * int) set;
}

let height = function
  | Node (_, _, _, h, _) -> h
  | Empty -> 0

let size = function (*minus calkowita licznosc sumy przedzialow w poddrzewie*)
  | Node (_, _, _, _, s) -> s
  | Empty -> Int64.zero

let len (a, b) = (*minus liczba elementow przedzialu [a,b] w calkowitych*)
  let nA = Int64.of_int(a) and nB = Int64.of_int(b) in
  Int64.pred (Int64.sub nA nB) (*-(b - a + 1) = (a - b - 1)*)

(*zalozenia: l, r to poprawne drzewa o roznicy glebokosci <= 2,
zachodzi l < k < r*)
let make l k r = Node (l, k, r, max (height l) (height r) + 1,
   Int64.add (Int64.add (size l) (size r)) (len k))
(*tworzy z nich drzewo AVL*)

(*bierze drzewo, zwraca poprawnie zbalansowane, 
roznica glebokosci na wejsciu <= 3*)
let bal l k r = 
  let hl = height l and sl = size l in
  let hr = height r and sr = size r in
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
  else Node (l, k, r, max hl hr + 1, Int64.add (len k) (Int64.add sl sr)) 
(*zwraca poprawne drzewo o roznicy glebokosci <= 2 w kazdym poddrzewie*)

let rec min_elt = function (*zwraca element najmniejszy w niepustym drzewie*)
  | Node (Empty, k, _, _, _) -> k
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> raise Not_found

let rec remove_min_elt = function (*usuwa element najmniejszy w niepustym drzewie*)
  | Node (Empty, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "PSet.remove_min_elt"

let merge t1 t2 = (*bierze 2 drzewa, klucze t1 < klucze t2, zwraca jedno drzewo z nich
jesli wysokosci t1 i t2 roznia sie o wiecej niz 3, wynik moze NIE BYC zbalansowany!!!*)
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let k = min_elt t2 in
      bal t1 k (remove_min_elt t2)

let create cmp = { cmp = cmp; set = Empty } (*tworzy pustego seta z danym komparatorem*)
let empty = { cmp = compare; set = Empty } (*zbior pusty*)


let is_empty x = 
  x.set = Empty

(*
funkcja dodaje przedzial x do drzewa
warunek przed add_one: dany zbior ma PUSTE przecięcie z przedzialem x
zlozonosc: log n (operacja elementarna w strukturze)
*)
let rec add_one cmp x = function 
  | Node (l, k, r, h, s) ->
      let c = cmp x k in
      if c = 0 then Node (l, x, r, h, s)
      else if c < 0 then
        let nl = add_one cmp x l in
        bal nl k r
      else
        let nr = add_one cmp x r in
        bal l k nr
  | Empty -> Node (Empty, x, Empty, 1, len x)

(*
bierze 2 drzewa, klucze l < v < klucze r, zwraca avl zawierające l, r oraz element v
zlozonosc: log n (operacja elementarna w strukturze)*)
let rec join cmp l v r = 
  match (l, r) with
  | (Empty, _) -> add_one cmp v r
  | (_, Empty) -> add_one cmp v l
  | (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
      if lh > rh + 2 then bal ll lv (join cmp lr v r) else
      if rh > lh + 2 then bal (join cmp l v rl) rv rr else
      make l v r

(*bierze liczbe x i seta, zwraca (lewe, czy, prawe) gdzie lewe < x < prawe oraz czy: czy x nalezy
do zbioru oraz lewe - zbior elementow seta mniejszych od x, prawe - zbior elementow seta wiekszych od x
zlozonosc: log n (operacja elementarna w strukturze)*)
let split x { cmp = cmp; set = set } = 
  let inside x (l, r) = (x >= l) && (x <= r) in
  let rec loop x = function 
    |Empty ->
      (Empty, false, Empty)
    | Node (l, v, r, _, _) ->
      let c = cmp (x, x) v in
      if inside x v then 
        let left = if(fst v < x) then (add_one cmp (fst v, x - 1) l) else l and
            right = if(snd v > x) then (add_one cmp (x + 1, snd v) r) else r in
        (left, true, right)
      else if c < 0 then
        let (ll, pres, rl) = loop x l in (ll, pres, join cmp rl v r)
      else
        let (lr, pres, rr) = loop x r in (join cmp l v lr, pres, rr)
  in
  let setl, pres, setr = loop x set in
  { cmp = cmp; set = setl }, pres, { cmp = cmp; set = setr }

(*bierze 2 drzewa, klucze t1 < klucze t2, zwraca zbalansowane drzewo z nich,
robi to samo co funkcja merge ale dla dowolnych t1 i t2 zwroci dobre drzewo*)
let quickMerge cmp t1 t2 = 
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let k = min_elt t2 in
      join cmp t1 k (remove_min_elt t2)
  
let remove x { cmp = cmp; set = set } = (*zwraca roznice zbioru set oraz przedzialu x*)
  let (l, _, _) = split (fst x) { cmp = cmp; set = set } in 
  let (_, _, r) = split (snd x) { cmp = cmp; set = set } in
  { cmp = cmp; set = (quickMerge cmp l.set r.set) }


let add x { cmp = cmp; set = set } = (*dodaje x do zbioru*)
  { cmp = cmp; set = add_one cmp x (remove x { cmp = cmp; set = set }).set}

let below x { cmp = cmp; set = set } = (*liczba elementow zbioru nie wiekszych niz x*)
  let (l, _, _) = if x < max_int then split (x + 1) { cmp = cmp; set = set } else
   ({ cmp = cmp; set = set }, false, empty) in (*przypadek brzegowy gdy x = max_int i nie mozna dodac 1*) 
  let cnt = size (l.set) in 
    if (Int64.neg (Int64.of_int(max_int)) < cnt) then Int64.to_int(Int64.neg cnt) 
    else max_int

let mem x { cmp = cmp; set = set } = (*czy x nalezy do zbioru*)
  let (_, ans, _) = split x { cmp = cmp; set = set } in ans

let exists = mem

(*lista elementow zbioru w kolejnosci inorder: zlozonosc O(n) gdzie n to rozmiar seta*)
let elements { set = set } = 
  let rec loop acc = function
    | Empty -> acc
    | Node(l, k, r, _, _) -> 
      let to_add = loop acc r in
      match to_add with
      | [] -> loop [k] l
      | h :: t -> if (snd k) + 1 < fst h then loop (k :: to_add) l
        else  loop ((fst k, snd h) :: t) l
    in
  loop [] set

(*wykonuje f na kazdym elemencie zbioru w kolejnosci inorder*)
let iter f { set = set } =
  List.iter f (elements { cmp = compare; set = set })

(*sklada funkcje f na przedzialach ze zbioru w kolejnosci inorder*)
let fold f { cmp = cmp; set = set } acc =
  let remap f a b  = f b a in
  List.fold_left (remap f) acc (elements { cmp = cmp; set = set })

(* ZBIOR TESTOW 

(* ADD/BELOW *)
let a = empty
let a = add (17, 20) a
let a = add (5, 8) a
let a = add (1, 2) a
let a = add (10, 12) a
let a = add (28, 35) a
let a = add (22, 23) a
let a = add (40, 43) a
let a = add (37, 37) a;;

assert((is_empty a) = false);;
assert(mem 29 a);;
assert(mem 21 a = false);;
assert(mem 38 a = false);;
assert(mem 37 a);;
assert(below 8 a = below 9 a);;
assert(below 29 a = 17);;

(* REMOVE *)
let s = add (1, 1) (add (15, 16) (add (10, 14) (add (6, 9) empty)));;
assert((mem 10 (remove (10, 10) s)) = false);;
assert(is_empty (remove (1, 20) s));;
assert(mem 7 (remove (8, 15) s));;

(* ELEMENTS *)
assert(elements (add (1, 2) (add (3, 7) empty))
        = [(1, 7)]);;

(* FOLD *)
let s = add (1, 1) (add (11, 14) (add (6, 9) (add (4, 5) empty)));;
let func x a = x::a;;
assert(fold func s [] = [(11, 14); (4, 9); (1, 1)]);;

*)