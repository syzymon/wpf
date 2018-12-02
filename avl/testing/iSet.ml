type 'k set = (*drzewko*)
  | Empty
  | Node of 'k set * 'k * 'k set * int * int64 
  (*lewe poddrzewo, przedzial w wierzcholku, prawe poddrzewo, czynnik balansu,
  suma rozmiarow przedzialow w poddrzewie - trzymane jako liczba ujemna ze wzgledu na zakres*)

type t = (*typ calego zbioru z komparatorem*)
{
  cmp : (int * int) -> (int * int) -> int;
  set : (int * int) set;
}

let height = function
  | Node (_, _, _, h, _) -> h
  | Empty -> 0

let size = function (*calkowita licznosc sumy przedzialow w poddrzewie*)
  | Node (_, _, _, _, s) -> s
  | Empty -> Int64.zero

let len (a, b) = (*zwraca liczbe przeciwna do dlugosci przedzialu (a,b)*)
  let nA = Int64.of_int(a) and nB = Int64.of_int(b) in
  Int64.pred (Int64.sub nA nB) (*-(b - a + 1) = (a - b - 1)*)

let make l k r = Node (l, k, r, max (height l) (height r) + 1,
   Int64.add (Int64.add (size l) (size r)) (len k)) (*zalozenia: l, r to poprawne drzewa o roznicy <= 2,
zachodzi l < k < r, robi z nich avl*)

let bal l k r = (*bierze drzewko, zwraca poprawnie zbalansowane, roznica glebokosci na wejsciu <= 3*)
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

let rec min_elt = function 
  | Node (Empty, k, _, _, _) -> k
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> raise Not_found

let rec remove_min_elt = function
  | Node (Empty, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "PSet.remove_min_elt"

let merge t1 t2 = (*bierze 2 drzewa, klucze e1 < klucze e2, zwraca zbalansowane avl z nich*)
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let k = min_elt t2 in
      bal t1 k (remove_min_elt t2)

let create cmp = { cmp = cmp; set = Empty }
let empty = { cmp = compare; set = Empty }


let is_empty x = 
  x.set = Empty

(*warunek przed add_one: dany zbior ma PUSTE przecięcie z przedzialem x*)
let rec add_one cmp x = function (*zwraca funkcje ktora bierze drzewo i zwraca drzewo z x-em, zbalansowane*)
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

let rec join cmp l v r = (*bierze 2 drzewa, klucze l < v < klucze r, zwraca avl zawierające l, r oraz element v*)
  match (l, r) with
  | (Empty, _) -> add_one cmp v r
  | (_, Empty) -> add_one cmp v l
  | (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
      if lh > rh + 2 then bal ll lv (join cmp lr v r) else
      if rh > lh + 2 then bal (join cmp l v rl) rv rr else
      make l v r


let split x { cmp = cmp; set = set } = 
  let inside x (l, r) = (x >= l) && (x <= r) in
  let rec loop x = function (*bierze liczbe x i drzewo, zwraca (lewe, czy, prawe) gdzie lewe < x < prawe oraz czy: czy x wystepuje*)
      Empty ->
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

let remove x { cmp = cmp; set = set } =
  let (l, _, _) = split (fst x) { cmp = cmp; set = set } in 
  let (_, _, r) = split (snd x) { cmp = cmp; set = set } in
  { cmp = cmp; set = (merge l.set r.set) }


let add x { cmp = cmp; set = set } = (*bierze seta i zwraca seta z dodanym x*)
  { cmp = cmp; set = add_one cmp x (remove x { cmp = cmp; set = set }).set}

let below x { cmp = cmp; set = set } = 
  let (l, _, _) = if x < max_int then split (x + 1) { cmp = cmp; set = set } else
   ({ cmp = cmp; set = set }, false, empty) in (*case gdy x = max_int i nie mozna dodac 1*) 
  let cnt = size (l.set) in 
    if (Int64.neg (Int64.of_int(max_int)) < cnt) then Int64.to_int(Int64.neg cnt) 
    else max_int

let mem x { cmp = cmp; set = set } = 
  let (_, ans, _) = split x { cmp = cmp; set = set } in ans

let exists = mem

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

let iter f { set = set } =
  List.iter f (elements { cmp = compare; set = set })

let fold f { cmp = cmp; set = set } acc =
  let remap f a b  = f b a in
  List.fold_left (remap f) acc (elements { cmp = cmp; set = set })