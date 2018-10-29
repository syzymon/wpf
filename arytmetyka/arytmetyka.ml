(*Autor: Szymon Tworkowski Review'er: Damian Werpachowski*)
type wartosc = (float * float) list;; (*wartosc to posortowana lista rozlacznych, niepustych przedzialow*)

(*KONSTRUKTORY*)
let wartosc_dokladnosc x p = 
	let popraw_prz (a, b) = if(a > b) then (b, a) else (a, b) in (*zabezpieczenie przed x ujemnym*)
[popraw_prz(x *. (1. -. (p /. 100.0)), x *. (1. +. p /. 100.0))]
	   
let wartosc_od_do x y = [(x, y)]

let wartosc_dokladna x = [(x, x)]

(*ZESTAW FUNKCJI POMOCNICZYCH*)

let nalezy x a = x >= fst a && x <= snd a

let zerowy_przedzial a = (*czy przedzial ma konce w zerze*) 
	if (abs_float (fst a) = 0.) && (abs_float (snd a) = 0.) then true 
	else false 	

let spec_min x y = (*specjalne minimum dzialajace poprawnie, gdy jednym z argumentow jest nan*) 
	match (classify_float x, classify_float y) with
		| (FP_nan, _) -> y
		| (_, FP_nan) -> x
		| (_, _) -> min x y
		
let spec_max x y = -.(spec_min (-.x) (-.y)) 

let minek l = List.fold_left spec_min nan l (* jako argument mozna podac nan poniewaz funkcje specjalne go obsluguja *)

let maxik l = List.fold_left spec_max nan l

type wynik = (*typ wynikowy do operacji posrednich na pojedynczych przedzialach*)
	| Pusty 
	| Niepusty of (float * float)
	| Para of wynik * wynik (*jedyna opcja jaka zakladamy tutaj to dwa przedzialy*)
let wynNaPrzedzial wyn = match wyn with (*wywolywane tylko dla typu Niepusty*)
	| Niepusty x -> x
	| _ -> (nan, nan) (*wynik niezdefiniowany - funkcja nigdy go nie zwroci*)

let dodaj a b = Niepusty(fst a +. fst b, snd a +. snd b) 

let odejmij a b = Niepusty(fst a -. snd b, snd a -. fst b)

let punktuj a b ( ** ) = (*zamiana 2 przedzialow na 4 ekstremalne punkty po operacji '**' *)
	[fst a ** fst b; fst a ** snd b; snd a ** fst b; snd a ** snd b;]

let mnoz a b = 
	if zerowy_przedzial a || zerowy_przedzial b then (*szczegolny przypadek*)
		Niepusty(0., 0.) 
	else 
		let lista = (punktuj a b ( *. )) in (*wynik mnozenia to zawsze jeden przedzial, o koncach w punktach ekstremalnych*)
		Niepusty(minek lista, maxik lista)

let rec dziel a b = 
	if zerowy_przedzial b then Pusty (*jezeli dzielnik jest zerem, kazdy potencjalny element wyniku jest niezdefiniowany*)
	else if zerowy_przedzial a then Niepusty(0. , 0.) 
	else if not(nalezy 0. b) then mnoz a (1. /. snd b, 1. /. fst b) (*dzielenie przez jednostronny przedzial to mnozenie przez odwrotnosc*)
	else match b with 
		| (x, 0.) -> mnoz a (neg_infinity, 1. /. x) (*jezeli zawiera zero z jednej strony, to *)
		| (0., y) -> mnoz a (1. /. y, infinity)
		| (x , y) -> Para((dziel a (x, 0.)), (dziel a (0., y))) (*jesli dzielnik zawiera obustronne otoczenie zera, wynik moze byc nieciagly*)


let przeciecie a b = (max (fst a) (fst b)) <= (min(snd a) (snd b)) (*czy dwa przedzialy sie przecinaja*)

let suma_mno a b = ((min (fst a) (fst b)), (max (snd a) (snd b))) (*suma mnogosciowa przecinajacych sie przedzialow*)

let wykonaj_operacje op w1 w2 = (*zwraca wynik operacji op na wartosciach w1 oraz w2*)
	let scal lista = (*scala posortowana liste niepustych przedzialow*)
		let rec scal_pom lista aku = match lista with
			| [] -> aku
			| h :: t -> 
				if (t = [] || not(przeciecie h (List.hd t))) then scal_pom t (h :: aku) (*glowa sie nie przecina z zadnym przedzialem wiec nalezy do koncowej listy*)
				else scal_pom ((suma_mno h (List.hd t)) :: (List.tl t)) aku (*przekazujemy glowe jako informacje w nastepnym przedziale*)
		in List.rev (scal_pom lista []) (*akumulator jest odwroconym wynikiem*)
	in
	let rec iteruj1 l aku = (*iteracja po wartosciach z w1*)
		let rec iteruj2 l war1 aku = (*iteracja po w2*) 
			let wrzuc wyn acc = match wyn with (*dodaje wynik operacji na koncowa liste*)
				|Pusty -> aku
				|Niepusty a -> a :: aku 
				|Para (a, b) -> (wynNaPrzedzial a) :: (wynNaPrzedzial b) :: aku
			in
			match l with
				| [] -> aku
				| h :: t -> iteruj2 t war1 (wrzuc (op war1 h) aku)
		in 
		match l with
			| [] -> aku
			| h :: t -> iteruj1 t (iteruj2 w2 h aku)
	in
	scal (List.sort compare (iteruj1 w1 [])) (*zapewnienie zalozen reprezentacji typu wartosc*)
		
(*MODYFIKATORY*)
let plus war1 war2 = 
	wykonaj_operacje dodaj war1 war2 

let minus war1 war2 = 
	wykonaj_operacje odejmij war1 war2 

let razy war1 war2 = 
	wykonaj_operacje mnoz war1 war2 
	
let podzielic war1 war2 = 
	wykonaj_operacje dziel war1 war2 

(*SELEKTORY*)
let in_wartosc w x = 
	List.fold_left (fun a b -> (a || (nalezy x b))) false w 

let rec min_wartosc w = 
	List.fold_left (fun a b -> spec_min a (fst b)) nan w (*inf A = min (inf A_i) po każdym A_i z A*)

let rec max_wartosc w = 
	List.fold_left (fun a b -> spec_max a (snd b)) nan w (*analogicznie dla sup*)

let sr_wartosc w = 
		let skonczone x = match (classify_float x) with (*sprawdza czy liczba jest rzeczywista*)
				| FP_infinite | FP_nan -> false
				| _ -> true
			in
		let minimum = min_wartosc w in
		let maksimum = max_wartosc w in
	if (skonczone minimum) || (skonczone maksimum) then (minimum +. maksimum) /. 2.
	else nan (* zgodnie z trescia zadania - jeśli min_wartosc x i max_wartosc x nie sa skonczone *)

(*let dbg w =  (*funkcja do debugowania listy przedzialow*)
		let rozmiar = List.length w in
		print_string "[ ";
		for i = 0 to (rozmiar - 1) do 
			print_string "("; print_float (fst(List.nth w i)); print_string " , "; print_float (snd(List.nth w i)); print_string "), ";
		done;
		print_string "]\n";
;;

ZBIOR TESTOW:*)
let zero = wartosc_od_do(0.) (0.)
let jeden = wartosc_od_do(1.) (1.)
let a = wartosc_od_do (-1.) (1.)
let b = podzielic a a 
let c = razy zero b;;
assert(min_wartosc c = 0.);;

let m = wartosc_dokladnosc (-3.0) (273.3);;          (* [-11.199, 5.199]               *)
assert(min_wartosc m = -11.199);;
assert(max_wartosc m = 5.199);;
assert(sr_wartosc m = (-3.0));;
assert(in_wartosc m 0.0);;

let a = sr_wartosc ( podzielic ( plus ( wartosc_dokladna (-8.200000) ) ( wartosc_dokladnosc (-6.200000) (0.000000) ) ) ( wartosc_dokladnosc (0.000000) (8.800000) ) ) ;;
assert ((classify_float a) == FP_nan);;

let a = max_wartosc ( podzielic ( wartosc_dokladna (0.200000) ) ( wartosc_dokladna (0.000000) ) ) ;;
assert ((classify_float a) == FP_nan);;

let a = in_wartosc ( podzielic ( wartosc_od_do (-5.600000) (6.400000) ) ( wartosc_dokladnosc (-8.000000) (5.600000) ) ) (-5.200000);;
assert (a = false);;

let a = wartosc_od_do 0. 1. (* <0,1> *)
let b = podzielic a a       (* <0, inf)*)
let c = razy b zero;;       (* <0,0> *)
assert ((sr_wartosc c, max_wartosc c, min_wartosc c) = (0.,0.,0.));;

let a = podzielic jeden zero;; (* nan *)
assert (classify_float (min_wartosc a) = FP_nan);
assert (classify_float (max_wartosc a) = FP_nan);
assert (classify_float (sr_wartosc a) = FP_nan);;

assert(in_wartosc ( razy ( plus ( plus ( wartosc_dokladnosc (9.400000) (8.000000) ) ( wartosc_od_do (-9.800000) (1.000000) ) )
 ( wartosc_dokladnosc (-2.200000) (7.600000) ) ) ( wartosc_od_do (-8.000000) (0.000000) ) ) (-7.400000));;