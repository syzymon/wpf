let ( + ) = ( +. )
let ( - ) = ( -. )
let ( * ) = ( *. )
let ( / ) = ( /. )

type wartosc = { licznik : (float * float); mianownik : (float * float) };;

(*KONSTRUKTORY*)
let wartosc_dokladnosc x p = 
	{ licznik = (x - (p*x/100.), x + (p*x/100.));
	   mianownik = (1., 1.)}
	   
let wartosc_od_do x y = { licznik = (x, y); mianownik = (1., 1.) }

let wartosc_dokladna x = { licznik = (x, x); mianownik = (1., 1.) }

(*MODYFIKATORY*)

let odwrotnosc x = { licznik = x.mianownik; mianownik = x.licznik; }
let przeciwna x = (-.fst x, -.snd x)
let minek l = List.fold_left min nan l (* wyjasnic dlaczego mozna wziac nan XD *)
let maxik l = List.fold_left max nan l

let punktuj a b ( ** ) = 
	[fst a ** fst b; fst a ** snd b; snd a ** fst b; snd a ** snd b;]

let dodaj a b = (fst a +. fst b, snd a +. snd b)
let odejmij a b = (fst a -. snd b, snd a -. fst b)

let mnoz a b = 
	let lista = (punktuj a b ( * )) in
(minek lista, maxik lista)

(*let x = (mnoz (1.0, 2.0) (3.0, 7.0));;
print_float (fst x);;
print_string " ";;
print_float (snd x);;
let pom = { licznik = (1.0, 2.0); mianownik = (3.0, 7.0) }*)

let razy war1 war2 = 
	{ licznik = mnoz war1.licznik war2.licznik;
	  mianownik = mnoz war1.mianownik war2.mianownik }
	
let podzielic war1 war2 = razy war1 (odwrotnosc war2)
	 
let plus war1 war2 = 
	{ licznik = dodaj (mnoz war1.licznik war2.mianownik) (mnoz war2.licznik war1.mianownik);
	  mianownik = mnoz war1.mianownik war2.mianownik }

let minus war1 war2 = plus war1 { licznik = przeciwna war2.licznik; mianownik = war2.mianownik; }

(*SELEKTORY*)

let rec przeciecie a b = if fst a < fst b then przeciecie b a else
	if fst b <= max (snd a) (snd b) then true else false

let in_wartosc w x = przeciecie (mnoz (x, x) w.mianownik) w.licznik
	
let min_wartosc w = if ((fst (w.mianownik) < 0.) && ((snd w.mianownik) > 0.)) then neg_infinity else 
	let lista = (punktuj w.licznik w.mianownik ( / )) in (*ogarnac fold left bo nan XDDD *)
minek lista
	
let max_wartosc w = -.min_wartosc {licznik = przeciwna w.licznik; mianownik = w.mianownik}
	
let skonczone x = match (classify_float x) with
	| FP_infinite | FP_nan -> false
	| _ -> true
	
let sr_wartosc w = 
	let minimum = min_wartosc w in
	let maksimum = max_wartosc w in
	if (skonczone minimum) || (skonczone maksimum) then (minimum + maksimum) / 2.
	else nan
