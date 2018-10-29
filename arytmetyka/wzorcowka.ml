type wartosc = float * float list
let validate w =
	let rec merge_subsets w =
		match w with
			| [] -> []
			| (a, b)::t -> if a < 0. && 0. < b then merge_subsets ((a,0.)::(0.,b)::t) else
		    match merge_subsets t with
				| [] -> [(a, b)]
				| ((c, d)::inner_t as merged_tail) ->
					if b < c || (b=0. && c=0.) then (a, b)::merged_tail
					else merge_subsets ((a, max b d)::inner_t)
	in merge_subsets (List.sort compare w)
let apply_and_validate f wx wy =
    let rec loop inner_wx acc =
        let rec inner_loop x inner_wy acc =
            match inner_wy with
                | [] -> acc
                | h::t -> inner_loop x t ((f x h)::acc)
        in match inner_wx with
            | [] -> acc
            | h::t -> loop t (inner_loop h wy acc)
    in validate (loop wx [])
let wartosc_od_do x y = validate [(x, y)]
let wartosc_dokladnosc x p = wartosc_od_do (x*.(1.0 +. p /. 100.)) (x*.(1.0 -. p /. 100.))
let wartosc_dokladna x = wartosc_od_do x x
let rec in_wartosc w x =
	match w with
		| [] -> false
		| (a, b)::t -> if a<=x && x<=b then true
			else in_wartosc t x
let rec min_wartosc w = match w with | [] -> nan | (a, _)::_ -> a
let rec max_wartosc w = match w with | [] -> nan | [(_, b)] -> b | _::t -> max_wartosc t	
let sr_wartosc w =
	let srednia = ((min_wartosc w) +. (max_wartosc w))/.2. in
  if srednia = infinity || srednia = neg_infinity then nan else srednia
let plus wx wy =
	let plus_basic (a, b) (c, d) = (a+.c, b+.d)
	in apply_and_validate plus_basic wx wy
let minus wx wy =
	let minus_basic (a, b) (c, d) = (a-.d, b-.c)
	in apply_and_validate minus_basic wx wy
let razy_basic (a, b) (c, d) = if a >= 0. && c >= 0. then (a*.c, b*.d)   
let razy wx wy = apply_and_validate razy_basic wx wy
let podzielic wx wy =
	let podzielic_basic x (a, b) = if a < 0. && b = 0. then razy_basic x (neg_infinity, 1./.a)
		else if a = 0. && b > 0. then razy_basic x (1./.b, infinity)
		else if a = 0. && b = 0. then razy_basic x (neg_infinity, infinity)
		else razy_basic x (1./.b, 1./.a)
in apply_and_validate podzielic_basic wx wy