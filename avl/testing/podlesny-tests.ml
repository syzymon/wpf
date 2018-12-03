       (****** TESTY *******)
    
    (* Kilka małych testów *)
    open ISet;;
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
    let b_100k = wypelnij (1, 7) 132138213 empty 100000;;

  
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
    
    
    (* Minimum po elementach a let rec wypelnij (s1, s2) seed t n =
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
    assert (below 5 a = 4);*)
    
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
    (* Suma elementów a; zakła let rec wypelnij (s1, s2) seed t n =
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
    assert (below 5 a = 4);amy, że wszystkie są większe od 0 *)
   
  (* Kilka małych testów *)
   
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
   
    
    (* Minimum po elementach a *)
    
    (* Suma elementów a; zakładamy, że wszystkie są większe od 0 *)
   
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
    
    let a = empty;;
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
    
    
    
    
    (*  Wykonuje fold prostą funkcją po zbiorze z wieloma elementami  *)
    let d_100k = fold (fun (a, b) acc -> min a acc) b_100k max_int;;
    let elem_100k = elements b_100k;;
    
    let b_1mln = wypelnij (1, 7) 2124213 empty 1000000;;
    assert (below max_int b_1mln = 6000000);; (* Włożone przedziały były różne *)
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
