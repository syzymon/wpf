(*Autor: Szymon Tworkowski 406386 Reviewer: Damian Werpachowski 407214*)

let debug lst = List.iter (fun x -> print_int x; print_string("  ")) lst;
print_newline(); print_newline();
;;


let rec nwd a b =
  if b = 0 then a else nwd b (a mod b);;

let przelewanka szkl = 
  let n = Array.length szkl in
  let gcd = Array.fold_left (fun x y -> nwd x (fst y)) 0 szkl in
  if ((Array.map (fun x -> snd x) szkl) = (Array.make n 0)) then 0 else
  if (Array.exists (fun x -> (snd x) mod gcd <> 0) szkl) || 
     (Array.for_all (fun x -> fst x = 0 || ((snd x <> fst x) && (snd x <> 0))) szkl)
  then (-1) else begin

    let mapa = Hashtbl.create 1234567 in
    let q = Queue.create() in
    let wzorzec = List.map (fun x -> snd x) (Array.to_list szkl) in
    let wyn = ref (-1) in

    let wstaw x d =
      if (Hashtbl.mem mapa x = false) then begin
        if x = wzorzec then wyn := d + 1 else begin 
          Hashtbl.add mapa x (d + 1); Queue.push x q;
        end
      end;
    in
    wstaw (Array.to_list(Array.make n 0)) (-1);
    while !wyn = (-1) do
      let u = Queue.take q in
      let tabU = Array.of_list(u) in
      let odl = Hashtbl.find mapa u in
      (*debug u;*)
      let dolej x =
        for i = 0 to n - 1 do
          x.(i) <- fst (szkl.(i));
          wstaw (Array.to_list x) odl;
          x.(i) <- tabU.(i);
        done
      in
      let wylej x =
        for i = 0 to n - 1 do
          x.(i) <- 0;
          wstaw (Array.to_list x) odl;
          x.(i) <- tabU.(i);
        done
      in
      let przelej x =
        for i = 0 to n - 1 do 
          for j = 0 to n - 1 do
            if i <> j then begin
              let zmiana = min x.(i) (fst szkl.(j) - x.(j)) in
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
