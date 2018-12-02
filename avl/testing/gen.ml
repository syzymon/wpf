module PseudoSet =
    struct
        type val_ = {
            size: int;
            mutable array: bool array
        }
        type obj_ = {
            value: val_;
            add: int * int -> unit;
            remove: int * int -> unit
        }
 
        let add_ {array = arr} (a, b) =
            for i = a to b do
                arr.(i) <- true
            done
 
        let remove_ {array = arr} (a, b) =
            for i = a to b do
                arr.(i) <- false
            done
 
        let create n =
            let self = {
                size = n;
                array = Array.make n false
            } in {
                value = self;
                add = add_ self;
                remove = remove_ self
            }
    end;;
 
(* let's use global vars *)
let n = 1000;;
let clear_step = 0;;
let pset = PseudoSet.create n;;
let iSet = ref ISet.empty;;
 
type testAction =
    | TestAdd
    | TestRemove
 
let sort (x, y) =
    if x < y then (x, y) else (y, x);;
 
let get_action () : testAction =
    if Random.int 2 = 0 then
        TestAdd
    else
        TestRemove;;
 
let test_add () : unit =
    let a, b = sort (Random.int n, Random.int n) in
    PseudoSet.(pset.add (a, b));
    iSet := ISet.add (a, b) !iSet;
    Printf.printf "add (%d, %d)... " a b;;
 
let test_remove () : unit =
    let a, b = sort (Random.int n, Random.int n) in
    PseudoSet.(pset.remove (a, b));
    iSet := ISet.remove (a, b) !iSet;
    Printf.printf "remove (%d, %d)... " a b;;
 
let check_correctness () : unit =
    for i = 0 to (n - 1) do
        assert (
            PseudoSet.(pset.value.array.(i)) =
            ISet.mem i !iSet
        )
    done;
    Printf.printf "- OK!\n"; flush stdout;;
 
let _ =
    Random.self_init ();
    Printf.printf "Starting.\n"; flush stdout;
    let i = ref 0 in
    while true do
      let () =
        if clear_step > 0 && !i mod clear_step = 0 then begin
            Printf.printf "[clear]\n";
            iSet := ISet.empty;
            PseudoSet.(pset.remove(0,n-1))
        end;
        i := !i + 1;
        Printf.printf "%d. " !i;
        match get_action () with
            | TestAdd -> test_add ()
            | TestRemove -> test_remove () in
        check_correctness ()
done;;
 