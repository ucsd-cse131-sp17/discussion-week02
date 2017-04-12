open OUnit2
open Week02

let id x = x

let t_string name value expected = name>::
  (fun ctxt -> assert_equal expected value ~printer:id)

let t_int name value expected = name>::
  (fun ctxt -> assert_equal expected value ~printer:string_of_int)

let string_of_a_list (printer: 'a -> string) (l: 'a list) : string = 
  let ss    = List.map printer l in
  let inner = String.concat ", " ss in
  "[" ^ inner ^ "]"

let t_int_list name value expected = name>::
  (fun ctxt -> assert_equal expected value ~printer:(string_of_a_list string_of_int))

let rec peano_of_int (n: int) : peano =
  if n = 0
  then Zero
  else Succ (peano_of_int (n-1))

let rec int_of_peano (p: peano) : int =
  match p with
  | Zero -> 0
  | Succ p' -> (int_of_peano p') + 1

let string_of_peano (p: peano) : string = 
  "(Peano " ^ string_of_int (int_of_peano p) ^ ")"

let t_peano name value expected = name>::
  (fun ctxt -> assert_equal expected value ~printer:(string_of_peano))

let string_of_hashmap (kp: 'k -> string) (vp: 'v -> string) (m: ('k,'v) hashmap) : string =
  let rec helper m =
    match m with
    | Nil            -> "]"
    | Entry (k,v,m') -> 
       ", " ^ (kp k) ^ " -> " ^ (vp v) ^ (helper m')
  in "[" ^
  match m with
  | Nil            -> "]"
  | Entry (k,v,m') -> (kp k) ^ " -> " ^ (vp v) ^ (helper m')
  
type hm = (string, int) hashmap

let hm1 : hm = put Nil "foo" 1 ;;
let hm2 : hm = put hm1 "bar" 2 ;;
let hm3 : hm = put hm1 "bar" 3 ;;
            

let suite =
"suite">:::
 [
 t_int_list "qs1" (quicksort []) [];
 t_int_list "qs2" (quicksort [1]) [1];
 t_int_list "qs3" (quicksort [3;4;2;1;5]) [1;2;3;4;5];
 t_int_list "qs4" (quicksort [1;2;3;4;5]) [1;2;3;4;5];
 
 t_peano "peano_add1" (add (peano_of_int 0) (peano_of_int 3)) (peano_of_int 3);
 t_peano "peano_add2" (add (peano_of_int 4) (peano_of_int 0)) (peano_of_int 4);
 t_peano "peano_add3" (add (peano_of_int 4) (peano_of_int 3)) (peano_of_int 7);

 t_peano "peano_mul1" (mult (peano_of_int 0) (peano_of_int 3)) (peano_of_int 0);
 t_peano "peano_mul2" (mult (peano_of_int 4) (peano_of_int 0)) (peano_of_int 0);
 t_peano "peano_mul3" (mult (peano_of_int 1) (peano_of_int 3)) (peano_of_int 3);
 t_peano "peano_mul4" (mult (peano_of_int 4) (peano_of_int 1)) (peano_of_int 4);
 t_peano "peano_mul5" (mult (peano_of_int 4) (peano_of_int 3)) (peano_of_int 12);
 
 t_int "hashmap_1" (get hm3 "bar") 3;
 t_int "hashmap_2" (get hm2 "bar") 2;
 t_int "hashmap_3" (get hm3 "foo") 1;
 t_int "hashmap_4" (get hm2 "foo") 1;
 ]
;;

run_test_tt_main suite
