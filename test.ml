open OUnit2
open Week02
open List

let t_string name value expected = name>::
  (fun ctxt -> assert_equal expected value ~printer:(fun x -> x))

let t_int name value expected = name>::
  (fun ctxt -> assert_equal expected value ~printer:string_of_int)

let string_of_int_list (l: int list) : string = 
  let rec helper2 (l: int list) : string =
  match l with
  | []  -> ""
  | h::t -> ", " ^ (string_of_int h) ^ (helper2 t)
  in
  let rec helper (l: int list) : string = 
  match l with
  | []  -> ""
  | [h] -> string_of_int h
  | h::t -> (string_of_int h) ^ (helper2 t)
  in "[" ^ (helper l) ^ "]"
;; 

let t_int_list name value expected = name>::
  (fun ctxt -> assert_equal expected value ~printer:string_of_int_list)

let suite =
"suite">:::
 [
 t_int_list "qs1" (quicksort []) [];
 t_int_list "qs2" (quicksort [1]) [1];
 t_int_list "qs3" (quicksort [3;4;2;1;5]) [1;2;3;4;5];
 t_int_list "qs4" (quicksort [1;2;3;4;5]) [1;2;3;4;5];
 ]
;;

run_test_tt_main suite
