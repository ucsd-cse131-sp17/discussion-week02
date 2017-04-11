open OUnit2
open Week02
open List

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
