open List;;
open Helper;;
open OUnit2;;

(* git page:
   https://github.com/ucsd-cse131-sp17/discussion-week02
 *)

(* OCAML:
   1. Everything is an expression
   2. Everything has a value
   3. Everything has a type
 *)

(* LISTS *)

(* 1. Nil operator [] *****************************************)

let l1 = []
;;
(* value: [] *)
(* type:  'a list *)

(* 2. Cons operator :: *****************************************)

let l2 = 1 :: [2; 3]
;;

(* value: [1;2;3] *)
(* type:  'a -> 'a list -> 'a list *)

(* 3. Append operator @ *****************************************)
(* NOTE: Can only append lists of the same type *)

let l3 = [1;2] @ [3;4;5]
;;

(* value: [1;2;3;4;5] *)
(* type:  'a list -> 'a list -> 'a list *)

(* 4. Head operator hd *****************************************)
(* NOTE: you have to import List module *)

let l4_v = hd [1;2;3;4]
;;

(* value: 1 *)
(* type:  'a list -> 'a *)

let hd' l = 
  match l with
  | []   -> error "empty"
  | h::t -> h
;;

(* ***************************************************************** *)
(* ***************************************************************** *)
(* ***************************************************************** *)

(* Tuples *)

(* Lists must have the same types, unbound number *)
(* Separator => ; *)

(* Tuples can have different types but fixed number *)
(* Separator => , *)
let t1 : (int * string) = (3, "abcd")
;;

let t2 : (int * string * (float * float)) = 
  (3, "abcd", (1.2, 3.4))
;;

let first (tup : ('a * 'b)) : 'a =
  match tup with
  | (a,b) -> a
;;

(* ***************************************************************** *)
(* ***************************************************************** *)
(* ***************************************************************** *)

(* Variables *)

let var0 = let x = 1               (* bind the value 1 to the variable x *)
        in  x + 1               (* inside this expression *)
;;

let var1 = 10
;;

let var1 = var1 * 20
;;

let var1_2 = let x = 10
             in let x = x * x
                in x + x
;;

(*
let var2 = let x = 10
           in (let z = 10 in x + z) + z
;;
*)

(* Binding by pattern-matching ***************************************)

let (var3,var4,var5) = 
  (2+3, "a" ^ "b", 1::[2])
;;
                  


(* why the following produces a warning? *)
let _ =
  let h::t = [1;2;3]
  in 0
;;

(* let _ = *)
(*   let h::t = [] *)
(*   in 0 *)

(* NEVER USE NON EXHAUSTIVE PATTERNS LIKE THIS
     let h::t = ...
   ALWAYS USE THIS FORM
     match l with ...
   AND FILL ALL THE CASES
 *)

(* ;; *)

(* ***************************************************************** *)
(* ***************************************************************** *)
(* ***************************************************************** *)

(* Functions *)

(* Functions is a value *)

let f1 = fun x -> x + 1
;;

(* you can also use the keyword "function" instead of "fun"
   which allows pattern matching: 
*)
let f2 = function | []   -> error "empty"
                  | h::t -> h
;;

(* You can use C-like arguments, which is basically a tuple *)
let f3 = fun (x,y) -> x < y
;;

(* Or, you can write "curried" functions *)
let f4  = fun x -> fun y -> x < y
;;

(* the function above can be re-written as the following
  f4 = fun x -> (fun y -> x < y)
  which makes it easier the see that the the of f4 is
  'a -> ('a -> bool)
  and hence f4 actually returns a function !
*)

(* By the way, the following two expressions are the same
   e1 e2 e3 e4 = ((e1 e2) e3) e4

   Meanwhile the following type signatures are the same
   'a -> 'b -> 'c -> 'd = 'a -> ('b -> ('c -> 'd))
 *)

(* You can write curried functions shorter like the following *)
let f4' = fun x y -> x < y
;;

(* A function can also take a function argument *)
let f5 = fun f x -> not (f x)
;;

(* what is the type of the following expression ? *)
(* let ff = fun f -> fun x -> (f x) + x *)
(* ;; *)

(* ***************************************************************** *)

(* Examples: *)

let rec filter (pred: 'a -> bool) (l: 'a list) : 'a list =
  match l with
  | [] -> []
  | h::t -> if pred h
            then h :: (filter pred t)
            else      (filter pred t)
;;

let partition (pred: 'a -> bool) (l: 'a list) : ('a list * 'a list) = 
  (filter pred l, filter (fun x -> not (pred x)) l)
;;

let rec quicksort (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | h::t -> let (lhs,rhs) = partition (fun x -> x < h) t in
            let (lhs',rhs') = (quicksort lhs, quicksort rhs) in
            lhs' @ [h] @ rhs'
;;

(* ***************************************************************** *)
(* ***************************************************************** *)
(* ***************************************************************** *)

(* Data types *)

type peano = 
  | Zero                        (* 0 *)
  | Succ of peano               (* p + 1 *)
;;

let rec add (x: 'peano) (y: 'peano) : 'peano =
  match x with
  | Zero    -> y
  | Succ x' -> add x' (Succ y)
;;

(* p1 = 3 + 2 *)
let p1 = add (Succ (Succ (Succ Zero))) (Succ (Succ Zero))
;;

let rec mult (x: 'peano) (y: 'peano) : 'peano =
  match x with
  | Zero      -> Zero           (* x = 0 *)
  | Succ Zero -> y              (* x = 1 *)
  | Succ x'   -> add y (mult x' y)
;;

(* p2 = 3 * 2 *)
let p2 = mult (Succ (Succ (Succ Zero))) (Succ (Succ Zero))
;;

type ('k,'v) hashmap =
  | Nil
  | Entry of 'k * 'v * ('k,'v) hashmap
;;
  
let rec get (m: ('k, 'v) hashmap) (key: 'k) : 'v =
  match m with
  | Nil            -> error "key not found"
  | Entry (k,v,m') -> if   k = key
                      then v
                      else get m' key
;;

let rec put (m: ('k, 'v) hashmap) (key: 'k) (value: 'v) : ('k,'v) hashmap =
  match m with
  | Nil            -> Entry (key, value, Nil)
  | Entry (k,v,m') -> if k = key
                      then Entry (k, value, m')
                      else Entry (k, v, put m' key value)
;;
