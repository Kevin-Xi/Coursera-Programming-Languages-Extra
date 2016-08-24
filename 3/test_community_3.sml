(* These tests is provide by Pavel Lepin in the forum thread: *)
(* https://www.coursera.org/learn/programming-languages/discussions/weeks/4/threads/DOv7aVlWEea3rQpoWFaegw *)
(* All tests should evaluate to true. *)

(* NOTE: None of the tests in this test suite verify that your solutions *)
(* use suggested higher-order functions. *)
(* You should check that yourself! *)

use "community_3.sml";
(** Higher-Order Fun **)

(* There Can Be Only One *)

val test_fold_map_1 = fold_map (fn x => x + 1) [1, 2, 3, 4, 5] = [2, 3, 4, 5, 6]
val test_fold_map_2 = fold_map (fn x => x) [] = []
val test_fold_map_3 = fold_map not [true, false, true, false] = [false, true, false, true]
val test_fold_map_4 = fold_map (fn x => "fnord " ^ x) ["a", "quick", "brown", "fox"] =
    ["fnord a", "fnord quick", "fnord brown", "fnord fox"]

val test_fold_filter_1 = fold_filter (fn x => x mod 2 = 0) [1, 2, 3, 4, 5] = [2, 4]
val test_fold_filter_2 = fold_filter (fn x => x) [] = []
val test_fold_filter_3 = fold_filter not [true, false, true, false] = [false, false]
val test_fold_filter_4 = fold_filter (fn x => String.size x < 4) ["a", "quick", "brown", "fox"] = ["a", "fox"]

(* The Evil Twin *)
val test_unfold_1 = unfold (fn x => if x > 3 then NONE else SOME (x + 1, x)) 0 = [0, 1, 2, 3]
val test_unfold_2 = unfold (fn _ => NONE) false = []
val test_unfold_3 = unfold (fn str => if String.size str > 12 then NONE else SOME ("Banana" ^ str, str)) "" =
    ["", "Banana", "BananaBanana"]

(* A Novel Approach *)
val test_factorial_1 = factorial 4 = 24
val test_factorial_2 = factorial 0 = 1
val test_factorial_3 = factorial 5 = 120
val test_factorial_4 = factorial 7 = 5040

(* Unforeseen Developments *)
val test_unfold_map_1 = unfold_map (fn x => x + 1) [1, 2, 3, 4, 5] = [2, 3, 4, 5, 6]
val test_unfold_map_2 = unfold_map (fn x => x) [] = []
val test_unfold_map_3 = unfold_map not [true, false, true, false] = [false, true, false, true]
val test_unfold_map_4 = unfold_map (fn x => "fnord " ^ x) ["a", "quick", "brown", "fox"] =
    ["fnord a", "fnord quick", "fnord brown", "fnord fox"]
(*
(* So Imperative *)
val test_do_until_1 = do_until (fn x => x div 2) (fn x => x mod 2 <> 0) 48 = 3
val test_do_until_2 = do_until (fn x => x div 2) (fn x => x mod 2 <> 0) 9 = 9
val test_do_until_3 = do_until (fn x => x ^ " ") (fn x => String.size x > 9) "abcde" = "abcde     "

(* Yet Another Factorial *)
val test_imp_factorial_1 = imp_factorial 4 = 24
val test_imp_factorial_2 = imp_factorial 0 = 1
val test_imp_factorial_3 = imp_factorial 5 = 120
val test_imp_factorial_4 = imp_factorial 7 = 5040

(* Fixed Point *)
val test_fixed_point_1 = fixed_point (fn x => x div 2) 17 = 0
val test_fixed_point_2 = fixed_point (fn x => x) 17 = 17

(* Newton's Method *)
val test_my_sqrt_1 = abs (my_sqrt 2.0 - Math.sqrt 2.0) < 0.01
val test_my_sqrt_2 = abs (my_sqrt 9.0 - Math.sqrt 9.0) < 0.01
val test_my_sqrt_3 = abs (my_sqrt 81.0 - Math.sqrt 81.0) < 0.01
val test_my_sqrt_4 = abs (my_sqrt 10.0 - Math.sqrt 10.0) < 0.01
val test_my_sqrt_5 = abs (my_sqrt 3.0 - Math.sqrt 3.0) < 0.01
val test_my_sqrt_6 = abs (my_sqrt 0.25 - Math.sqrt 0.25) < 0.01

(* Deeper Into The Woods *)

val test_tree_fold_1 = tree_fold (fn (l, v, r) => l ^ v ^ r) "!"
    (node { value = "foo", left = node { value = "bar", left = leaf, right = leaf },
    right = node { value = "baz", left = leaf, right = leaf }}) = "!bar!foo!baz!"
val test_tree_fold_2 = tree_fold (fn (l, v, r) => l * r * v) 1 leaf = 1
val test_tree_fold_3 = tree_fold (fn (l, v, r) => v - l - r) 1
    (node { value = 10, left = node { value = 5, left = leaf, right = leaf },
    right = node { value = 3, left = leaf, right = leaf }}) = 6

val test_tree_unfold_1 = tree_unfold (fn x => if x = 0 then NONE else SOME (x - 1, x, x - 1)) 2 =
    node { value = 2, left = node { value = 1, left = leaf, right = leaf },
    right = node { value = 1, left = leaf, right = leaf }}
val test_tree_unfold_2 = tree_unfold (fn x => NONE) NONE = leaf
val test_tree_unfold_3 = tree_unfold (fn x => if x = 0 then NONE else SOME (x div 2, x, x div 3)) 6 =
    node { value = 6, left = node { value = 3, left = node { value = 1, left = leaf, right = leaf },
    right = node { value = 1, left = leaf, right = leaf } }, right = node { value = 2,
    left = node { value = 1, left = leaf, right = leaf }, right = leaf }}

(** A Grand Challenge **)
val test_infer_type_1 = infer_type (conditional (literal_bool, literal_int,
    binary_int_op (literal_int, literal_int))) = type_int
val test_infer_type_2 = infer_type literal_int = type_int
val test_infer_type_3 = infer_type literal_bool = type_bool
val test_infer_type_4 = (infer_type (conditional (literal_bool, literal_int,
    binary_int_op (literal_bool, literal_int))); false) handle TypeError => true = true
val test_infer_type_5 = (infer_type (conditional (literal_bool, literal_bool, binary_int_op
    (literal_int, literal_int))); false) handle TypeError => true = true
val test_infer_type_6 = (infer_type (conditional (literal_bool, literal_int,
    comparison (literal_int, literal_int))); false) handle TypeError => true = true
val test_infer_type_7 = infer_type (conditional (literal_bool, literal_bool,
    comparison (literal_int, literal_int))) = type_bool
val test_infer_type_8 = infer_type (conditional (literal_bool, conditional (literal_bool, literal_bool,
    comparison (literal_int, binary_int_op (literal_int, literal_int))),
    comparison (literal_int, literal_int))) = type_bool
val test_infer_type_9 = infer_type (conditional (literal_bool, binary_bool_op (literal_bool,
    comparison (literal_int, literal_int)), comparison (literal_int, literal_int))) = type_bool
val test_infer_type_10 = infer_type (binary_int_op (literal_int, literal_int)) = type_int
val test_infer_type_11 = (infer_type (binary_int_op (literal_int, literal_bool)); false)
    handle TypeError => true = true
val test_infer_type_12 = (infer_type (binary_bool_op (literal_int, literal_bool)); false)
    handle TypeError => true = true
val test_infer_type_13 = (infer_type (comparison (literal_int, literal_bool)); false)
    handle TypeError => true = true
val test_infer_type_14 = infer_type (conditional (binary_bool_op (literal_bool, literal_bool),
    literal_bool, comparison (literal_int, literal_int))) = type_bool
val test_infer_type_15 = infer_type (conditional (comparison (literal_int, literal_int),
    literal_bool, comparison (literal_int, literal_int))) = type_bool
val test_infer_type_16 = (infer_type (conditional (binary_int_op (literal_int, literal_int),
    literal_bool, comparison (literal_int, literal_int))); false) handle TypeError => true = true
*)
