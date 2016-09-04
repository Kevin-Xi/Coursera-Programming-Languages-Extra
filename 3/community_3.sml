(* There Can Be Only One *)
fun fold_map f =
  List.foldr (fn (x, acc) => f x :: acc) [];

fun fold_filter f =
  List.foldr (fn (x, acc) => if (f x) then (x::acc) else acc) [];

(* Then Evil Twin *)
fun unfold f state =
  case f state of
      NONE => []
    | SOME(next_state, elem) => elem :: (unfold f next_state);

(* A Novel Approach *)
(* The time complexity is O(n) as tail-recursive version, but the space complexity is O(n) compare to O(1) of tail-recursive version *)
val factorial = (List.foldl (op * ) 1) o (unfold (fn x => if x < 1 then NONE else SOME(x - 1, x)));

(* Unforeseen Developments *)
fun unfold_map f =
  unfold (fn xs => case xs of [] => NONE
			    | x::xs' => SOME(xs', f x));

(* So Imperative *)
fun do_until f p x =
  if p x
  then x
  else do_until f p (f x);

(* Yet Another Factorial *)(***)
fun imp_factorial n =
  #1 (do_until (fn (acc, x) => (acc * x, x - 1))
	       (fn (_, x) => x < 1)
	       (1, n));

(* Fixed Point *)
fun fixed_point f =
  do_until f
	   (fn x => f x = x);

(* Newton's Method *)
fun my_sqrt n =
  let
      fun almost_fixed_point f =
	do_until f
		 (fn x => abs(f x - x) < 0.0001)
  in
      almost_fixed_point (fn x => 0.5 * (x + n/x)) n
  end;

(* Deeper Into The Woods *)
datatype 'a tree = leaf | node of { value: 'a, left: 'a tree, right: 'a tree };

fun tree_fold f acc t =
  case t of
      leaf => acc
    | node {value, left, right} => f(tree_fold f acc left, value, tree_fold f acc right);

fun tree_unfold f state =
  case f state of
      NONE => leaf
    | SOME (l_state, v, r_state) => node {value=v, left=tree_unfold f l_state, right=tree_unfold f r_state};

(* A Grand Challenge *)
datatype expr = literal_bool | literal_int | binary_bool_op of expr * expr | binary_int_op of expr * expr | comparison of expr * expr | conditional of expr * expr * expr;

datatype expr_type = type_bool | type_int;

exception TypeError;

fun infer_type e =
  case e of
      literal_bool => type_bool
    | literal_int => type_int
    | binary_bool_op (e1, e2) => (case (infer_type e1, infer_type e2) of
				     (type_bool, type_bool) => type_bool
				   | _ => raise TypeError)
    | binary_int_op (e1, e2) => (case (infer_type e1, infer_type e2) of
				    (type_int, type_int) => type_int
				  | _ => raise TypeError)
    | comparison (e1, e2) => (case (infer_type e1, infer_type e2) of
				  (type_int, type_int) => type_bool
				| _ => raise TypeError)
    | conditional (e1, e2, e3) => (case (infer_type e1, infer_type e2, infer_type e3) of
				       (type_bool, type_bool, type_bool) => type_bool
				     | (type_bool, type_int, type_int) => type_int
				     | _ => raise TypeError);

(* GCD -- Final Redux *)
fun gcd (a: int, b: int) =
  if a=b
  then a
  else
      if a<b
      then gcd (a, b-a)
      else gcd (a-b, b);

fun gcd_list xs =
  List.foldl gcd (hd xs) (tl xs);

(* Element Of A List -- Final Redux *)
fun is_divisible_by (a: int, b: int) =
  a mod b = 0;

fun any_divisible_by (xs, d) =
  List.foldl (fn (x, acc) => acc orelse is_divisible_by(x, d)) false xs;

(* Quirky Addition *)
val add_all_opt =
  List.foldl (fn (x, acc) =>
		 case (x, acc) of
		     (SOME v, SOME av) => SOME (v + av)
		   | (NONE, acc) => acc
		   | (x, _) => x)
	     NONE;

(* Flip Flop -- Final Redux *)
(* I think it is not elegant with foldl, so just not *)

(* Minimum/Maximum -- Final Redux *)
fun min_max xs = (* can use (x::xs) as param *)
  List.foldl (fn (x, (min, max)) => (if x < min then x else min, if x > max then x else max)) (hd xs, hd xs) (tl xs);

(* Lists And Tuples, Oh My! - Final Redux *)
(*val unzip_broken =
  List.foldl (fn (x, acc) => ((#1 acc) @ (#1 x), (#2 acc) @ (#2 x))) ([], []);*)

fun unzip xs =
  List.foldr (fn ((x, y), (xs, ys)) => (x :: xs, y :: ys)) ([], []) xs;

(* Lists And Tuples, Oh My! -- Continued (1) -- Final Redux *)
(*fun zip xs =
    unfold (fn (s1, s2) => case (s1, s2) of
			       ([], _) => NONE
			     | (_, []) => NONE
			     | (_, _) => SOME ((tl s1, tl s2), (hd s1, hd s2))) xs;*)

fun zip xs =
  let
      fun helper param =
	case param of
	    (x::xs, y::ys) => SOME ((xs, ys), (x, y))
	  | _ => NONE
  in
      unfold helper xs
  end;

(* BBCA -- Final Redux *)
(* Is there a way to give unfold to auto-gen fold?
   Say. given unfold some_fun xs = ys, generate a some_fun2 that
   make fold some_fun2 ys = xs sound? *)
fun repeats_list xs =
  let
      fun helper param =
	case param of
	    (_::ss, 0::cs) => helper(ss, cs)
	  | (x::xs, c::cs) => SOME((x :: xs, c-1 :: cs), x)
	  | _ => NONE
  in
      unfold helper xs
  end;

(* 38 Cons Cells -- Final Redux *)
fun length_of_a_list xs =
    List.foldl (fn (x, acc) => acc + 1) 0 xs;
