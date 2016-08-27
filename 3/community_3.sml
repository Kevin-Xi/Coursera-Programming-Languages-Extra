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
