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
