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
