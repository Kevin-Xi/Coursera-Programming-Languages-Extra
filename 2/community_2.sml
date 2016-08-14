(* 38 Cons Cells *)
fun length_of_a_list lst =
  let
      fun aux (l, acc) =
	case l of
	    [] => acc
	  | _ :: l' => aux(l', acc + 1)
  in
      aux(lst, 0)
  end
