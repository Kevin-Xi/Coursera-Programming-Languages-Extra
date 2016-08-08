(* 1 *)
fun alternate (lst: int list) =
  if null lst
  then 0
  else hd lst - alternate(tl lst)

(* 2 *)
fun min_max (lst: int list) =
  if null (tl lst)
  then (hd lst, hd lst)
  else
      let val min_max_of_rest = min_max(tl lst)
      in
	  if hd lst < #1 min_max_of_rest
	  then (hd lst, #2 min_max_of_rest)
	  else if hd lst > #2 min_max_of_rest
	  then (#1 min_max_of_rest, hd lst)
	  else min_max_of_rest
      end

(* 3 *)
fun cumsum (lst: int list) =
  if null (tl lst)
  then lst
  else
      let val rest = tl lst
      in hd lst :: cumsum ((hd lst + hd rest) :: tl rest)
      end

(* 4 *)
fun greeting (who: string option) =
  let
      val name = if isSome who
		 then valOf who
		 else "you"
  in
      "Hello there, " ^ name ^ "!"
  end

(* 5 *)
fun repeat (elems: int list, times: int list) =
  if null elems orelse null times
  then []
  else if hd times = 0
  then repeat(tl elems, tl times)
  else hd elems :: repeat(elems, (hd times - 1) :: (tl times))

(* 6 *)
fun addOpt (a1: int option, a2: int option) =
  if isSome a1 andalso isSome a2
  then SOME (valOf a1 + valOf a2)
  else NONE

(* 7 *)
fun addAllOpt (lst: (int option) list) =
  if null lst
  then NONE
  else
      let val rest_opt = addAllOpt(tl lst)
      in
	  if isSome (hd lst)
	  then
	      if isSome rest_opt
	      then SOME (valOf (hd lst) + valOf rest_opt)
	      else hd lst
	  else rest_opt
      end
