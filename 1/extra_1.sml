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
