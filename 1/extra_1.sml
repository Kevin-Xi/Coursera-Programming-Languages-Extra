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

(* 8 *)
fun any (lst: bool list) =
  not (null lst) andalso (hd lst orelse any(tl lst))

(* 9 *)
fun all (lst: bool list) =
  not (null lst) andalso (
      if null (tl lst)
      then hd lst
      else hd lst andalso all(tl lst))

(* 10 *)
fun zip (l1: int list, l2: int list) =
  if null l1 orelse null l2
  then []
  else (hd l1, hd l2) :: zip(tl l1, tl l2)

(* 11 *)
(* TODO: This approach seems less elegent *)
fun is_longer (t1: int list, t2: int list) =
  if not (null t1) andalso null t2
  then true
  else if null t1
  then false
  else is_longer(tl t1, tl t2)

fun normalize (l1: int list, l2: int list) =
  let
      val is_l1_longer = is_longer(l1, l2)
      val longer_one = if is_l1_longer then l1 else l2
      val shorter_one = if is_l1_longer then l2 else l1
      fun double_match (shorter: int list) =
	if is_longer(shorter, longer_one)
	then shorter
	else double_match(shorter @ shorter_one)
  in
      if is_l1_longer
      then (l1, double_match l2)
      else (double_match l1, l2)
  end

fun zipRecycle (l1: int list, l2: int list) =
  if null l1 orelse null l2
  then []
  else zip(normalize(l1, l2))

(* 12 *)
fun zipOpt (l1: int list, l2: int list) =
  if not (is_longer(l1, l2)) andalso not (is_longer(l2, l1))
  then SOME (zip(l1, l2))
  else NONE

(* 13 *)
fun lookup (lst: (string * int) list, key: string) =
  if null lst
  then NONE
  else
      if #1 (hd lst) = key
      then SOME (#2 (hd lst))
      else lookup (tl lst, key)

(* 14 *)
fun all_gte (lst: int list, threshold: int) =
  if null lst
  then []
  else
      if hd lst >= threshold
      then hd lst :: all_gte(tl lst, threshold)
      else all_gte(tl lst, threshold)

fun all_lt (lst: int list, threshold: int) =
  if null lst
  then []
  else
      if hd lst < threshold
      then hd lst :: all_lt(tl lst, threshold)
      else all_lt(tl lst, threshold)

fun splitup (lst: int list) =
  (all_gte (lst, 0), all_lt(lst, 0))

(* 15 *)
fun splitAt (lst: int list, threshold: int) =
  (all_gte(lst, threshold), all_lt(lst, threshold))

(* 16 *)
fun isSorted (lst: int list) =
  null lst orelse null (tl lst) orelse hd lst <= hd (tl lst) andalso isSorted(tl lst)

(* 17 *)
fun reverse (lst: int list) =
  if null lst orelse null (tl lst)
  then lst
  else (reverse(tl lst)) @ [(hd lst)]

fun isAnySorted (lst: int list) =
  isSorted(lst) orelse isSorted(reverse lst)

(* 18 *)
fun sortedMerge (l1: int list, l2: int list) =
  if null l1
  then l2
  else if null l2
  then l1
  else if hd l1 <= hd l2
  then hd l1 :: sortedMerge(tl l1, l2)
  else hd l2 :: sortedMerge(l1, tl l2)

(* 19 *)
fun qsort (lst: int list) =
  if null lst orelse null (tl lst)
  then lst
  else
      let
	  val two_parts = splitAt(tl lst, hd lst)
	  val sorted_1 = qsort(#1 two_parts)
	  val sorted_2 = qsort(#2 two_parts)
      in
	  sorted_2 @ [hd lst] @ sorted_1
      end
