use "extra_1.sml";

(* Helpers *)
fun count (from: int, to: int) =
  if from = to
  then [to]
  else if from > to
  then from :: count(from-1, to)
  else from :: count(from+1, to)

(* 1 *)
val t1_1 = alternate [1,2,3,4] = ~2;
val t1_2 = alternate [1,2,3] = 2;
val t1_3 = alternate [1] = 1;
val t1_4 = alternate [] = 0;

(* 2 *)
val t2_1 = min_max (count (1, 30)) = (1, 30);
val t2_2 = min_max (count (30, 1)) = (1, 30);
val t2_3 = min_max [1] = (1, 1);
val t2_4 = min_max [3, 9, 2, 4, 7] = (2, 9);

(* 3 *)
val t3_1 = cumsum [1, 4, 20] = [1, 5, 25];
val t3_2 = cumsum [1] = [1];
val t3_3 = cumsum [9, 3, 2] = [9, 12, 14];
val t3_4 = cumsum [2, 4] = [2, 6];
