use "extra_1.sml";

(* 1 *)
val t1_1 = alternate [1,2,3,4] = ~2;
val t1_2 = alternate [1,2,3] = 2;
val t1_3 = alternate [1] = 1;
val t1_4 = alternate [] = 0;
