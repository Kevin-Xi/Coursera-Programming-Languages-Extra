(* 1 *)
fun is_positive (n: int) =
  n > 0;

(* 2 *)
fun is_divisible_by (divee: int, diver: int) =
  divee mod diver = 0;

(* 3 *)
fun divide_by (divee: int, diver: int) =
  if divee < diver
  then 0
  else 1 + divide_by(divee - diver, diver);

(* 4 *)
fun gcd (a: int, b: int) =
  if a = b
  then a
  else if a > b
  then gcd(a - b, b)
  else gcd(a, b - a);

(* 5 *)
fun lcm (a: int, b: int) =
  (a div gcd(a, b)) * b;

(* 6 *)
fun gcd_list (lst: int list) =
  if null (tl lst)
  then hd lst
  else gcd(hd lst, gcd_list(tl lst));
