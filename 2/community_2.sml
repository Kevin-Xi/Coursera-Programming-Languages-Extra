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

(* === Pass/Fail === *)
type student_id = int
type grade = int (* must be in 0 to 100 range *)
type final_grade = { id : student_id, grade : grade option }
datatype pass_fail = pass | fail

(* Pass/Fail -- 1 *)
fun pass_or_fail {id, grade} =
  case grade of
      NONE => fail
    | SOME g => if g >= 75 then pass else fail

(* Pass/Fail -- 2 *)
fun has_passed g =
  pass_or_fail g = pass

(* Pass/Fail -- 3 *)
fun number_passed gs =
  let
      fun aux(gs, acc) =
	case gs of
	    [] => acc
	  | g :: gs' => if has_passed g then aux(gs', acc + 1) else aux(gs', acc)
  in
      aux(gs, 0)
  end

(* Pass/Fail -- 4 *)
fun group_by_outcome gs =
  let
      (* can pass fun as arg *)
      (* if tail call, should use @ here *)
      fun build_pass_list gs =
	case gs of
	    [] => []
	  | {id, grade} :: gs' => if has_passed {id=id, grade=grade}
			then id :: build_pass_list gs'
			else build_pass_list gs'

      fun build_fail_list gs =
	case gs of
	    [] => []
	  | {id, grade} :: gs' => if not (has_passed {id=id, grade=grade})
			then id :: build_fail_list gs'
			else build_fail_list gs'
  in
      case (build_pass_list gs, build_fail_list gs) of
	  ([], []) => []
	| (ps, []) => [(pass, ps)]
	| ([], fs) => [(fail, fs)]
	| (ps, fs) => [(pass, ps), (fail, fs)]
  end


(* === Forest For The Trees === *)
datatype 'a tree = leaf | node of { value: 'a, left: 'a tree, right: 'a tree }
datatype flag = leave_me_alone | prune_me

(* Forest For The Trees *)
fun tree_height t =
  case t of
      leaf => 0
    | node {value, left, right} => 1 + Int.max(tree_height left, tree_height right)

(* Forest For The Trees -- 2 *)
fun sum_tree t =
  case t of
      leaf => 0
   | node {value, left, right} => value + sum_tree left + sum_tree right

(* Forest For The Trees -- 3 *)
fun gardener t =
  case t of
      leaf => leaf
    | node {value, left, right} => case value of
				       leave_me_alone => node {value = value, left = gardener left, right = gardener right}
				     | prune_me => leaf

(* GCD -- Redux *)
fun gcd (a : int, b : int) =
    if a = b
    then a
    else
	if a < b
        then gcd (a, b - a)
        else gcd (a - b, b)

fun gcd_list lst =
  case lst of
      head :: nil => head
    | head :: rest => gcd(head, gcd_list rest);

(* Element Of A List -- Redux *)
fun is_divisible_by (a, b) = a mod b = 0;

fun any_divisible_by (lst, d) =
  case lst of
      [] => false
    | head :: rest => is_divisible_by(head, d) orelse any_divisible_by(rest, d);

(* Quirky Addition -- Redux *)
fun add_opt arg =
  case arg of
      (SOME a, SOME b) => SOME (a + b)
    | _ => NONE;

(* Quirky Addition -- Continued -- Redux *)
fun add_all_opt lst =
  case lst of
      [] => NONE
    | head :: rest => case (head, add_all_opt rest) of
			  (SOME h, SOME r) => SOME (h + r)
			| (NONE, SOME r) => SOME r
			| (SOME h, NONE) => SOME h
			| (NONE, NONE) => NONE;

(* Flip Flop -- Redux *)
fun alternate lst =
  case lst of
      [] => 0
    | h :: lst' => h - alternate lst';
