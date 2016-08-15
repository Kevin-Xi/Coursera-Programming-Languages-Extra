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
