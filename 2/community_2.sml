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
