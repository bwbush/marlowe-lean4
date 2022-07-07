

import Marlowe.Language
import Marlowe.Primitives
import Marlowe.Semantics


namespace Marlowe.Proof


open Marlowe.Language.Contract
open Marlowe.Language.State
open Marlowe.Primitives (Integer integer)
open Marlowe.Semantics (evaluate observe)


variable (e : Environment)
variable (s : State)

variable (x y : Value)
variable (x' y' : Int)

variable (hx : evaluate e s x = x')
variable (hy : evaluate e s y = y')

variable (a : Observation)
variable (a' : Bool)

variable (ha : observe e s a = a')

variable (c : ChoiceIdT)


-- theorem evaluate_money


theorem evaluate_constant (h : x' = x''.toInt) : evaluate e s (Constant x'') = x' :=
  by simp [evaluate, h]

#check evaluate_constant


theorem evaluate_neg :  evaluate e s (NegValue x) = - x' :=
  by simp [evaluate, hx]

#check evaluate_neg


theorem evaluate_add : evaluate e s (AddValue x y) = x' + y' :=
  by simp [evaluate, hx, hy]

#check evaluate_add


theorem evaluate_sub : evaluate e s (SubValue x y) = x' - y' :=
  by simp [evaluate, hx, hy]

#check evaluate_sub


theorem evaluate_mul : evaluate e s (MulValue x y) = x' * y' :=
  by simp [evaluate, hx, hy]

#check evaluate_mul


private def check_div (m n k : Int) : Prop :=
  evaluate e s (DivValue (Constant $ integer m) (Constant $ integer n)) =  k

example : check_div e s    0    0    0  := by simp [evaluate, check_div]
example : check_div e s    0    6    0  := by simp [evaluate, check_div]
example : check_div e s   12    0    0  := by simp [evaluate, check_div]
example : check_div e s   12    6    2  := by simp [evaluate, check_div]
example : check_div e s   13    6    2  := by simp [evaluate, check_div]
example : check_div e s   14    6    2  := by simp [evaluate, check_div]
example : check_div e s   15    6    2  := by simp [evaluate, check_div]
example : check_div e s   16    6    3  := by simp [evaluate, check_div]
example : check_div e s   17    6    3  := by simp [evaluate, check_div]
example : check_div e s   18    6    3  := by simp [evaluate, check_div]
example : check_div e s   19    6    3  := by simp [evaluate, check_div]
example : check_div e s   20    6    3  := by simp [evaluate, check_div]
example : check_div e s   21    6    4  := by simp [evaluate, check_div]
example : check_div e s   22    6    4  := by simp [evaluate, check_div]
example : check_div e s   23    6    4  := by simp [evaluate, check_div]
example : check_div e s   24    6    4  := by simp [evaluate, check_div]
example : check_div e s    0  (-6)   0  := by simp [evaluate, check_div]
example : check_div e s (-12)   0    0  := by simp [evaluate, check_div]
example : check_div e s   12  (-6) (-2) := by simp [evaluate, check_div]
example : check_div e s (-13)   6  (-2) := by simp [evaluate, check_div]
example : check_div e s   14  (-6) (-2) := by simp [evaluate, check_div]
example : check_div e s (-15)   6  (-2) := by simp [evaluate, check_div]
example : check_div e s   16  (-6) (-3) := by simp [evaluate, check_div]
example : check_div e s (-17)   6  (-3) := by simp [evaluate, check_div]
example : check_div e s   18  (-6) (-3) := by simp [evaluate, check_div]
example : check_div e s (-19)   6  (-3) := by simp [evaluate, check_div]
example : check_div e s   20  (-6) (-3) := by simp [evaluate, check_div]
example : check_div e s (-21)   6  (-4) := by simp [evaluate, check_div]
example : check_div e s   22  (-6) (-4) := by simp [evaluate, check_div]
example : check_div e s (-23)   6  (-4) := by simp [evaluate, check_div]
example : check_div e s   24  (-6) (-4) := by simp [evaluate, check_div]
example : check_div e s    9    1  ( 9) := by simp [evaluate, check_div]
example : check_div e s ( -9)   1  (-9) := by simp [evaluate, check_div]


theorem evaluate_scale (m n : Integer) : evaluate e s (Scale m n x) = evaluate e s (DivValue (MulValue x (Constant m)) (Constant n)) :=
  by simp [evaluate, hx]

#check evaluate_scale


-- theorem evaluate_choice


theorem evaluate_start : evaluate e s TimeIntervalStart = e.timeInterval.fst.toInt :=
  by simp [evaluate]

#check evaluate_start


theorem evaluate_end : evaluate e s TimeIntervalEnd = e.timeInterval.snd.toInt :=
  by simp [evaluate]

#check evaluate_end


-- theorem evaluate_use (h : s.boundValues.contains v)


theorem evaluate_cond : evaluate e s (Cond a x y) = if a' then x' else y' :=
  by simp [evaluate, ha, hx, hy]

#check evaluate_cond


end Marlowe.Proof
