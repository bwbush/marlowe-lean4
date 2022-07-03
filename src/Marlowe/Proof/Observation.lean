

import Marlowe.Language
import Marlowe.Semantics


namespace Marlowe.Proof


open Marlowe.Language.Contract
open Marlowe.Language.State
open Marlowe.Semantics (evaluate observe)


variable (e : Environment)
variable (s : State)

variable (a b : Observation)
variable (a' b' : Bool)

variable (ha : observe e s a = a')
variable (hb : observe e s b = b')

variable (c : ChoiceIdT)

variable (x y : Value)
variable (x' y' : Int)

variable (hx : evaluate e s x = x')
variable (hy : evaluate e s y = y')


theorem observe_and : observe e s (AndObs a b) = (a' && b') :=
  by simp [observe, ha, hb]

#check observe_and


theorem observe_or : observe e s (OrObs a b) = (a' || b') :=
  by simp [observe, ha, hb]

#check observe_or


theorem observe_not : observe e s (NotObs a) = !a' :=
 by simp [observe, ha]

#check observe_not


theorem observe_chosen : observe e s (ChoseSomething c) = s.choices.contains c :=
  by simp [observe]

#check observe_chosen


theorem observe_ge : observe e s (ValueGE x y) = (x' >= y') :=
  by simp [observe, hx, hy]

#check observe_ge


theorem observe_gt : observe e s (ValueGT x y) = (x' > y') :=
  by simp [observe, hx, hy]

#check observe_gt


theorem observe_lt : observe e s (ValueLT x y) = (x' < y') :=
  by simp [observe, hx, hy]

#check observe_lt


theorem observe_le : observe e s (ValueLE x y) = (x' <= y') :=
  by simp [observe, hx, hy]

#check observe_le


theorem observe_eq : observe e s (ValueEQ x y) = (x' == y') :=
  by simp [observe, hx, hy]

#check observe_eq


theorem observe_true : observe e s TrueObs = true :=
  by rfl

#check observe_true


theorem observe_false : observe e s FalseObs = false :=
  by rfl

#check observe_false


theorem negate_true : observe e s (NotObs TrueObs) = false := by simp [observe]

example : observe e s (NotObs TrueObs) = false := by
  calc
    observe e s (NotObs TrueObs) = ! observe e s TrueObs := by simp [observe]
    _                            = ! true                := by rfl
    _                            = false                 := by rfl


end Marlowe.Proof
