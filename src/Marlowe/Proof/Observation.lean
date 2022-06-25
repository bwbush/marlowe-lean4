

import Marlowe.Language
import Marlowe.Semantics


namespace Marlowe.Proof


open Marlowe.Language.Contract
open Marlowe.Language.State
open Marlowe.Semantics (observe)


variable (e : Environment)
variable (s : State)
variable (o : Observation)


theorem truth : observe e s TrueObs = true := by
  rfl


theorem falsity : observe e s FalseObs = false := by
  rfl


theorem negate_true : observe e s (NotObs TrueObs) = false := by simp [observe]

example : observe e s (NotObs TrueObs) = false := by
  calc
    observe e s (NotObs TrueObs) = ! observe e s TrueObs := by simp [observe]
    _                            = ! true                := by rfl
    _                            = false                 := by rfl


theorem negate_anything (x : Observation) (y : Bool) (h : observe e s x = y) : observe e s (NotObs x) = !y :=
  by simp [observe, h]

#check negate_anything

#print negate_anything


end Marlowe.Proof
