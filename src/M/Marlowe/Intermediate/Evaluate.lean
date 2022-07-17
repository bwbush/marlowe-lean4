

import M.Marlowe.Intermediate.AST
import M.Marlowe.Language


namespace Marlowe.Intermediate


open Marlowe.Language.Contract


mutual

  def evaluate₁ : Value → Int₁
    | AvailableMoney a t => GetMoney a t
    | Constant x         => ConstantInt₁ x
    | NegValue x         => -₁ evaluate₁ x
    | AddValue x y       => evaluate₁ x +₁ evaluate₁ y
    | SubValue x y       => evaluate₁ x -₁ evaluate₁ y
    | MulValue x y       => evaluate₁ x *₁ evaluate₁ y
    | DivValue x y       => evaluate₁ x /₁ evaluate₁ y
    | ChoiceValue c      => GetChoice c
    | TimeIntervalStart  => GetStart
    | TimeIntervalEnd    => GetEnd
    | UseValue v         => GetValue v
    | Cond o x y         => observe₁ o ?₁ evaluate₁ x :₁ evaluate₁ y

  def observe₁ : Observation → Bool₁
    | AndObs x y       => observe₁ x &&₁ observe₁ y
    | OrObs x y        => observe₁ x ||₁ observe₁ y
    | NotObs x         => !₁ observe₁ x
    | ChoseSomething c => HasChosen c
    | ValueGE x y      => evaluate₁ x ≥₁  evaluate₁ y
    | ValueGT x y      => evaluate₁ x >₁  evaluate₁ y
    | ValueLT x y      => evaluate₁ x <₁  evaluate₁ y
    | ValueLE x y      => evaluate₁ x ≤₁  evaluate₁ y
    | ValueEQ x y      => evaluate₁ x ==₁ evaluate₁ y
    | TrueObs          => ConstantBool₁ true
    | FalseObs         => ConstantBool₁ false

end


def evaluate₂ : Value → Int₂ :=
  Evaluate ∘ evaluate₁

def observe₂ : Observation → Bool₂ :=
  Observe ∘ observe₁


end Marlowe.Intermediate
