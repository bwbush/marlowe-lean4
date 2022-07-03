

import Marlowe.Language
import Marlowe.Primitives
import Std


namespace Marlowe.Semantics


open Marlowe.Language.Contract
open Marlowe.Language.Input
open Marlowe.Language.State
open Std.RBMap (contains findD)


def divide (num : Int) (den : Int) : Int :=
  if num == 0 || den == 0
    then 0
    else
      let sig := if num * den > 0 then 1 else -1
      let num' : Nat := Int.natAbs num
      let den' : Nat := Int.natAbs den
      let rat := num' / den'
      let rem := num' % den'
      match compare (2 * rem) den', rat % 2 == 0 with
        | Ordering.lt, _     => sig * rat
        | Ordering.gt, _     => sig * (rat + 1)
        | _          , true  => sig * rat
        | _          , false => sig * (rat + 1)


mutual

  def evaluate (e : Environment) (s : State) : Value → Int
    | AvailableMoney a t => (s.accounts.findD (a, t) default).toInt
    | Constant x         => x.toInt
    | NegValue x         => - evaluate e s x
    | AddValue x y       => evaluate e s x + evaluate e s y
    | SubValue x y       => evaluate e s x - evaluate e s y
    | MulValue x y       => evaluate e s x * evaluate e s y
    | DivValue x y       => divide (evaluate e s x) (evaluate e s y)
    | Scale num den x    => divide (num.toInt * evaluate e s x) den.toInt
    | ChoiceValue c      => (s.choices.findD c default).toInt
    | TimeIntervalStart  => e.timeInterval.fst.toInt
    | TimeIntervalEnd    => e.timeInterval.snd.toInt
    | UseValue v         => (s.boundValues.findD v default).toInt
    | Cond o x y         => if observe e s o then evaluate e s x else evaluate e s y

  def observe (e : Environment) (s : State) : Observation → Bool
    | AndObs x y       => observe e s x && observe e s y
    | OrObs x y        => observe e s x || observe e s y
    | NotObs x         => ! observe e s x
    | ChoseSomething c => s.choices.contains c
    | ValueGE x y      => evaluate e s x >= evaluate e s y
    | ValueGT x y      => evaluate e s x >  evaluate e s y
    | ValueLT x y      => evaluate e s x <  evaluate e s y
    | ValueLE x y      => evaluate e s x <= evaluate e s y
    | ValueEQ x y      => evaluate e s x == evaluate e s y
    | TrueObs          => true
    | FalseObs         => false

end


end Marlowe.Semantics
