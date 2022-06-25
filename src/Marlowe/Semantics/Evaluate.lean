

import Marlowe.Language
import Marlowe.Primitives
import Std


namespace Marlowe.Semantics


open Marlowe.Language.Contract
open Marlowe.Language.Input
open Marlowe.Language.State
open Marlowe.Primitives (fromInteger)
open Std.RBMap (contains findD)


private def divide (num : Int) (den : Int) : Int :=
  num / den  -- FIXME: This is intentionally incorrect.


mutual

  def evaluate : Environment → State → Value → Int
    | _, s, AvailableMoney a t => (accounts s).findD (a, t) (Int.ofNat 0)
    | _, _, Constant x         => x
    | e, s, NegValue x         => - evaluate e s x
    | e, s, AddValue x y       => evaluate e s x + evaluate e s y
    | e, s, SubValue x y       => evaluate e s x - evaluate e s y
    | e, s, MulValue x y       => evaluate e s x * evaluate e s y
    | e, s, DivValue x y       => divide (evaluate e s x) (evaluate e s y)
    | e, s, Scale num den x    => divide (fromInteger num * evaluate e s x) den
    | _, s, ChoiceValue c      => (choices s).findD c (Int.ofNat 0)
    | e, _, TimeIntervalStart  => (timeInterval e).fst
    | e, _, TimeIntervalEnd    => (timeInterval e).snd
    | _, s, UseValue v         => (boundValues s).findD v (Int.ofNat 0)
    | e, s, Cond o x y         => if observe e s o then evaluate e s x else evaluate e s y

  def observe : Environment → State → Observation → Bool
    | e, s, (AndObs x y      ) => observe e s x && observe e s y
    | e, s, (OrObs x y       ) => observe e s x || observe e s y
    | e, s, (NotObs x        ) => ! observe e s x
    | _, s, (ChoseSomething c) => (choices s).contains c
    | e, s, (ValueGE x y     ) => evaluate e s x >= evaluate e s y
    | e, s, (ValueGT x y     ) => evaluate e s x >  evaluate e s y
    | e, s, (ValueLT x y     ) => evaluate e s x <  evaluate e s y
    | e, s, (ValueLE x y     ) => evaluate e s x <= evaluate e s y
    | e, s, (ValueEQ x y     ) => evaluate e s x == evaluate e s y
    | _, _, TrueObs            => true
    | _, _, FalseObs           => false

end


end Marlowe.Semantics
