

import M.Marlowe.Language


namespace Marlowe.Semantics


open Marlowe.Language.Class
open Marlowe.Language.Contract
open Marlowe.Language.Input
open Marlowe.Language.State


mutual

  def evaluate [ABool ω] [AInt ι] [APOSIXTime τ ι] [AEq β ω] [AEq ι ω] [AOrd ι ω] [ACond ω ι]
               [AMap μ (AccountId β × TokenT β) ι σ ω]
               [AMap μ (ChoiceIdT β) (ChosenNum ι) σ ω]
               [AMap μ (ValueIdT β) ι σ ω]
               (e : Environment τ) (s : State β ι τ μ) : Value β ι → ι
    | AvailableMoney a t => AMap.lookup (a, t) s.accounts.val
    | Constant x         => x
    | NegValue x         => A- evaluate e s x
    | AddValue x y       => evaluate e s x A+ evaluate e s y
    | SubValue x y       => evaluate e s x A- evaluate e s y
    | MulValue x y       => evaluate e s x A* evaluate e s y
    | DivValue x y       => evaluate e s x A/ evaluate e s y
    | ChoiceValue c      => (AMap.lookup c s.choices).val
    | TimeIntervalStart  => APOSIXTime.toAInt e.timeInterval.val.fst
    | TimeIntervalEnd    => APOSIXTime.toAInt e.timeInterval.val.snd
    | UseValue v         => AMap.lookup v s.boundValues
    | Cond o x y         => observe e s o A? evaluate e s x A: evaluate e s y

  def observe [ABool ω] [AInt ι] [APOSIXTime τ ι] [AEq β ω] [AEq ι ω] [AOrd ι ω] [ACond ω ι]
              [AMap μ (AccountId β × TokenT β) ι σ ω]
              [AMap μ (ChoiceIdT β) (ChosenNum ι) σ ω]
              [AMap μ (ValueIdT β) ι σ ω]
              (e : Environment τ) (s : State β ι τ μ) : Observation β ι → ω
    | AndObs x y       => observe e s x A&& observe e s y
    | OrObs x y        => observe e s x A|| observe e s y
    | NotObs x         => A! (observe e s x)
    | ChoseSomething c => AMap.member c s.choices
    | ValueGE x y      => evaluate e s y A<= evaluate e s x
    | ValueGT x y      => evaluate e s y A<  evaluate e s x
    | ValueLT x y      => evaluate e s x A<  evaluate e s y
    | ValueLE x y      => evaluate e s x A<= evaluate e s y
    | ValueEQ x y      => evaluate e s x A== evaluate e s y
    | TrueObs          => atrue
    | FalseObs         => afalse

end


end Marlowe.Semantics
