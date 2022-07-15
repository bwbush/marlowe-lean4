

import M.Marlowe.Language


namespace Marlowe.Semantics


open Marlowe.Language.Class
open Marlowe.Language.Contract
open Marlowe.Language.Input
open Marlowe.Language.State


private def deposit [ABool ω] [AInt ι] [AEq β ω]
                    [AMap μ (AccountId β × TokenT β) ι σ ω]
                    (s : Accounts β ι μ) (a : AccountId β) (t : TokenT β) (n : ι) : Accounts β ι μ :=
  let previous := AMap.lookup (a, t) s.val
  Accounts.mk $ AMap.insert (a, t) (previous A+ n) s.val


def act (s : State β ι τ μ) [ABool ω] [AInt ι] [AEq β ω]
                            [AMap μ (AccountId β × TokenT β) ι σ ω]
                            [AMap μ (ChoiceIdT β) (ChosenNum ι) σ ω]
                            : InputContent β ι → State β ι τ μ 
  | IDeposit a _ t n => {s with accounts := deposit s.accounts a t n}
  | IChoice c n      => {s with choices := AMap.insert c n s.choices}
  | INotify          => s


end Marlowe.Semantics
