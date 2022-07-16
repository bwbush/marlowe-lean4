

import M.Marlowe.Intermediate.AST
import M.Marlowe.Language


namespace Marlowe.Intermediate


open Marlowe.Language.Contract
open Marlowe.Language.Input
open Marlowe.Language.State


def deposit (a : AccountId) (t : TokenT) (n : Int) : Op →  Op :=
  let prior := GetMoney a t
  let posterior := prior +₁ ConstantInt₁ n
  SetMoney a t
    $ Evaluate posterior


def choose (c : ChoiceIdT) (n : Int) : Op → Op :=
  SetChoice c
    $ ConstantInt₂ n


def act : InputContent → Op → Op
  | IDeposit a _ t n => deposit a t n
  | IChoice c n      => choose c n
  | INotify          => id


end Marlowe.Intermediate
