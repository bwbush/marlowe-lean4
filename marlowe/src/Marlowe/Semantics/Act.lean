

import Marlowe.Language
import Marlowe.Primitives
import Std


namespace Marlowe.Semantics


open Marlowe.Language.Contract
open Marlowe.Language.Input
open Marlowe.Language.State
open Marlowe.Primitives (Integer)
open Std.RBMap (findD insert)


private def deposit (s : Accounts) (a : AccountId) (t : TokenT) (n : Integer): Accounts :=
  let previous := s.findD (a, t) default
  s.insert (a, t) {toInt := previous.toInt + n.toInt}


def act (s : State) : InputContent → State
  | IDeposit a _ t n => {s with accounts := deposit s.accounts a t n}
  | IChoice c n      => {s with choices := s.choices.insert c n}
  | INotify          => s


end Marlowe.Semantics
