

import Marlowe.Language
import Marlowe.Primitives
import Std


namespace Marlowe.Semantics


open Marlowe.Language.Contract
open Marlowe.Language.Input
open Marlowe.Language.State
open Marlowe.Primitives (Integer fromInteger)
open Std.RBMap (findD insert)


private def deposit (s : Accounts) (a : AccountId) (t : TokenT) (n : Integer): Accounts :=
  let previous : Int := s.findD (a, t) (Int.ofNat 0)
  s.insert (a, t) (previous + fromInteger n)


def act : Environment × State → InputContent → Environment × State
  | (e, s), IDeposit a _ t n => (e, {s with accounts := deposit (accounts s) a t n})
  | (e, s), IChoice c n      => (e, {s with choices := (choices s).insert c n})
  | (e, s), INotify          => (e, s)


end Marlowe.Semantics
