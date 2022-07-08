

import M.Marlowe.Language.Contract
import M.Marlowe.Language.Input
import M.Marlowe.Primitives
import Std


namespace Marlowe.Language.State


open Marlowe.Language.Contract
open Marlowe.Language.Input
open Marlowe.Primitives (Integer POSIXTime)
open Std (RBMap)


private def compareAT : (AccountId × TokenT) → (AccountId × TokenT) →  Ordering
  | (a0, t0), (a1, t1) => match compare a0 a1 with
                            | Ordering.eq => compare t0 t1
                            | cmp         => cmp


def Accounts := RBMap (AccountId × TokenT) Integer compareAT

deriving instance Repr for Accounts


structure State :=
  accounts    : Accounts
  choices     : RBMap ChoiceIdT ChosenNum compare
  boundValues : RBMap ValueIdT Integer compare
  minTime     : POSIXTime
deriving Repr

instance : BEq State where
  beq x y := x.accounts.toList == y.accounts.toList
               && x.choices.toList == y.choices.toList
               && x.boundValues.toList == y.boundValues.toList
               && x.minTime == y.minTime

instance : Inhabited State where
  default := {
               accounts    := RBMap.empty
             , choices     := RBMap.empty
             , boundValues := RBMap.empty
             , minTime     := default
             }
export State (accounts choices boundValues minTime)


def TimeInterval := POSIXTime × POSIXTime

deriving instance BEq, Repr for TimeInterval


structure Environment :=
  timeInterval : TimeInterval
deriving Repr

export Environment (timeInterval)


def Money := RBMap TokenT Integer compare

instance : BEq Money where
  beq x y := x.toList == y.toList

deriving instance Repr for Money


def singletonMoney (t : TokenT) (n : Integer) : Money :=
  RBMap.ofList [(t, n)]


structure Payment :=
  account : AccountId
  payee   : Payee
  money   : Money
deriving BEq, Repr


end Marlowe.Language.State
