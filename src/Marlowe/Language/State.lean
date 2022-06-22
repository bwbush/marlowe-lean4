

import Marlowe.Language.Contract
import Marlowe.Language.Input
import Marlowe.Primitives
import Std.Data.RBMap


namespace Marlowe.Language.State


open Marlowe.Language.Contract
open Marlowe.Language.Input
open Marlowe.Primitives
open Std (RBMap)


def compareAT : (AccountId × TokenT) → (AccountId × TokenT) →  Ordering
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

export State (accounts choices boundValues minTime)


def TimeInterval := Prod POSIXTime POSIXTime

deriving instance Repr for TimeInterval


structure Environment :=
  timeInterval : TimeInterval
deriving Repr

export Environment (timeInterval)


end Marlowe.Language.State
