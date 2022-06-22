

import Marlowe.Semantics.Contract
import Marlowe.Semantics.Input
import Marlowe.Semantics.Primitives
import Std.Data.RBMap


namespace Marlowe.Semantics.State


open Marlowe.Semantics.Contract
open Marlowe.Semantics.Input
open Marlowe.Semantics.Primitives
open Std (RBMap)


def compareAT (_at : AccountId × TokenT) (_at' : AccountId × TokenT) : Ordering :=
  Ordering.lt


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


end Marlowe.Semantics.State
