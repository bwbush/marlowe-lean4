

import M.Marlowe.Language.Contract
import M.Marlowe.Language.Input
import M.Plutus
import M.PlutusTx


namespace Marlowe.Language.State


open Marlowe.Language.Contract
open Marlowe.Language.Input
open Plutus.V1.Ledger.Time (POSIXTime)
open PlutusTx.AssocMap (Map)


def Accounts := Map (AccountId × TokenT) Int

deriving instance Repr for Accounts


structure State :=
  accounts    : Accounts
  choices     : Map ChoiceIdT ChosenNum
  boundValues : Map ValueIdT Int
  minTime     : POSIXTime
deriving Repr

instance : BEq State where
  beq x y := x.accounts.toList == y.accounts.toList
               && x.choices.toList == y.choices.toList
               && x.boundValues.toList == y.boundValues.toList
               && x.minTime == y.minTime

instance : Inhabited State where
  default := {
               accounts    := Map.empty
             , choices     := Map.empty
             , boundValues := Map.empty
             , minTime     := default
             }
export State (accounts choices boundValues minTime)


def TimeInterval := POSIXTime × POSIXTime

deriving instance BEq, Repr for TimeInterval


structure Environment :=
  timeInterval : TimeInterval
deriving Repr

export Environment (timeInterval)


def Money := Map TokenT Int

instance : BEq Money where
  beq x y := x.toList == y.toList

deriving instance Repr for Money


def singletonMoney (t : TokenT) (n : Int) : Money :=
  Map.mk [(t, n)]


structure Payment :=
  account : AccountId
  payee   : Payee
  money   : Money
deriving BEq, Repr


end Marlowe.Language.State
