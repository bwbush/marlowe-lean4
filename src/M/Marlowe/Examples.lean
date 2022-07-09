

import M.Marlowe.Language
import M.Marlowe.Semantics
import M.Plutus
import M.PlutusCore


namespace Marlowe.Examples


open Marlowe.Language.Contract
open Marlowe.Language.Input
open Marlowe.Language.State
open Marlowe.Semantics
open Plutus.V1.Ledger.Time (POSIXTime)
open PlutusCore (ByteString)


def trivial (party : PartyT) (deposit : Int) (withdrawal : Int) (timeout : Timeout): Contract :=
  When
    [
      Case (Deposit party party Ada (Constant deposit))
        $ When
          [
            Case (Notify TrueObs)
              $ Pay party (Party party) Ada (Constant withdrawal)
              $ When
                [
                  Case (Notify TrueObs)
                  Close
                ]
                timeout
                Close
          ]
          timeout
          Close
    ]
    timeout
    Close

private def theParty := Role $ ByteString.fromString "Party"

def runTrivial : Except String (List OperationResult) :=
  let party := theParty
  let deposit : Int := 100
  let withdrawal : Int := 60
  let timeout : Timeout := POSIXTime.mk 1000
  let e : Environment := {timeInterval := (POSIXTime.mk 1, POSIXTime.mk 10)}
  let s0 := default
  let c0 := trivial party deposit withdrawal timeout
  let is := [
              NormalInput $ IDeposit party party Ada deposit
            , NormalInput INotify
            , NormalInput INotify
            ]
  do
    let r0 <- operate e s0          is                 c0
    let r1 <- operate e r0.newState r0.inputsRemaining r0.newContract
    let r2 <- operate e r1.newState r1.inputsRemaining r1.newContract
    let r3 <- operate e r2.newState r2.inputsRemaining r2.newContract
    let r4 <- operate e r3.newState r3.inputsRemaining r3.newContract
    pure [r0, r1,r2, r3, r4]

#eval runTrivial


def executeTrivial : Except String (List OperationResult) :=
  let party := theParty
  let deposit : Int := 100
  let withdrawal : Int := 60
  let timeout : Timeout := POSIXTime.mk 1000
  let e : Environment := {timeInterval := (POSIXTime.mk 1, POSIXTime.mk 10)}
  let s0 := default
  let c0 := trivial party deposit withdrawal timeout
  let is := [
              NormalInput $ IDeposit party party Ada deposit
            , NormalInput INotify
            , NormalInput INotify
            ]
  execute e s0 is c0 9

#eval executeTrivial


def checkTrivial : Bool :=
  let actual := runTrivial
  let expected :=
    {
      (default : OperationResult) with
        newState    := {(default : State) with minTime := POSIXTime.mk 10}
      , newContract := Close
      , payments    := [{account := theParty, payee := Party $ theParty, money := singletonMoney Ada 40}]
    }
  match actual with
    | Except.ok [_, _, _, _, actual'] => actual' == expected
    | _                               => false

#eval checkTrivial


end Marlowe.Examples
