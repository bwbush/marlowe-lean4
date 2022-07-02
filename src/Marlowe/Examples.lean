

import Marlowe.Language.Contract
import Marlowe.Language.State
import Marlowe.Primitives
import Marlowe.Semantics


namespace Marlowe.Examples


open Marlowe.Language.Contract
open Marlowe.Language.Input
open Marlowe.Language.State
open Marlowe.Primitives (Integer integer posixTime)
open Marlowe.Semantics


def trivial (party : PartyT) (deposit : Integer) (withdrawal : Integer) (timeout : Timeout): Contract :=
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

def runTrivial : Except String (List OperationResult) :=
  let party := Role "Party"
  let deposit : Integer := integer 100
  let withdrawal : Integer := integer 60
  let timeout : Timeout := posixTime 1000
  let e : Environment := {timeInterval := (posixTime 1, posixTime 10)}
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
  let party := Role "Party"
  let deposit : Integer := integer 100
  let withdrawal : Integer := integer 60
  let timeout : Timeout := posixTime 1000
  let e : Environment := {timeInterval := (posixTime 1, posixTime 10)}
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
        newState    := {(default : State) with minTime := posixTime 10}
      , newContract := Close
      , payments    := [{account := Role "Party", payee := Party $ Role "Party", money := singletonMoney Ada $ integer 40}]
    }
  match actual with
  | Except.ok [_, _, _, _, actual'] => actual' == expected
  | _                               => false

#eval checkTrivial


end Marlowe.Examples
