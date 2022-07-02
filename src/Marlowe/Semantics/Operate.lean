

import Marlowe.Language
import Marlowe.Semantics.Act
import Marlowe.Semantics.Evaluate
import Marlowe.Primitives

namespace Marlowe.Semantics


open Marlowe.Language.Contract
open Marlowe.Language.Input
open Marlowe.Language.State
open Marlowe.Primitives (integer)
open Std (RBMap)


structure OperationResult :=
  newState          : State
  newContract       : Contract
  inputsApplied     : List Input
  payments          : List Payment
  warnings          : List String
  inputsRemaining   : List Input
deriving BEq, Repr

instance : Inhabited OperationResult where
  default := {
               newState        := default
             , newContract     := default
             , payments        := []
             , warnings        := []
             , inputsApplied   := []
             , inputsRemaining := []
             }

export OperationResult (newState newContract inputsApplied payments warnings inputsRemaining)


private def noInputsApplied (e : Environment) (s : State) (is : List Input) (c : Contract): OperationResult :=
  {
    (default : OperationResult) with
      newState        := {s with minTime := e.timeInterval.snd}
    , inputsRemaining := is
    , newContract     := c
  }


private def demerkleize : Input → CaseT → Option (InputContent × Action × Contract)
  | NormalInput input                      , Case action continuation    => some (input, action, continuation)
  | MerkleizedInput input hash continuation, MerkleizedCase action hash' => if hash == hash'
                                                                              then some (input, action, continuation)
                                                                              else none
  | _                                       , _                          => none


private def demerkleize' : Input → CaseT → Except String (InputContent × Action × Contract)
  | NormalInput input                      , Case action continuation    => pure (input, action, continuation)
  | MerkleizedInput input hash continuation, MerkleizedCase action hash' => if hash == hash'
                                                                              then pure (input, action, continuation)
                                                                              else throw "Merkle hashes do no match."
  | MerkleizedInput _ _ _                  , Case _ _                    => throw "Merkleized input requires merkleized case."
  | NormalInput _                          , MerkleizedCase _ _          => throw "Merkleized case requires merkleized input."


private def applyInput (e : Environment) (s : State) (i : InputContent) (a : Action) : Option State :=
  match i, a with
  | IDeposit account party token amount, Deposit account' party' token' value => let amount' := evaluate e s value
                                                                                 do
                                                                                   guard $ account == account'
                                                                                   guard $ party == party'
                                                                                   guard $ token == token'
                                                                                   guard $ amount.toInt == amount'
                                                                                   pure $ act s i
  | IChoice choiceId choiceNum         , Choice choiceId' bounds              => do
                                                                                   guard $ choiceId == choiceId'
                                                                                   let inBound : BoundT → Bool
                                                                                     | Bound lower upper =>
                                                                                         choiceNum.toInt >= lower.toInt
                                                                                           && choiceNum.toInt <= upper.toInt
                                                                                   guard $ bounds.all inBound
                                                                                   pure $ act s i
  | INotify                            , Notify observation                   => do
                                                                                   guard $ observe e s observation
                                                                                   pure $ act s i
  | _                                  , _                                    => none


private def inputsApplied (e : Environment) (s : State) (inputs : List Input) (cases : List CaseT) (t : Timeout) (c : Contract) : Except String OperationResult :=
  let startBeforeTimeout : Bool := e.timeInterval.fst.toInt < t.toInt
  let finishBeforeTimeout : Bool := e.timeInterval.snd.toInt < t.toInt
  match startBeforeTimeout, finishBeforeTimeout, inputs with
  | false, _    , _               => pure $ noInputsApplied e s inputs c
  | true , false, _               => throw "Ambiguous time interval."
  | _    , _    , input :: inputs'=> let merge (prior : Option (State × Contract)) (case : CaseT) : Option (State × Contract) :=
                                       prior <|> do
                                         let (content, action, continuation) <- demerkleize input case
                                         let s' <- applyInput e s content action
                                         pure (s', continuation)
                                     let result := cases.foldl merge none
                                     match result with
                                     | some (s', c') => pure
                                                          {
                                                            newState        := s'
                                                          , newContract     := c'
                                                          , payments        := []
                                                          , warnings        := []
                                                          , inputsApplied   := [input]
                                                          , inputsRemaining := inputs'
                                                          }
                                     | none          => throw "No case matching input."
  | _   , _     , []              => throw "No input."


private def makePayments (accounts : Accounts) : List Payment :=
  accounts.toList.map
    (
      fun ((a, t), n) => {account := a, payee := Party a, money := singletonMoney t n}
    )


private def makePayment (accounts : Accounts) (a : AccountId) (p : Payee) (t : TokenT) (payment : Int): Except String (Accounts × Payment) :=
  do
    let available : Int := (accounts.findD (a, t) default).toInt
    let remainder : Int := available - payment
    match (payment > 0 : Bool), (remainder > 0 : Bool) with
    | false, _     => throw "Attempt to withdraw non-positive amount."
    | _    , false => throw "Attempt to withdraw amount in excess of available funds."
    | true , true  => pure
                        (
                          accounts.insert (a, t) $ integer remainder
                        , {account := a, payee := p, money := singletonMoney t $ integer payment}
                        )


private def bindVariable (e : Environment) (s : State) (v : ValueIdT) (x : Value) : State :=
  {s with boundValues := s.boundValues.insert v $ integer $ evaluate e s x}


private def ensureValidTime (e : Environment) (s : State) : Except String Unit :=
  let start   := e.timeInterval.fst
  let finish  := e.timeInterval.snd
  let minimum := s.minTime
  match (start.toInt <= finish.toInt : Bool), (minimum.toInt <= finish.toInt : Bool) with
  | false, _     => throw "Start of validity interval follows its finish."
  | _    , false => throw "Finish of validity interval preceeds minimum time."
  | true , true  => pure ()


def operate (e : Environment) (s : State) (is : List Input) (c : Contract) : Except String OperationResult :=
  do
    ensureValidTime e s
    match c with
    | Close            => pure
                            $ {noInputsApplied e {s with accounts := RBMap.empty} is default with payments := makePayments (accounts s)}
    | Pay a p t x c    => do
                            let x' := evaluate e s x
                            let (accounts', y) <- makePayment (accounts s) a p t x'
                            pure
                              $ {noInputsApplied e {s with accounts := accounts'} is c with payments := [y]}
    | If o cThen cElse => pure
                            $ noInputsApplied e s is 
                            $ if observe e s o then cThen else cElse
    | When cs t c      => inputsApplied e s is cs t c
    | Let v x c        => pure
                            $ noInputsApplied e (bindVariable e s v x) is c
    | Assert o c       => if observe e s o
                            then pure $ noInputsApplied e s is c
                            else pure {(noInputsApplied e s is c) with warnings := ["Assertion failed."]}


def execute (e : Environment) (s : State) (is : List Input) (c: Contract) : Nat -> Except String (List OperationResult)
  | 0 => pure []
  | n + 1 => do
               let result <- operate e s is c
               if result.newContract == Close && result.newState.accounts.toList == []
                 then pure [result]
                 else do
                        let remainder <- execute e result.newState result.inputsRemaining result.newContract n
                        pure $ result :: remainder


end Marlowe.Semantics
