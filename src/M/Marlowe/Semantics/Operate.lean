

import M.Marlowe.Language
import M.Marlowe.Semantics.Act
import M.Marlowe.Semantics.Evaluate


namespace Marlowe.Semantics


open Marlowe.Language.Class
open Marlowe.Language.Contract
open Marlowe.Language.Input
open Marlowe.Language.State


structure OperationResult (β ι τ : Type) (μ : Type → Type → Type) (σ : Type → Type) where
  newState        : State β ι τ μ 
  newContract     : Contract β ι τ
  inputsApplied   : σ (Input β ι τ)
  payments        : σ (Payment β ι)
  warnings        : List String
  inputsRemaining : σ (Input β ι τ)


instance [Inhabited (State β ι τ μ)]
         [AList σ (Input β ι τ)]
         [AList σ (Payment β ι)]
         : Inhabited (OperationResult β ι τ μ σ) where
  default := {
               newState        := default
             , newContract     := default
             , payments        := default
             , warnings        := default
             , inputsApplied   := default
             , inputsRemaining := default
             }

export OperationResult (newState newContract inputsApplied payments warnings inputsRemaining)


private def noInputsApplied [Inhabited (OperationResult β ι τ μ σ)]
                            (e : Environment τ) (s : State β ι τ μ) (is : σ (Input β ι τ)) (c : Contract β ι τ): OperationResult β ι τ μ σ :=
  {
    (default : OperationResult β ι τ μ σ) with
      newState        := {s with minTime := e.timeInterval.val.snd}
    , inputsRemaining := is
    , newContract     := c
  }


private def demerkleize [AEq β ω] [ACond ω (Option (InputContent β ι × Action β ι × Contract β ι τ))]
                        : Input β ι τ → CaseT β ι τ → Option (InputContent β ι × Action β ι × Contract β ι τ)
  | NormalInput input                      , Case action continuation    => some (input, action, continuation)
  | MerkleizedInput input hash continuation, MerkleizedCase action hash' => hash A== hash'
                                                                              A? some (input, action, continuation)
                                                                              A: none
  | _                                       , _                          => none


private def demerkleize' [AEq β ω] [ACond ω (Except String (InputContent β ι × Action β ι × Contract β ι τ))]
                         : Input β ι τ → CaseT β ι τ → Except String (InputContent β ι × Action β ι × Contract β ι τ)
  | NormalInput input                      , Case action continuation    => pure (input, action, continuation)
  | MerkleizedInput input hash continuation, MerkleizedCase action hash' => hash A== hash'
                                                                              A? pure (input, action, continuation)
                                                                              A: throw "Merkle hashes do no match."
  | MerkleizedInput _ _ _                  , Case _ _                    => throw "Merkleized input requires merkleized case."
  | NormalInput _                          , MerkleizedCase _ _          => throw "Merkleized case requires merkleized input."


private def applyInput [AInt ι] [APOSIXTime τ ι] [ABool Bool]
                       [AEq β Bool] [AEq ι Bool] [AOrd ι Bool] [ACond Bool ι]
                       [AMap μ (AccountId β × TokenT β) ι σ Bool]
                       [AMap μ (ChoiceIdT β) (ChosenNum ι) σ Bool]
                       [AMap μ (ValueIdT β) ι σ Bool]
                       (e : Environment τ) (s : State β ι τ μ) (i : InputContent β ι) (a : Action β ι) : Option (State β ι τ μ) :=
  match i, a with
    | IDeposit account party token amount, Deposit account' party' token' value => let amount' := evaluate e s value
                                                                                   do
                                                                                     guard $ account A== account'
                                                                                     guard $ party A== party'
                                                                                     guard $ token A== token'
                                                                                     guard $ amount A== amount'
                                                                                     pure $ act s i
    | IChoice choiceId choiceNum         , Choice choiceId' bounds              => do
                                                                                     guard $ choiceId A== choiceId'
                                                                                     let inBound : BoundT ι → Bool
                                                                                       | Bound lower upper =>
                                                                                           lower A<= choiceNum.val
                                                                                             A&& choiceNum.val A<= upper
                                                                                     guard $ bounds.all inBound
                                                                                     pure $ act s i
    | INotify                            , Notify observation                   => do
                                                                                     guard $ observe e s observation
                                                                                     pure $ act s i
    | _                                  , _                                    => none


private def inputsApplied [AInt ι] [APOSIXTime τ ι] [AEq β Bool] [AEq ι Bool] [AOrd ι Bool] [AOrd τ Bool] [ABool Bool] [ACond Bool ι]
                          [Inhabited (ACond Bool (Option (InputContent β ι × Action β ι × Contract β ι τ)))]
                          [Inhabited (OperationResult β ι τ μ List)]
                          [AMap μ (AccountId β × TokenT β) ι σ Bool]
                          [AMap μ (ChoiceIdT β) (ChosenNum ι) σ Bool]
                          [AMap μ (ValueIdT β) ι σ Bool]
                          (e : Environment τ) (s : State β ι τ μ) (inputs : List (Input β ι τ)) (cases : List (CaseT β ι τ)) (t : Timeout τ) (c : Contract β ι τ) : Except String (OperationResult β ι τ μ List) :=
  let startBeforeTimeout := e.timeInterval.val.fst A< t.val
  let finishBeforeTimeout := e.timeInterval.val.snd A< t.val
  if startBeforeTimeout
    then do
           unless finishBeforeTimeout
             do throw "Ambiguous time interval."
           match inputs with
             | input :: inputs'=> let merge (prior : Option (State β ι τ μ × Contract β ι τ)) (case : CaseT β ι τ) : Option (State β ι τ μ × Contract β ι τ) :=
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
             | []              => throw "No input."
    else pure $ noInputsApplied e s inputs c


private def makePayments (accounts : Accounts β ι μ) : σ (Payment β ι) :=
  accounts.toList.map
    (
      fun ((a, t), n) => {account := a, payee := Party a, money := singletonMoney t n}
    )


private def makePayment (accounts : Accounts β ι μ) (a : AccountId β) (p : Payee β) (t : TokenT β) (payment : ι): Except String (Accounts β ι μ × Payment β ι) :=
  do
    let available : Int := accounts.lookup (a, t)
    let remainder : Int := available - payment
    unless (payment > 0)
      do throw "Attempt to withdraw non-positive amount."
    unless (remainder > 0)
      do throw "Attempt to withdraw amount in excess of available funds."
    pure
      (
        accounts.insert (a, t) remainder
      , {account := a, payee := p, money := singletonMoney t payment}
      )


private def bindVariable (e : Environment τ) (s : State β ι τ μ) (v : ValueIdT β) (x : Value β ι) : State β ι τ μ :=
  {s with boundValues := s.boundValues.insert v $ evaluate e s x}


private def ensureValidTime (e : Environment τ) (s : State β ι τ μ) : Except String Unit :=
  do
    let start   := e.timeInterval.fst
    let finish  := e.timeInterval.snd
    let minimum := s.minTime
    unless (start <= finish)
      do throw "Start of validity interval follows its finish."
    unless (minimum <= finish)
      do throw "Finish of validity interval preceeds minimum time."


def operate (e : Environment τ) (s : State β ι τ μ) (is : σ (Input β ι τ)) (c : Contract β ι τ) : Except String OperationResult β ι τ μ σ :=
  do
    ensureValidTime e s
    match c with
      | Close            => pure
                              $ {noInputsApplied e {s with accounts := Map.empty} is default with payments := makePayments (accounts s)}
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


def execute (e : Environment τ) (s : State β ι τ μ) (is : σ (Input β ι τ)) (c: Contract β ι τ) : Nat -> Except String (List OperationResult β ι τ μ σ)
  | 0 => pure []
  | n + 1 => do
               let result <- operate e s is c
               if result.newContract == Close && result.newState.accounts.toList == []
                 then pure [result]
                 else do
                        let remainder <- execute e result.newState result.inputsRemaining result.newContract n
                        pure $ result :: remainder


end Marlowe.Semantics
