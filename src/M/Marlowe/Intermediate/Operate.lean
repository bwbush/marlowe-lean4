

import M.Marlowe.Intermediate.Act
import M.Marlowe.Intermediate.AST
import M.Marlowe.Intermediate.Evaluate
import M.Marlowe.Language


namespace Marlowe.Intermediate


open Marlowe.Language.Contract
open Marlowe.Language.Input
open Marlowe.Language.State


-- This is a workaround for constructors not being allowed as an argument to an inductive datatype's definition.
def ifThenElse (condition : Bool₂) (f : Op → Op) (g : Op → Op) (continuation : Op) : Op :=
  IfThenElse condition
    (f continuation)
    (g continuation)


-- This is a workaround for constructors not being allowed as an argument to an inductive datatype's definition.
def guard₂ (condition : Bool₂) (f : Op → Op) (continuation : Op) : Op :=
  Guard condition
    (f continuation)
    continuation

def guard₁ : Bool₁ → (Op → Op) → Op → Op :=
  guard₂ ∘ Observe


def throw (s : String) (_ : Op) : Op :=
  Fail s
  

def demerkleize : Input → CaseT → (Contract → InputContent → Action → Op → Op) → Op → Op
  | NormalInput input                  , Case action contract       , f => f contract input action
  | MerkleizedInput input hash contract, MerkleizedCase action hash', f => guard₂ (ConstantBytes hash ==ₓ ConstantBytes hash')
                                                                             $ f contract input action
  | _                                  , _                          , _ => throw "Merkleization mismatch."


def inRanges (c : Int₂) : List BoundT → Bool₂
  | []                          => ConstantBool₂ false
  | Bound lower upper :: bounds => ConstantInt₂ lower ≤₂ c &&₂ c ≤₂ ConstantInt₂ upper
                                     ||₂ inRanges c bounds

def applyInput  (applied notApplied: Op → Op) (i : InputContent) (a : Action) : Op → Op :=
  match i, a with
    | IDeposit account party token amount, Deposit account' party' token' value => let amount' := evaluate₂ value
                                                                                   guard₂
                                                                                     (
                                                                                           account' ==ₐ account
                                                                                       &&₂ party'   ==ₚ party
                                                                                       &&₂ token'   ==ₜ token
                                                                                       &&₂ amount'  ==₂ ConstantInt₂ amount
                                                                                     )
                                                                                     $ act i
                                                                                     ∘ applied
    | IChoice choiceId choiceNum         , Choice choiceId' bounds              => guard₂
                                                                                     (
                                                                                       choiceId ==ₖ choiceId'
                                                                                       &&₂ inRanges (ConstantInt₂ choiceNum) bounds
                                                                                     )
                                                                                     $ act i
                                                                                     ∘ applied
    | INotify                            , Notify observation                   => guard₂
                                                                                     (observe₂ observation)
                                                                                     $ act i
                                                                                     ∘ applied
    | _                                  , _                                    => notApplied



def applyCases (input : Input) (cases : List CaseT) (f : Contract → Op → Op) : Op → Op :=
  match cases with
    | case :: cases' => demerkleize input case
                          $ fun contract =>
                              applyInput
                                (InputApplied input ∘ f contract)
                                (applyCases input cases' f)
    | []             => throw "No matching cases."


def inputApplied (input : Input) (cases : List CaseT) (t : Timeout) (c : Contract) (f : Contract → Op → Op) : Op → Op :=
  let t' := ConstantInt₂ t.getPOSIXTime
  let startBeforeTimeout  := Evaluate GetStart <₂ t'
  let finishBeforeTimeout := Evaluate GetEnd   <₂ t'
  ifThenElse startBeforeTimeout
    (Ensure finishBeforeTimeout "Ambiguous time interval." ∘ applyCases input cases f)
    (f c)


def makePayment (a : AccountId) (p : Payee) (t : TokenT) (payment : Int₂) : Op → Op :=
  let available := Evaluate $ GetMoney a t
  let remainder := available -₂ payment
  let zero := ConstantInt₂ 0
  Ensure (payment >₂ zero) "Attempt to withdraw non-positive amount."
    ∘ Ensure (remainder >₂ zero) "Attempt to withdraw amount in excess of available funds."
    ∘ SetMoney a t remainder
    ∘ MakePayment a p t payment


def ensureValidTime : Op → Op :=
  let start   := Evaluate GetStart
  let finish  := Evaluate GetEnd
  let minimum := Evaluate GetMin
  Ensure (start ≤₂ finish) "Start of validity interval follows its finish."
    ∘ Ensure (minimum ≤₂ finish) "Finish of validity interval preceeds minimum time."


def done (contract : Contract) (_ : Op) : Op :=
  Done contract


def step (inputs : List Input) (contract : Contract) (f : Contract → Op → Op) : Op → Op :=
  let f' inputs' contract' :=
    match inputs' with
      | []       => f contract'
      | inputs'' => RemainingInput inputs'' ∘ f contract'
  ensureValidTime
    ∘ match contract, inputs with
        | Close           , _                 => done Close
        | Pay a p t x c   , _                 => makePayment a p t (evaluate₂ x) ∘ f' inputs c
        | If o cThen cElse, _                 => ifThenElse (observe₂ o) (f' inputs cThen) (f' inputs cElse)
        | When cs t c     , input' :: inputs' => inputApplied input' cs t c $ f' inputs'
        | When _  _ _     , []                => f' inputs contract
        | Let v x c       , _                 => SetValue v (evaluate₂ x) ∘ f' inputs c
        | Assert o c      , _                 => TraceIf (observe₂ o) "Assertion failed." ∘ f' inputs c


def operate (inputs : List Input) (contract : Contract) (f : Contract → Op → Op) : Op → Op :=
  ensureValidTime 
    ∘ match contract, inputs with
        | Close           , []               => done Close
        | Close           , _                => throw "Extra inputs."
        | Pay a p t x c   , inputs'          => makePayment a p t (evaluate₂ x) ∘ operate inputs' c f
        | If o cThen cElse, inputs'          => ifThenElse (observe₂ o) (operate inputs' cThen f) (operate inputs' cElse f)
        | When cs t c     , input :: inputs' => inputApplied input cs t c $ fun c' => operate inputs' c' f
        | When cs t c     , []               => f $ When cs t c
        | Let v x c       , inputs'          => SetValue v (evaluate₂ x) ∘ operate inputs' c f
        | Assert o c      , inputs'          => TraceIf (observe₂ o) "Assertion failed." ∘ operate inputs' c f
  termination_by operate is c _ => (is, c)


end Marlowe.Intermediate
