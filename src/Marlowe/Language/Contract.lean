

import Marlowe.Primitives


namespace Marlowe.Language.Contract


open Marlowe.Primitives


def Timeout := POSIXTime

deriving instance Ord, Repr for Timeout


def CurrencySymbol := ByteString

deriving instance Ord, Repr for CurrencySymbol


def TokenName := ByteString

deriving instance Ord, Repr for TokenName


inductive TokenT where
| Token : CurrencySymbol → TokenName → TokenT
deriving Ord, Repr

export TokenT (Token)


inductive PartyT where
| PubKey : ByteString → PartyT
| Role   : TokenName  → PartyT
deriving Ord, Repr

export PartyT (PubKey Role)


def AccountId := PartyT

deriving instance Ord, Repr for AccountId


inductive Payee where
| Account : AccountId → Payee
| Party : PartyT → Payee
deriving Ord, Repr

export Payee (Account Party)


inductive BoundT where
| Bound : Integer → Integer → BoundT
deriving Ord, Repr

export BoundT (Bound)


inductive ChoiceIdT where
| ChoiceId : ByteString → PartyT → ChoiceIdT
deriving Ord, Repr

export ChoiceIdT (ChoiceId)


inductive ValueIdT where
| ValueId : ByteString → ValueIdT
deriving Ord, Repr

export ValueIdT (ValueId)


mutual

  inductive Value where
  | AvailableMoney : AccountId → TokenT → Value
  | Constant : Integer → Value
  | NegValue : Value → Value
  | AddValue : Value → Value → Value
  | SubValue : Value → Value → Value
  | MulValue : Value → Value → Value
  | DivValue : Value → Value → Value
  | Scale : Integer → Integer → Value → Value
  | ChoiceValue : ChoiceIdT → Value
  | TimeIntervalStart : Value
  | TimeIntervalEnd : Value
  | UseValue : ValueIdT → Value
  | Cond : Observation → Value → Value → Value
  deriving Repr

  inductive Observation where
  | AndObs : Observation → Observation → Observation
  | OrObs : Observation → Observation → Observation
  | NotObs : Observation → Observation
  | ChoseSomething : ChoiceIdT → Observation
  | ValueGE : Value → Value → Observation
  | ValueGT : Value → Value → Observation
  | ValueLT : Value → Value → Observation
  | ValueLE : Value → Value → Observation
  | ValueEQ : Value → Value → Observation
  | TrueObs : Observation
  | FalseObs : Observation
  deriving Repr

end

export Value (AvailableMoney Constant NegValue AddValue SubValue MulValue DivValue Scale ChoiceValue TimeIntervalStart TimeIntervalEnd Cond)

export Observation (AndObs OrObs NotObs ChoseSomething ValueGE ValueGT ValueLT ValueLE ValueEQ TrueObs FalseObs)


inductive Action where
| Deposit : AccountId → PartyT → TokenT → Value → Action
| Choice : ChoiceIdT → BoundT → Action
| Notify : Observation → Action
 deriving Repr

 export Action (Deposit Choice Notify)


mutual

  inductive CaseT where
  | Case : Action → Contract → CaseT
  | MerkleizedCase : Action → ByteString -> CaseT
  deriving Repr

  inductive Contract where
  | Close : Contract
  | Pay : AccountId → Payee → TokenT → Value → Contract → Contract
  | If : Observation → Contract → Contract → Contract
  | When : List CaseT → Timeout → Contract → Contract
  | Let : ValueIdT → Value → Contract → Contract
  | Assert : Observation → Contract → Contract
  deriving Repr

end

export CaseT (Case MerkleizedCase)

export Contract (Close Pay If When Let Assert)


end Marlowe.Language.Contract
